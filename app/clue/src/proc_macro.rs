use anyhow::{Context, bail};
use ast::{self, support::AstNode};
use frontend::incremental::IncrementalParser;
use riddlec::proc_macro::{
    ProcMacroDefinition, ProcMacroDiagnostic, ProcMacroExpansion, ProcMacroExport, ProcMacroKind,
    ProcMacroProvider, ProcMacroTokenStream, ProcMacroTokenTree,
};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::io::{self, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, SyncSender};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crate::project::ProcMacroPackage;

const MAX_PROC_MACRO_BYTES: usize = 16 * 1024 * 1024;
const MAX_CACHED_EXPANSIONS: usize = 128;
const PROC_MACRO_PROTOCOL_VERSION: u32 = 1;
const PROC_MACRO_TIMEOUT: Duration = Duration::from_secs(10);
const PROC_MACRO_API: &str = concat!(
    include_str!("../../../std/std/proc_macro.rid"),
    "\n",
    include_str!("../../../std/std/syn.rid")
);

#[derive(Debug, Clone)]
pub struct HostMacroExport {
    pub macro_name: String,
    pub function_name: String,
    pub function_name_range: std::ops::Range<usize>,
    pub wrapper_name: String,
    pub kind: ProcMacroKind,
    pub helper_attributes: Vec<String>,
}

pub fn discover_exports(source: &str) -> anyhow::Result<Vec<HostMacroExport>> {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    if let Some(error) = parse.errors.first() {
        bail!("cannot parse proc-macro package: {}", error.message);
    }

    let root = ast::Root::cast(parse.syntax()).context("proc-macro source has no root")?;
    let mut exports = Vec::new();
    let mut macro_names = HashSet::new();
    for stmt in root.stmts() {
        let ast::Stmt::FuncDecl(function) = stmt else {
            continue;
        };
        let Some(function_name) = function.name().map(|name| name.text().to_string()) else {
            continue;
        };
        let function_name_range = function
            .name()
            .map(|name| {
                usize::from(name.text_range().start())..usize::from(name.text_range().end())
            })
            .unwrap();
        let declarations = ast::attrs_for_node(function.syntax())
            .into_iter()
            .filter_map(|attr| parse_export_attribute(&attr.raw_text()))
            .collect::<anyhow::Result<Vec<_>>>()?;
        if declarations.len() > 1 {
            bail!("proc-macro function `{function_name}` has more than one export attribute");
        }
        for declaration in declarations {
            validate_export_signature(&function, &function_name, declaration.kind)?;
            let macro_name = declaration.name.unwrap_or_else(|| function_name.clone());
            if !macro_names.insert(macro_name.clone()) {
                bail!("proc macro `{macro_name}` is exported more than once");
            }
            exports.push(HostMacroExport {
                wrapper_name: wrapper_name(&macro_name),
                macro_name,
                function_name: function_name.clone(),
                function_name_range: function_name_range.clone(),
                kind: declaration.kind,
                helper_attributes: declaration.helper_attributes,
            });
        }
    }
    if exports.is_empty() {
        bail!("proc-macro package exports no process macro function");
    }
    Ok(exports)
}

pub fn host_source(source: &str, exports: &[HostMacroExport]) -> String {
    let suffix = host_suffix(exports);
    let mut output = String::with_capacity(PROC_MACRO_API.len() + source.len() + suffix.len() + 1);
    output.push_str(host_prefix());
    output.push('\n');
    output.push_str(source);
    output.push_str(&suffix);
    output
}

pub const fn host_prefix() -> &'static str {
    PROC_MACRO_API
}

pub fn host_suffix(exports: &[HostMacroExport]) -> String {
    let mut output = String::with_capacity(exports.len() * 180 + 120);
    for export in exports {
        output.push_str("\n#[c_export]\nfun ");
        output.push_str(&export.wrapper_name);
        output.push_str("(input: &str, second_input: &str) {\n");
        output.push_str("    let output = ");
        output.push_str(&export.function_name);
        output.push_str("(riddle_proc_decode_token_stream(input)");
        if export.kind == ProcMacroKind::Attribute {
            output.push_str(", riddle_proc_decode_token_stream(second_input)");
        }
        output.push_str(");\n");
        output.push_str("    emit_output(&output);\n}\n");
    }
    output
}

const HOST_RUNTIME_PREAMBLE: &str = r#"#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#define RIDDLE_PROC_EXPORT __declspec(dllexport)
#else
#define RIDDLE_PROC_EXPORT __attribute__((visibility("default")))
#endif

void rgc_init(void *stack_bottom);
void riddle_proc_begin(size_t call_site_start, size_t call_site_end);
void riddle_proc_emit_diagnostic(
    uint8_t level,
    size_t start,
    size_t end,
    const uint8_t *message,
    size_t len
);
const char *riddle_proc_output_value(void);
size_t riddle_proc_output_length(void);
size_t riddle_proc_diagnostic_count(void);
const void *riddle_proc_diagnostics_value(void);

int riddle_proc_putchar(int value) {
    unsigned char byte = (unsigned char)value;
    return fwrite(&byte, 1u, 1u, stderr) == 1u ? (int)byte : EOF;
}

void riddle_proc_abort(void) {
    fflush(stderr);
    exit(EXIT_FAILURE);
}

typedef struct {
    uint8_t level;
    size_t start;
    size_t end;
    const char *message;
    size_t message_len;
} RiddleProcDiagnostic;

typedef struct {
    const char *output;
    size_t output_len;
    const RiddleProcDiagnostic *diagnostics;
    size_t diagnostic_count;
} RiddleProcResult;

"#;
const HOST_RUNTIME_DISPATCH_PREFIX: &str = r"

static void dispatch_macro(
    const char *name,
    const char *input,
    const char *second_input,
    size_t call_site_start,
    size_t call_site_end
) {
";
const HOST_RUNTIME_ENTRY: &str = r#"    else {
        static const char message[] = "unknown process macro";
        riddle_proc_emit_diagnostic(
            0u,
            call_site_start,
            call_site_end,
            (const uint8_t *)message,
            sizeof(message) - 1u
        );
    }
}

RIDDLE_PROC_EXPORT int riddle_proc_expand(
    const char *name,
    const char *input,
    const char *second_input,
    size_t call_site_start,
    size_t call_site_end,
    RiddleProcResult *result
) {
    if (!name || !input || !second_input || !result || call_site_start > call_site_end) {
        return 1;
    }

    int stack_anchor = 0;
    rgc_init(&stack_anchor);
    riddle_proc_begin(call_site_start, call_site_end);
    dispatch_macro(name, input, second_input, call_site_start, call_site_end);
    result->output = riddle_proc_output_value();
    result->output_len = riddle_proc_output_length();
    result->diagnostics = (const RiddleProcDiagnostic *)riddle_proc_diagnostics_value();
    result->diagnostic_count = riddle_proc_diagnostic_count();
    return 0;
}
"#;

const PROC_MACRO_RUNNER_C: &str = r#"#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#define RIDDLE_PROC_MAX 16777216u

typedef struct {
    uint8_t level;
    size_t start;
    size_t end;
    const char *message;
    size_t message_len;
} RiddleProcDiagnostic;

typedef struct {
    const char *output;
    size_t output_len;
    const RiddleProcDiagnostic *diagnostics;
    size_t diagnostic_count;
} RiddleProcResult;

typedef int (*RiddleProcExpand)(
    const char *, const char *, const char *, size_t, size_t, RiddleProcResult *
);

static int read_exact(void *buffer, size_t len) {
    return len == 0u || fread(buffer, 1u, len, stdin) == len;
}

static int read_request_length(uint32_t *value) {
    unsigned char bytes[4];
    size_t read = fread(bytes, 1u, sizeof(bytes), stdin);
    if (read == 0u && feof(stdin)) return 0;
    if (read != sizeof(bytes)) return -1;
    *value = (uint32_t)bytes[0]
        | ((uint32_t)bytes[1] << 8u)
        | ((uint32_t)bytes[2] << 16u)
        | ((uint32_t)bytes[3] << 24u);
    return 1;
}

static uint32_t frame_u32(const unsigned char *frame, size_t offset) {
    return (uint32_t)frame[offset]
        | ((uint32_t)frame[offset + 1u] << 8u)
        | ((uint32_t)frame[offset + 2u] << 16u)
        | ((uint32_t)frame[offset + 3u] << 24u);
}

static int write_u32(uint32_t value) {
    unsigned char bytes[4] = {
        (unsigned char)(value & 0xffu),
        (unsigned char)((value >> 8u) & 0xffu),
        (unsigned char)((value >> 16u) & 0xffu),
        (unsigned char)((value >> 24u) & 0xffu),
    };
    return fwrite(bytes, 1u, sizeof(bytes), stdout) == sizeof(bytes);
}

static int write_bytes(const void *value, size_t len) {
    return len == 0u || (value && fwrite(value, 1u, len, stdout) == len);
}

static RiddleProcExpand load_expand(const char *path) {
#if defined(_WIN32)
    HMODULE library = LoadLibraryA(path);
    if (!library) return NULL;
    RiddleProcExpand expand = (RiddleProcExpand)(void *)GetProcAddress(library, "riddle_proc_expand");
    if (!expand) expand = (RiddleProcExpand)(void *)GetProcAddress(library, "_riddle_proc_expand");
    return expand;
#else
    void *library = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!library) return NULL;
    RiddleProcExpand expand = (RiddleProcExpand)dlsym(library, "riddle_proc_expand");
    if (!expand) expand = (RiddleProcExpand)dlsym(library, "_riddle_proc_expand");
    return expand;
#endif
}

static int process_request(RiddleProcExpand expand) {
    uint32_t frame_len = 0u;
    int read_status = read_request_length(&frame_len);
    if (read_status <= 0) return read_status;
    if (frame_len < 24u || frame_len > RIDDLE_PROC_MAX) return -1;
    unsigned char *frame = (unsigned char *)malloc(frame_len);
    if (!frame || !read_exact(frame, frame_len)) { free(frame); return -1; }

    uint32_t version = frame_u32(frame, 0u);
    uint32_t name_len = frame_u32(frame, 4u);
    uint32_t input_len = frame_u32(frame, 8u);
    uint32_t second_len = frame_u32(frame, 12u);
    uint32_t call_start = frame_u32(frame, 16u);
    uint32_t call_end = frame_u32(frame, 20u);
    if (version != 1u || call_start > call_end
        || name_len > frame_len - 24u
        || input_len > frame_len - 24u - name_len
        || second_len != frame_len - 24u - name_len - input_len) {
        free(frame);
        return -1;
    }

    char *name = (char *)malloc((size_t)name_len + 1u);
    char *input = (char *)malloc((size_t)input_len + 1u);
    char *second = (char *)malloc((size_t)second_len + 1u);
    if (!name || !input || !second) {
        free(name); free(input); free(second); free(frame); return -1;
    }
    memcpy(name, frame + 24u, name_len); name[name_len] = '\0';
    memcpy(input, frame + 24u + name_len, input_len); input[input_len] = '\0';
    memcpy(second, frame + 24u + name_len + input_len, second_len); second[second_len] = '\0';
    free(frame);

    RiddleProcResult result = {0};
    int status = expand(name, input, second, call_start, call_end, &result);
    free(name); free(input); free(second);
    if (status != 0 || result.output_len > RIDDLE_PROC_MAX - 16u
        || (result.output_len && !result.output)
        || result.diagnostic_count > RIDDLE_PROC_MAX / sizeof(RiddleProcDiagnostic)
        || (result.diagnostic_count && !result.diagnostics)) return -1;

    size_t response_len = 16u + result.output_len;
    for (size_t index = 0u; index < result.diagnostic_count; ++index) {
        const RiddleProcDiagnostic *diagnostic = &result.diagnostics[index];
        if (diagnostic->start > diagnostic->end
            || diagnostic->end > UINT32_MAX
            || diagnostic->message_len > RIDDLE_PROC_MAX - response_len
            || 16u > RIDDLE_PROC_MAX - response_len - diagnostic->message_len
            || (diagnostic->message_len && !diagnostic->message)) return -1;
        response_len += 16u + diagnostic->message_len;
    }
    if (response_len > RIDDLE_PROC_MAX || result.diagnostic_count > UINT32_MAX) return -1;

    if (!write_u32((uint32_t)response_len)
        || !write_u32(1u)
        || !write_u32((uint32_t)result.output_len)
        || !write_u32((uint32_t)result.diagnostic_count)
        || !write_u32(0u)
        || !write_bytes(result.output, result.output_len)) return -1;
    for (size_t index = 0u; index < result.diagnostic_count; ++index) {
        const RiddleProcDiagnostic *diagnostic = &result.diagnostics[index];
        if (!write_u32((uint32_t)diagnostic->level)
            || !write_u32((uint32_t)diagnostic->start)
            || !write_u32((uint32_t)diagnostic->end)
            || !write_u32((uint32_t)diagnostic->message_len)
            || !write_bytes(diagnostic->message, diagnostic->message_len)) return -1;
    }
    // A macro's own `println!` is forwarded to this process's stderr
    // (`riddle_proc_putchar`). Flush it before the response leaves, so the log
    // is on the wire before the caller can tear the worker down: shutdown kills
    // the runner, and whatever the C runtime still buffered would be lost —
    // which is exactly the intermittent missing "macro log" line.
    fflush(stderr);
    return fflush(stdout) == 0 ? 1 : -1;
}

int main(int argc, char **argv) {
    if (argc != 2) return 1;
#if defined(_WIN32)
    if (_setmode(_fileno(stdin), _O_BINARY) == -1
        || _setmode(_fileno(stdout), _O_BINARY) == -1) return 1;
    HMODULE runtime = LoadLibraryA("ucrtbase.dll");
    if (runtime) {
        typedef unsigned int (__cdecl *SetAbortBehavior)(unsigned int, unsigned int);
        SetAbortBehavior set_abort_behavior =
            (SetAbortBehavior)(void *)GetProcAddress(runtime, "_set_abort_behavior");
        if (set_abort_behavior) {
            set_abort_behavior(0u, _WRITE_ABORT_MSG | _CALL_REPORTFAULT);
        }
    }
#endif
    RiddleProcExpand expand = load_expand(argv[1]);
    if (!expand) return 1;

    for (;;) {
        int status = process_request(expand);
        if (status == 0) return 0;
        if (status < 0) return 1;
    }
}
"#;

pub fn host_runtime_c(exports: &[HostMacroExport]) -> String {
    let mut output = String::from(HOST_RUNTIME_PREAMBLE);
    append_host_declarations(&mut output, exports);
    output.push_str(HOST_RUNTIME_DISPATCH_PREFIX);
    append_host_dispatch(&mut output, exports);
    output.push_str(HOST_RUNTIME_ENTRY);
    output
}

pub const fn proc_macro_runner_c() -> &'static str {
    PROC_MACRO_RUNNER_C
}

fn append_host_declarations(output: &mut String, exports: &[HostMacroExport]) {
    for export in exports {
        output.push_str("void ");
        output.push_str(&export.wrapper_name);
        output.push_str("(const char *input, const char *second_input);\n");
    }
}

fn append_host_dispatch(output: &mut String, exports: &[HostMacroExport]) {
    for (index, export) in exports.iter().enumerate() {
        if index == 0 {
            output.push_str("    if ");
        } else {
            output.push_str("    else if ");
        }
        output.push_str("(strcmp(name, \"");
        output.push_str(&c_escape(&export.macro_name));
        output.push_str("\") == 0) { ");
        output.push_str(&export.wrapper_name);
        output.push_str("(input, second_input); }\n");
    }
}

struct ExportAttribute {
    kind: ProcMacroKind,
    name: Option<String>,
    helper_attributes: Vec<String>,
}

fn parse_export_attribute(raw: &str) -> Option<anyhow::Result<ExportAttribute>> {
    match parse_export_attribute_inner(raw) {
        Ok(Some(attribute)) => Some(Ok(attribute)),
        Ok(None) => None,
        Err(error) => Some(Err(error)),
    }
}

fn parse_export_attribute_inner(raw: &str) -> anyhow::Result<Option<ExportAttribute>> {
    let tokens = ProcMacroTokenStream::from_source(raw, 0)
        .map_err(anyhow::Error::msg)
        .context("invalid process macro export attribute")?;
    let [
        ProcMacroTokenTree::Punct { value: '#', .. },
        ProcMacroTokenTree::Group {
            delimiter: riddlec::proc_macro::ProcMacroDelimiter::Bracket,
            stream,
            ..
        },
    ] = tokens.trees.as_slice()
    else {
        return Ok(None);
    };
    let Some(ProcMacroTokenTree::Ident {
        text: attribute, ..
    }) = stream.trees.first()
    else {
        return Ok(None);
    };
    match attribute.as_str() {
        "proc_macro" | "proc_macro_attribute" => {
            if stream.trees.len() != 1 {
                bail!("`#[{attribute}]` does not accept arguments");
            }
            Ok(Some(ExportAttribute {
                kind: if attribute == "proc_macro" {
                    ProcMacroKind::FunctionLike
                } else {
                    ProcMacroKind::Attribute
                },
                name: None,
                helper_attributes: Vec::new(),
            }))
        }
        "proc_macro_derive" => parse_derive_export(stream).map(Some),
        _ => Ok(None),
    }
}

fn parse_derive_export(stream: &ProcMacroTokenStream) -> anyhow::Result<ExportAttribute> {
    let [
        ProcMacroTokenTree::Ident { .. },
        ProcMacroTokenTree::Group {
            delimiter: riddlec::proc_macro::ProcMacroDelimiter::Parenthesis,
            stream: args,
            ..
        },
    ] = stream.trees.as_slice()
    else {
        bail!("proc_macro_derive expects (Name, attributes(...))");
    };
    let Some(ProcMacroTokenTree::Ident { text: name, .. }) = args.trees.first() else {
        bail!("proc_macro_derive expects a macro name");
    };

    let mut index = 1usize;
    let mut helper_attributes = Vec::new();
    if index < args.trees.len() {
        expect_punct(&args.trees, &mut index, ',')?;
        if index < args.trees.len() {
            expect_ident(&args.trees, &mut index, "attributes")?;
            let Some(ProcMacroTokenTree::Group {
                delimiter: riddlec::proc_macro::ProcMacroDelimiter::Parenthesis,
                stream: helpers,
                ..
            }) = args.trees.get(index)
            else {
                bail!("attributes expects a parenthesized identifier list");
            };
            index += 1;
            helper_attributes = parse_ident_list(&helpers.trees)?;
            if index < args.trees.len() {
                expect_punct(&args.trees, &mut index, ',')?;
            }
        }
    }
    if index != args.trees.len() {
        bail!("unexpected tokens in proc_macro_derive");
    }
    Ok(ExportAttribute {
        kind: ProcMacroKind::Derive,
        name: Some(name.clone()),
        helper_attributes,
    })
}

fn parse_ident_list(trees: &[ProcMacroTokenTree]) -> anyhow::Result<Vec<String>> {
    let mut output = Vec::new();
    let mut seen = HashSet::new();
    let mut index = 0usize;
    while index < trees.len() {
        let name = match trees.get(index) {
            Some(ProcMacroTokenTree::Ident { text, .. }) => text.clone(),
            _ => bail!("expected an identifier in attributes(...)"),
        };
        if !seen.insert(name.clone()) {
            bail!("derive helper attribute `{name}` is declared more than once");
        }
        output.push(name);
        index += 1;
        if index < trees.len() {
            expect_punct(trees, &mut index, ',')?;
        }
    }
    Ok(output)
}

fn expect_ident(
    trees: &[ProcMacroTokenTree],
    index: &mut usize,
    expected: &str,
) -> anyhow::Result<()> {
    match trees.get(*index) {
        Some(ProcMacroTokenTree::Ident { text, .. }) if text == expected => {
            *index += 1;
            Ok(())
        }
        _ => bail!("expected `{expected}` in process macro attribute"),
    }
}

fn expect_punct(
    trees: &[ProcMacroTokenTree],
    index: &mut usize,
    expected: char,
) -> anyhow::Result<()> {
    match trees.get(*index) {
        Some(ProcMacroTokenTree::Punct { value, .. }) if *value == expected => {
            *index += 1;
            Ok(())
        }
        _ => bail!("expected `{expected}` in process macro attribute"),
    }
}

fn validate_export_signature(
    function: &ast::FuncDecl,
    name: &str,
    kind: ProcMacroKind,
) -> anyhow::Result<()> {
    if !function.is_pub() {
        bail!("proc-macro function `{name}` must be public");
    }
    if function.generic_params().is_some() {
        bail!("proc-macro function `{name}` cannot be generic");
    }
    let params = function
        .param_list()
        .map(|params| params.params().collect::<Vec<_>>())
        .unwrap_or_default();
    let expected = if kind == ProcMacroKind::Attribute {
        2
    } else {
        1
    };
    if params.len() != expected
        || !params
            .iter()
            .all(|param| param.ty().as_ref().is_some_and(is_token_stream_type))
    {
        bail!(
            "proc-macro function `{name}` must take exactly {expected} `TokenStream` parameter(s)"
        );
    }
    if !function
        .return_type()
        .as_ref()
        .is_some_and(is_token_stream_type)
    {
        bail!("proc-macro function `{name}` must return `TokenStream`");
    }
    Ok(())
}

fn is_token_stream_type(ty: &ast::Type) -> bool {
    let text = ty
        .syntax()
        .text()
        .to_string()
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect::<String>();
    text == "TokenStream" || text.ends_with("::TokenStream")
}

fn wrapper_name(macro_name: &str) -> String {
    let mut encoded = String::with_capacity(macro_name.len() * 2);
    for byte in macro_name.bytes() {
        write!(encoded, "{byte:02x}").expect("writing to a String should not fail");
    }
    format!("__riddle_proc_macro_{encoded}")
}

fn c_escape(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            _ => escaped.push(ch),
        }
    }
    escaped
}

pub struct ProcMacroLibrary {
    library: PathBuf,
    runner: PathBuf,
    worker: Option<ProcMacroWorker>,
}

impl ProcMacroLibrary {
    pub(crate) fn load(library: &Path, runner: &Path) -> anyhow::Result<Self> {
        if !library.is_file() {
            bail!("proc-macro library `{}` does not exist", library.display());
        }
        if !runner.is_file() {
            bail!("proc-macro runner `{}` does not exist", runner.display());
        }
        Ok(Self {
            library: library.to_owned(),
            runner: runner.to_owned(),
            worker: None,
        })
    }

    pub(crate) fn expand(
        &mut self,
        macro_name: &str,
        input: &ProcMacroTokenStream,
        second_input: Option<&ProcMacroTokenStream>,
        call_site: std::ops::Range<usize>,
    ) -> anyhow::Result<ProcMacroExpansion> {
        let input = input.encode();
        let second_input = second_input.cloned().unwrap_or_default().encode();
        if macro_name.as_bytes().contains(&0)
            || input.as_bytes().contains(&0)
            || second_input.as_bytes().contains(&0)
        {
            bail!("proc-macro request contains NUL");
        }
        let content_len = macro_name
            .len()
            .checked_add(input.len())
            .and_then(|len| len.checked_add(second_input.len()))
            .context("proc-macro request is too large")?;
        let payload_len = 24usize
            .checked_add(content_len)
            .context("proc-macro request is too large")?;
        if payload_len > MAX_PROC_MACRO_BYTES {
            bail!("proc-macro request exceeds the size limit");
        }
        let name_len = u32::try_from(macro_name.len()).context("proc-macro name is too large")?;
        let input_len = u32::try_from(input.len()).context("proc-macro input is too large")?;
        let second_len =
            u32::try_from(second_input.len()).context("second proc-macro input is too large")?;
        let call_start = u32::try_from(call_site.start)
            .context("proc-macro call-site start exceeds the source range")?;
        let call_end = u32::try_from(call_site.end)
            .context("proc-macro call-site end exceeds the source range")?;
        if call_start > call_end {
            bail!("invalid proc-macro call-site span");
        }

        let mut request = Vec::with_capacity(payload_len + 4);
        request.extend_from_slice(&(payload_len as u32).to_le_bytes());
        request.extend_from_slice(&PROC_MACRO_PROTOCOL_VERSION.to_le_bytes());
        request.extend_from_slice(&name_len.to_le_bytes());
        request.extend_from_slice(&input_len.to_le_bytes());
        request.extend_from_slice(&second_len.to_le_bytes());
        request.extend_from_slice(&call_start.to_le_bytes());
        request.extend_from_slice(&call_end.to_le_bytes());
        request.extend_from_slice(macro_name.as_bytes());
        request.extend_from_slice(input.as_bytes());
        request.extend_from_slice(second_input.as_bytes());
        let response = self.run(macro_name, request)?;
        parse_proc_macro_response(&response)
    }

    fn run(&mut self, macro_name: &str, request: Vec<u8>) -> anyhow::Result<Vec<u8>> {
        if self.worker.is_none() {
            self.worker = Some(ProcMacroWorker::spawn(&self.runner, &self.library)?);
        }
        let result = self
            .worker
            .as_mut()
            .expect("worker was initialized")
            .request(macro_name, request);
        if result.is_err() {
            self.worker = None;
        }
        result
    }
}

struct WorkerRequest {
    frame: Vec<u8>,
    response: SyncSender<io::Result<Vec<u8>>>,
}

struct ProcMacroWorker {
    child: Child,
    requests: Option<SyncSender<WorkerRequest>>,
    io_thread: Option<JoinHandle<()>>,
}

impl ProcMacroWorker {
    fn spawn(runner: &Path, library: &Path) -> anyhow::Result<Self> {
        let mut child = Command::new(runner)
            .arg(library)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .with_context(|| format!("failed to start proc-macro runner `{}`", runner.display()))?;
        let stdin = child.stdin.take().expect("piped stdin must be available");
        let stdout = child.stdout.take().expect("piped stdout must be available");
        let (requests, receiver) = mpsc::sync_channel(1);
        let io_thread = match thread::Builder::new()
            .name("riddle-proc-macro-io".into())
            .spawn(move || serve_proc_macro_io(stdin, stdout, receiver))
        {
            Ok(thread) => thread,
            Err(error) => {
                let _ = child.kill();
                let _ = child.wait();
                return Err(error).context("failed to start proc-macro I/O thread");
            }
        };
        Ok(Self {
            child,
            requests: Some(requests),
            io_thread: Some(io_thread),
        })
    }

    fn request(&mut self, macro_name: &str, frame: Vec<u8>) -> anyhow::Result<Vec<u8>> {
        let (response, receiver) = mpsc::sync_channel(1);
        let request = WorkerRequest { frame, response };
        if self
            .requests
            .as_ref()
            .context("proc-macro worker is unavailable")?
            .send(request)
            .is_err()
        {
            return Err(self.failure(macro_name, "request channel closed"));
        }
        match receiver.recv_timeout(PROC_MACRO_TIMEOUT) {
            Ok(Ok(response)) => Ok(response),
            Ok(Err(error)) => Err(self.failure(macro_name, &error.to_string())),
            Err(RecvTimeoutError::Disconnected) => {
                Err(self.failure(macro_name, "response channel closed"))
            }
            Err(RecvTimeoutError::Timeout) => {
                let _ = self.child.kill();
                let _ = self.child.wait();
                bail!(
                    "proc-macro `{macro_name}` exceeded the {} second timeout",
                    PROC_MACRO_TIMEOUT.as_secs()
                );
            }
        }
    }

    fn failure(&mut self, macro_name: &str, detail: &str) -> anyhow::Error {
        let status = match self.child.try_wait() {
            Ok(Some(status)) => Some(status),
            Ok(None) => {
                let _ = self.child.kill();
                self.child.wait().ok()
            }
            Err(_) => None,
        };
        if let Some(status) = status {
            anyhow::anyhow!("proc-macro `{macro_name}` process exited with {status}: {detail}")
        } else {
            anyhow::anyhow!("proc-macro `{macro_name}` worker failed: {detail}")
        }
    }
}

impl Drop for ProcMacroWorker {
    fn drop(&mut self) {
        // Closing the request channel ends the I/O thread, which drops the
        // child's stdin: the runner then returns from its read loop and exits
        // normally, flushing whatever the C runtime still holds (a macro's
        // forwarded `println!` goes to its stderr). Give it a short window to
        // do that instead of killing it outright — a kill can cut a log line
        // off mid-flight, which is what made the proc-macro tests flaky.
        self.requests.take();
        if let Some(io_thread) = self.io_thread.take() {
            let _ = io_thread.join();
        }
        let deadline = std::time::Instant::now() + std::time::Duration::from_millis(200);
        loop {
            match self.child.try_wait() {
                Ok(Some(_)) => return,
                Ok(None) if std::time::Instant::now() < deadline => {
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Ok(None) | Err(_) => break,
            }
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn serve_proc_macro_io(
    mut stdin: ChildStdin,
    stdout: ChildStdout,
    requests: Receiver<WorkerRequest>,
) {
    let mut stdout = BufReader::new(stdout);
    for request in requests {
        let result = stdin
            .write_all(&request.frame)
            .and_then(|()| stdin.flush())
            .and_then(|()| read_proc_macro_response(&mut stdout));
        let failed = result.is_err();
        let _ = request.response.send(result);
        if failed {
            break;
        }
    }
}

fn read_proc_macro_response(reader: &mut impl Read) -> io::Result<Vec<u8>> {
    let mut length = [0; 4];
    reader.read_exact(&mut length)?;
    let frame_len = u32::from_le_bytes(length) as usize;
    if !(16..=MAX_PROC_MACRO_BYTES).contains(&frame_len) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid proc-macro response frame",
        ));
    }
    let mut response = Vec::with_capacity(frame_len + 4);
    response.extend_from_slice(&length);
    response.resize(frame_len + 4, 0);
    reader.read_exact(&mut response[4..])?;
    Ok(response)
}

fn parse_proc_macro_response(bytes: &[u8]) -> anyhow::Result<ProcMacroExpansion> {
    let mut cursor = 0usize;
    let frame_len = take_u32(bytes, &mut cursor)? as usize;
    if !(16..=MAX_PROC_MACRO_BYTES).contains(&frame_len) || frame_len + 4 != bytes.len() {
        bail!("invalid proc-macro response frame");
    }
    let version = take_u32(bytes, &mut cursor)?;
    if version != PROC_MACRO_PROTOCOL_VERSION {
        bail!("unsupported proc-macro response version `{version}`");
    }
    let output_len = take_u32(bytes, &mut cursor)? as usize;
    let diagnostic_count = take_u32(bytes, &mut cursor)? as usize;
    if take_u32(bytes, &mut cursor)? != 0 {
        bail!("proc-macro response has an invalid status");
    }
    let encoded_output = take_text(bytes, &mut cursor, output_len, "output")?;
    let output = if encoded_output.is_empty() {
        ProcMacroTokenStream::default()
    } else {
        ProcMacroTokenStream::decode(&encoded_output)
            .map_err(anyhow::Error::msg)
            .context("invalid structured proc-macro output")?
    };
    if diagnostic_count > (bytes.len().saturating_sub(cursor)) / 16 {
        bail!("proc-macro response has too many diagnostics");
    }
    let mut diagnostics = Vec::with_capacity(diagnostic_count);
    for _ in 0..diagnostic_count {
        let level = take_u32(bytes, &mut cursor)?;
        let start = take_u32(bytes, &mut cursor)? as usize;
        let end = take_u32(bytes, &mut cursor)? as usize;
        let message_len = take_u32(bytes, &mut cursor)? as usize;
        if start > end {
            bail!("invalid proc-macro diagnostic span");
        }
        diagnostics.push(ProcMacroDiagnostic {
            severity: match level {
                1 => type_checker::Severity::Warning,
                2 => type_checker::Severity::Note,
                3 => type_checker::Severity::Help,
                _ => type_checker::Severity::Error,
            },
            message: take_text(bytes, &mut cursor, message_len, "diagnostic")?,
            span: start..end,
        });
    }
    if cursor != bytes.len() {
        bail!("proc-macro response has trailing bytes");
    }
    Ok(ProcMacroExpansion {
        output,
        diagnostics,
    })
}

fn take_u32(bytes: &[u8], cursor: &mut usize) -> anyhow::Result<u32> {
    let end = cursor
        .checked_add(4)
        .context("proc-macro response is too large")?;
    let value = bytes
        .get(*cursor..end)
        .context("truncated proc-macro response")?
        .try_into()
        .expect("four-byte slice");
    *cursor = end;
    Ok(u32::from_le_bytes(value))
}

fn take_text(bytes: &[u8], cursor: &mut usize, len: usize, kind: &str) -> anyhow::Result<String> {
    let end = cursor
        .checked_add(len)
        .context("proc-macro response is too large")?;
    let value = bytes
        .get(*cursor..end)
        .with_context(|| format!("truncated proc-macro {kind}"))?;
    *cursor = end;
    String::from_utf8(value.to_vec()).with_context(|| format!("proc-macro {kind} is not UTF-8"))
}

pub struct ClueProcMacroProvider {
    libraries: HashMap<String, ProcMacroLibrary>,
    exports: HashMap<String, Vec<ProcMacroExport>>,
    definitions: HashMap<(String, String), ProcMacroDefinition>,
    expansions: HashMap<ExpansionCacheKey, ProcMacroExpansion>,
}

#[derive(PartialEq, Eq, Hash)]
struct ExpansionCacheKey {
    package: String,
    macro_name: String,
    kind: ProcMacroKind,
    input: String,
    second_input: String,
    call_start: usize,
    call_end: usize,
}

impl ClueProcMacroProvider {
    pub(crate) fn build(packages: &[ProcMacroPackage]) -> anyhow::Result<Self> {
        let mut provider = Self {
            libraries: HashMap::new(),
            exports: HashMap::new(),
            definitions: HashMap::new(),
            expansions: HashMap::new(),
        };
        for package in packages {
            let exports = discover_exports(&package.source.source)?;
            let expansion =
                riddlec::proc_macro::expand_source(&package.source.source, &mut provider);
            let errors = expansion
                .diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.severity == type_checker::Severity::Error)
                .map(|diagnostic| diagnostic.message.as_str())
                .collect::<Vec<_>>();
            if !errors.is_empty() {
                bail!(
                    "failed to expand proc-macro package `{}`: {}",
                    package.name,
                    errors.join("; ")
                );
            }
            let artifacts =
                crate::build::build_proc_macro_library(package, &exports, &expansion.source)?;
            for export in &exports {
                let range = rowan::TextRange::new(
                    u32::try_from(export.function_name_range.start)
                        .context("proc-macro definition start exceeds the source range")?
                        .into(),
                    u32::try_from(export.function_name_range.end)
                        .context("proc-macro definition end exceeds the source range")?
                        .into(),
                );
                let mapped = package
                    .source
                    .source_map
                    .map_range(range)
                    .with_context(|| {
                        format!(
                            "cannot map proc-macro definition `{}` to its source file",
                            export.macro_name
                        )
                    })?;
                provider.definitions.insert(
                    (package.alias.clone(), export.macro_name.clone()),
                    ProcMacroDefinition {
                        path: mapped.path.to_owned(),
                        source: mapped.source.into(),
                        range: usize::from(mapped.range.start())..usize::from(mapped.range.end()),
                    },
                );
            }
            provider.exports.insert(
                package.alias.clone(),
                exports
                    .iter()
                    .map(|export| ProcMacroExport {
                        name: export.macro_name.clone(),
                        kind: export.kind,
                        helper_attributes: export.helper_attributes.clone(),
                    })
                    .collect(),
            );
            provider.libraries.insert(
                package.alias.clone(),
                ProcMacroLibrary::load(&artifacts.library, &artifacts.runner)?,
            );
        }
        Ok(provider)
    }
}

impl ProcMacroProvider for ClueProcMacroProvider {
    fn exports(&self, package: &str) -> Option<Vec<ProcMacroExport>> {
        self.exports.get(package).cloned()
    }

    fn definition(&self, package: &str, macro_name: &str) -> Option<ProcMacroDefinition> {
        self.definitions
            .get(&(package.into(), macro_name.into()))
            .cloned()
    }

    fn expand(
        &mut self,
        package: &str,
        macro_name: &str,
        kind: ProcMacroKind,
        input: &ProcMacroTokenStream,
        second_input: Option<&ProcMacroTokenStream>,
        call_site: std::ops::Range<usize>,
    ) -> Result<ProcMacroExpansion, String> {
        let Some(export) = self
            .exports
            .get(package)
            .and_then(|exports| exports.iter().find(|export| export.name == macro_name))
        else {
            return Err(format!("unknown process macro `{package}::{macro_name}`"));
        };
        if export.kind != kind {
            return Err(format!(
                "`{package}::{macro_name}` is a {:?} macro, not a {:?} macro",
                export.kind, kind
            ));
        }
        let key = ExpansionCacheKey {
            package: package.into(),
            macro_name: macro_name.into(),
            kind,
            input: input.encode(),
            second_input: second_input.map_or_else(String::new, ProcMacroTokenStream::encode),
            call_start: call_site.start,
            call_end: call_site.end,
        };
        if let Some(expansion) = self.expansions.get(&key) {
            return Ok(expansion.clone());
        }
        let library = self
            .libraries
            .get_mut(package)
            .ok_or_else(|| format!("unknown proc-macro package `{package}`"))?;
        let expansion = library
            .expand(macro_name, input, second_input, call_site)
            .map_err(|error| error.to_string())?;
        if self.expansions.len() >= MAX_CACHED_EXPANSIONS {
            self.expansions.clear();
        }
        self.expansions.insert(key, expansion.clone());
        Ok(expansion)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_proc_macro_exports() {
        let valid = r"
            #[proc_macro_derive(Answer, attributes(answer))]
            pub fun derive_answer(input: TokenStream) -> TokenStream { input }
        ";
        let exports = discover_exports(valid).unwrap();
        assert_eq!(exports.len(), 1);
        assert_eq!(exports[0].macro_name, "Answer");

        let private =
            "#[proc_macro_derive(Answer)] fun derive(input: TokenStream) -> TokenStream { input }";
        assert!(
            discover_exports(private)
                .unwrap_err()
                .to_string()
                .contains("must be public")
        );

        let duplicate = r"
            #[proc_macro_derive(Answer)]
            pub fun first(input: TokenStream) -> TokenStream { input }
            #[proc_macro_derive(Answer)]
            pub fun second(input: TokenStream) -> TokenStream { input }
        ";
        assert!(
            discover_exports(duplicate)
                .unwrap_err()
                .to_string()
                .contains("exported more than once")
        );
    }

    #[test]
    fn proc_macro_host_exports_a_cdylib_entry() {
        let runtime = host_runtime_c(&[]);
        assert!(runtime.contains("RIDDLE_PROC_EXPORT int riddle_proc_expand("));
        assert!(runtime.contains("rgc_init(&stack_anchor)"));
        assert!(runtime.contains("void riddle_proc_abort(void)"));
        assert!(!runtime.contains("_setmode"));
        assert!(!runtime.contains("fread"));
        assert!(!runtime.contains("riddle_proc_run"));
        assert!(!host_suffix(&[]).contains("fun main"));
    }
}
