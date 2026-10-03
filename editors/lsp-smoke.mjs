import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { mkdirSync, mkdtempSync, realpathSync, rmSync, statSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

const command = process.argv[2] ?? 'riddle-lsp';
const smokeRoot = realpathSync.native(mkdtempSync(join(tmpdir(), 'riddle-lsp-smoke-')));
const uri = pathToFileURL(join(smokeRoot, 'riddle-lsp-smoke.rid')).href;
const stableUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-stable.rid')).href;
const fixUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-fix.rid')).href;
const completionUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-completion.rid')).href;
const generalCompletionUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-general-completion.rid')).href;
const navigationUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-navigation.rid')).href;
const codeActionUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-code-action.rid')).href;
const linksUri = pathToFileURL(join(smokeRoot, 'links.rid')).href;
const untitledUri = 'untitled:riddle-lsp-untitled.rid';
const untitledText = 'fun target() -> i32 { 1 }\nfun main() { target(); }\n';
const navigationText = [
  'trait Show { fun show(&self) -> i32; }',
  'struct Value {}',
  'impl Show for Value { fun show(&self) -> i32 { 1 } }',
  'fun main() { let value = Value {}; value.show(); }',
  'enum Foo {',
  '    A,',
  '    B(i32),',
  '    C((i32, &Foo)),',
  '}',
  'type Alias = Foo;',
].join('\n');
const projectRoot = join(smokeRoot, 'project');
const secondWorkspaceRoot = join(smokeRoot, 'second-workspace');
const projectMainText = [
  'mod util;',
  'fun main() { util::make(); }',
  'fun imported() { mak }',
  'trait Base {}',
  'trait Child: Base {}',
  'struct Value {}',
  'impl Base for Value {}',
].join('\n');
const projectUtilText = 'pub fun make() -> i32 { 1 }\n';
mkdirSync(join(projectRoot, 'src'), { recursive: true });
writeFileSync(join(smokeRoot, 'helpers.rid'), 'pub fun support() -> i32 { 7 }\n');
writeFileSync(join(smokeRoot, 'links.rid'), 'mod helpers;\nmod gone;\nfun main() { let n = helpers::support(); }\n');
mkdirSync(secondWorkspaceRoot, { recursive: true });
writeFileSync(
  join(projectRoot, 'Clue.toml'),
  '[package]\nname = "smoke"\n\n[dependencies]\n',
);
writeFileSync(join(projectRoot, 'src', 'main.rid'), projectMainText);
const projectUtilPath = join(projectRoot, 'src', 'util.rid');
writeFileSync(projectUtilPath, projectUtilText);
const projectMainUri = pathToFileURL(join(projectRoot, 'src', 'main.rid')).href;
const projectUtilUri = pathToFileURL(join(projectRoot, 'src', 'util.rid')).href;
const server = spawn(command, [], { stdio: ['pipe', 'pipe', 'inherit'] });
let input = Buffer.alloc(0);
const messages = [];
const waiters = [];

function send(message) {
  const body = JSON.stringify(message);
  server.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
}

function dispatch(message) {
  const index = waiters.findIndex(({ predicate }) => predicate(message));
  if (index === -1) {
    messages.push(message);
    return;
  }
  const [{ resolve, timer }] = waiters.splice(index, 1);
  clearTimeout(timer);
  resolve(message);
}

function read(predicate, timeout = 15_000) {
  const index = messages.findIndex(predicate);
  if (index !== -1) {
    return Promise.resolve(messages.splice(index, 1)[0]);
  }
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error('timed out waiting for LSP message')), timeout);
    waiters.push({ predicate, resolve, timer });
  });
}

function semanticTokenTypeAt(data, targetLine, targetCharacter) {
  let line = 0;
  let character = 0;
  for (let index = 0; index < data.length; index += 5) {
    const deltaLine = data[index];
    line += deltaLine;
    character = deltaLine === 0 ? character + data[index + 1] : data[index + 1];
    if (line === targetLine && character === targetCharacter) return data[index + 3];
  }
  return undefined;
}

server.stdout.on('data', (chunk) => {
  input = Buffer.concat([input, chunk]);
  while (true) {
    const headerEnd = input.indexOf('\r\n\r\n');
    if (headerEnd === -1) return;
    const header = input.subarray(0, headerEnd).toString('ascii');
    const length = Number(/^Content-Length:\s*(\d+)$/im.exec(header)?.[1]);
    assert(Number.isInteger(length), `invalid LSP header: ${header}`);
    const bodyStart = headerEnd + 4;
    if (input.length < bodyStart + length) return;
    const body = input.subarray(bodyStart, bodyStart + length).toString('utf8');
    input = input.subarray(bodyStart + length);
    dispatch(JSON.parse(body));
  }
});

try {
  send({
    jsonrpc: '2.0',
    id: 1,
    method: 'initialize',
    params: {
      processId: null,
      rootUri: null,
      workspaceFolders: [
        { uri: pathToFileURL(projectRoot).href, name: 'project' },
        { uri: pathToFileURL(secondWorkspaceRoot).href, name: 'second-workspace' },
      ],
      capabilities: {
        textDocument: {
          completion: { completionItem: { labelDetailsSupport: true } },
          typeHierarchy: { dynamicRegistration: true },
        },
        workspace: { didChangeWatchedFiles: { dynamicRegistration: true } },
      },
    },
  });
  const initialized = await read((message) => message.id === 1);
  assert.equal(initialized.result.serverInfo.name, 'riddle-lsp');
  assert.equal(initialized.result.capabilities.positionEncoding, 'utf-16');
  assert.deepEqual(initialized.result.capabilities.textDocumentSync, {
    openClose: true,
    change: 2,
    save: true,
  });
  assert.deepEqual(initialized.result.capabilities.codeActionProvider.codeActionKinds, [
    'quickfix',
    'source.organizeImports',
    'source.addMissingImports',
    'source.fixAll',
  ]);
  assert.equal(initialized.result.capabilities.documentFormattingProvider, true);
  assert.equal(initialized.result.capabilities.documentRangeFormattingProvider, true);
  assert.equal(initialized.result.capabilities.documentHighlightProvider, true);
  assert.equal(initialized.result.capabilities.documentSymbolProvider, true);
  assert.equal(initialized.result.capabilities.workspaceSymbolProvider, true);
  assert.equal(initialized.result.capabilities.foldingRangeProvider, true);
  assert.equal(initialized.result.capabilities.hoverProvider, true);
  assert.equal(initialized.result.capabilities.declarationProvider, true);
  assert.equal(initialized.result.capabilities.definitionProvider, true);
  assert.equal(initialized.result.capabilities.typeDefinitionProvider, true);
  assert.equal(initialized.result.capabilities.implementationProvider, true);
  assert.equal(initialized.result.capabilities.callHierarchyProvider, true);
  assert.equal(initialized.result.capabilities.referencesProvider, true);
  assert.equal(initialized.result.capabilities.renameProvider.prepareProvider, true);
  const triggerCharacters = initialized.result.capabilities.completionProvider.triggerCharacters;
  assert.deepEqual(triggerCharacters, ['.', ':']);
  assert.deepEqual(initialized.result.capabilities.signatureHelpProvider.triggerCharacters, ['(', ',']);
  assert.equal(initialized.result.capabilities.inlayHintProvider, true);
  assert.equal(initialized.result.capabilities.selectionRangeProvider, true);
  assert.equal(initialized.result.capabilities.semanticTokensProvider.full.delta, true);
  assert.equal(initialized.result.capabilities.semanticTokensProvider.range, true);
  assert.equal(initialized.result.capabilities.diagnosticProvider.workspaceDiagnostics, true);
  assert.equal(initialized.result.capabilities.workspace.workspaceFolders.supported, true);
  assert(
    initialized.result.capabilities.workspace.fileOperations.willRename,
    'the server must announce willRename so a module rename can rewrite `use` paths',
  );

  send({ jsonrpc: '2.0', method: 'initialized', params: {} });
  const watcherRegistration = await read(
    (message) => message.method === 'client/registerCapability',
    3_000,
  );
  const watchedFiles = watcherRegistration.params.registrations.find(
    (registration) => registration.method === 'workspace/didChangeWatchedFiles',
  );
  assert(watchedFiles);
  assert.deepEqual(
    new Set(watchedFiles.registerOptions.watchers.map((watcher) => watcher.globPattern)),
    new Set(['**/*.rid', '**/Clue.toml', '**/Clue.lock']),
  );
  send({ jsonrpc: '2.0', id: watcherRegistration.id, result: null });
  const typeHierarchyRegistration = await read(
    (message) =>
      message.method === 'client/registerCapability' &&
      message.params.registrations.some(
        (registration) => registration.method === 'textDocument/prepareTypeHierarchy',
      ),
    3_000,
  );
  send({ jsonrpc: '2.0', id: typeHierarchyRegistration.id, result: null });
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri,
        languageId: 'riddle',
        version: 1,
        text: 'fun main() { missing; }',
      },
    },
  });
  const diagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === uri &&
      message.params.version === 1,
  );
  assert.equal(diagnostics.params.diagnostics.length, 1);
  const [unresolved] = diagnostics.params.diagnostics;
  assert.equal(unresolved.code, 'E0050');
  assert.equal(unresolved.source, 'riddle');
  assert.equal(unresolved.severity, 1);
  assert.equal(unresolved.message, 'unresolved name: `missing`');
  assert.deepEqual(unresolved.range, {
    start: { line: 0, character: 13 },
    end: { line: 0, character: 20 },
  });

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri, version: 2 },
      contentChanges: [{ text: 'fun main() {}' }],
    },
  });
  const fixed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === uri &&
      message.params.version === 2,
  );
  assert.deepEqual(fixed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: stableUri,
        languageId: 'riddle',
        version: 1,
        text: 'fun stable() { stable_missing; }',
      },
    },
  });
  const stable = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === stableUri &&
      message.params.version === 1,
  );
  assert.equal(stable.params.diagnostics[0].code, 'E0050');
  send({
    jsonrpc: '2.0',
    id: 21,
    method: 'textDocument/semanticTokens/full',
    params: { textDocument: { uri: stableUri } },
  });
  const stableTokens = await read((message) => message.id === 21);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: fixUri,
        languageId: 'riddle',
        version: 1,
        text: 'fun main() { let mut total = 0; let add = [ -> { total += 1; }]; add(); }',
      },
    },
  });
  const fixDiagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === fixUri &&
      message.params.version === 1,
  );
  const mutableClosure = fixDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === 'E0031',
  );
  assert(mutableClosure);
  assert.equal(mutableClosure.relatedInformation[0].message, 'mutable closure called here');
  send({
    jsonrpc: '2.0',
    id: 22,
    method: 'textDocument/semanticTokens/full',
    params: { textDocument: { uri: stableUri } },
  });
  const stableTokensAfterUnrelatedOpen = await read((message) => message.id === 22);
  assert.equal(stableTokensAfterUnrelatedOpen.result.resultId, stableTokens.result.resultId);

  send({
    jsonrpc: '2.0',
    id: 2,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: fixUri },
      range: mutableClosure.range,
      context: { diagnostics: [mutableClosure], only: ['quickfix'] },
    },
  });
  const codeActions = await read((message) => message.id === 2);
  assert.equal(codeActions.result.length, 1);
  assert.equal(codeActions.result[0].kind, 'quickfix');
  assert.equal(codeActions.result[0].isPreferred, true);
  assert.deepEqual(codeActions.result[0].edit.documentChanges[0], {
    textDocument: { uri: fixUri, version: 1 },
    edits: [
      {
        range: { start: mutableClosure.range.start, end: mutableClosure.range.start },
        newText: 'mut ',
      },
    ],
  });

  send({
    jsonrpc: '2.0',
    id: 20,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: fixUri },
      range: mutableClosure.range,
      context: { diagnostics: [mutableClosure], only: ['source.organizeImports'] },
    },
  });
  const filteredCodeActions = await read((message) => message.id === 20);
  assert.deepEqual(filteredCodeActions.result, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri: fixUri, version: 2 },
      contentChanges: [
        { text: 'struct Foo{}\n\nfun main(){\n    let a = Foo{};\n    let b = a;\n    let c = a;\n}' },
      ],
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === fixUri &&
      message.params.version === 2 &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === 'E0100'),
  );
  send({
    jsonrpc: '2.0',
    id: 23,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: fixUri },
      range: mutableClosure.range,
      context: { diagnostics: [mutableClosure], only: ['quickfix'] },
    },
  });
  const staleCodeActions = await read((message) => message.id === 23);
  assert.deepEqual(staleCodeActions.result, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: codeActionUri,
        languageId: 'riddle',
        version: 1,
        text: 'mod util {\n    pub fun helper() -> i32 { 1 }\n}\n\nuse zzz;\nuse aaa;\n\nfun main() {\n    let n = helper();\n}\n\ntrait Show { fun show(self) -> i32; }\nstruct Boxed {}\nimpl Show for Boxed { }\nfun show_case() {\n    let boxed = Boxed {};\n    let shown = boxed.showw();\n}\n',
      },
    },
  });
  const codeActionDiagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === codeActionUri &&
      message.params.version === 1 &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === 'E0050'),
  );
  const unresolvedHelper = codeActionDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === 'E0050',
  );
  send({
    jsonrpc: '2.0',
    id: 24,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: codeActionUri },
      range: unresolvedHelper.range,
      context: { diagnostics: [unresolvedHelper], only: ['quickfix'] },
    },
  });
  const importActions = await read((message) => message.id === 24);
  const importAction = importActions.result.find(
    (action) => action.title === 'Import `helper` from `util::helper`',
  );
  assert(importAction);
  assert.equal(importAction.kind, 'quickfix');
  assert.equal(importAction.isPreferred, false);
  assert.deepEqual(importAction.edit.documentChanges[0].edits, [
    {
      range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
      newText: 'use util::helper;\n',
    },
  ]);

  send({
    jsonrpc: '2.0',
    id: 25,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: codeActionUri },
      range: unresolvedHelper.range,
      context: { diagnostics: [unresolvedHelper] },
    },
  });
  const allActions = await read((message) => message.id === 25);
  const organizeAction = allActions.result.find(
    (action) => action.title === 'Organize imports',
  );
  assert(organizeAction);
  assert.equal(organizeAction.kind, 'source.organizeImports');
  assert.equal(organizeAction.edit.documentChanges[0].textDocument.uri, codeActionUri);
  assert.equal(organizeAction.edit.documentChanges[0].edits.length, 1);
  assert.equal(organizeAction.edit.documentChanges[0].edits[0].newText, 'use aaa;\nuse zzz;');

  const missingShow = codeActionDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === 'E0026',
  );
  const typoMethod = codeActionDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === 'E0013',
  );
  assert(missingShow);
  assert(typoMethod);
  send({
    jsonrpc: '2.0',
    id: 141,
    method: 'textDocument/codeAction',
    params: {
      textDocument: { uri: codeActionUri },
      range: missingShow.range,
      context: { diagnostics: [missingShow, typoMethod] },
    },
  });
  const implActions = await read((message) => message.id === 141);
  const implementAction = implActions.result.find(
    (action) => action.title === 'Implement `show` from `Show`',
  );
  assert(implementAction);
  assert.equal(implementAction.kind, 'quickfix');
  assert(
    implementAction.edit.documentChanges[0].edits.some((edit) =>
      edit.newText.includes('fun show(self) -> i32 {'),
    ),
  );
  const suggestAction = implActions.result.find(
    (action) => action.title === 'Did you mean `show`?',
  );
  assert(suggestAction);
  assert.equal(suggestAction.kind, 'quickfix');
  send({
    jsonrpc: '2.0',
    id: 3,
    method: 'textDocument/inlayHint',
    params: {
      textDocument: { uri: fixUri },
      range: { start: { line: 0, character: 0 }, end: { line: 6, character: 1 } },
    },
  });
  const inlayHints = await read((message) => message.id === 3);
  assert.equal(inlayHints.result.length, 2);
  assert.equal(inlayHints.result.filter((hint) => hint.label === ': Foo').length, 2);

  const lambdaUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-lambda.rid')).href;
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: lambdaUri,
        languageId: 'riddle',
        version: 1,
        text: 'fun apply(f: impl Fn(i32) -> i32) -> i32 { f(1) }\nfun main() {\n    let doubled = apply([v -> v * 2]);\n}\n',
      },
    },
  });
  send({
    jsonrpc: '2.0',
    id: 140,
    method: 'textDocument/inlayHint',
    params: {
      textDocument: { uri: lambdaUri },
      range: { start: { line: 0, character: 0 }, end: { line: 4, character: 0 } },
    },
  });
  const lambdaHints = await read((message) => message.id === 140);
  assert(
    lambdaHints.result.some(
      (hint) => hint.label === ': i32' && hint.kind === 1 && hint.position.line === 2,
    ),
  );

  send({
    jsonrpc: '2.0',
    id: 142,
    method: 'textDocument/selectionRange',
    params: {
      textDocument: { uri: lambdaUri },
      positions: [{ line: 2, character: 30 }],
    },
  });
  const selectionRanges = await read((message) => message.id === 142);
  assert(Array.isArray(selectionRanges.result) && selectionRanges.result.length === 1);
  let selection = selectionRanges.result[0];
  let depth = 1;
  while (selection.parent) {
    depth += 1;
    selection = selection.parent;
    assert(
      selection.range.start.line <= selection.range.end.line,
      'ancestor ranges must stay within the document',
    );
  }
  assert(depth >= 3, `expected a nested selection chain, got depth ${depth}`);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: linksUri,
        languageId: 'riddle',
        version: 1,
        text: 'mod helpers;\nmod gone;\nfun main() { let n = helpers::support(); }\n',
      },
    },
  });
  send({
    jsonrpc: '2.0',
    id: 144,
    method: 'textDocument/documentLink',
    params: { textDocument: { uri: linksUri } },
  });
  const linksResult = await read((message) => message.id === 144);
  assert.equal(linksResult.result.length, 1);
  assert(linksResult.result[0].target.endsWith('helpers.rid'));

  send({
    jsonrpc: '2.0',
    id: 145,
    method: 'textDocument/diagnostic',
    params: { textDocument: { uri: linksUri } },
  });
  const pullDiagnostics = await read((message) => message.id === 145);
  assert(Array.isArray(pullDiagnostics.result.items));

  const badManifestUri = pathToFileURL(join(smokeRoot, 'bad-manifest', 'Clue.toml')).href;
  mkdirSync(join(smokeRoot, 'bad-manifest'), { recursive: true });
  const badManifestText = '[package]\nname = "smoke"\nmistake = true\nversion = "0.1.0"\n';
  writeFileSync(join(smokeRoot, 'bad-manifest', 'Clue.toml'), badManifestText);
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: badManifestUri,
        languageId: 'toml',
        version: 1,
        text: badManifestText,
      },
    },
  });
  const manifestDiagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === badManifestUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === 'CLUE0003'),
  );
  assert(
    manifestDiagnostics.params.diagnostics.some((diagnostic) =>
      diagnostic.message.includes('unknown key `package.mistake`'),
    ),
  );
  send({
    jsonrpc: '2.0',
    id: 146,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: badManifestUri },
      position: { line: 3, character: 0 },
    },
  });
  const manifestCompletions = await read((message) => message.id === 146);
  assert(
    manifestCompletions.result.some((item) => item.label === 'license'),
    'expected package key completions',
  );
  send({
    jsonrpc: '2.0',
    id: 147,
    method: 'textDocument/hover',
    params: {
      textDocument: { uri: badManifestUri },
      position: { line: 1, character: 3 },
    },
  });
  const manifestHover = await read((message) => message.id === 147);
  assert(manifestHover.result);
  assert(manifestHover.result.contents.value.includes('**package.name**'));

  // A nested dependency table has a fixed key schema even though its name is
  // user-chosen, so it must offer completions.
  const nestedManifestText =
    '[package]\nname = "smoke"\n\n[dependencies.helper]\n\n';
  const nestedManifestUri = pathToFileURL(join(smokeRoot, 'nested-manifest', 'Clue.toml')).href;
  mkdirSync(join(smokeRoot, 'nested-manifest'), { recursive: true });
  writeFileSync(join(smokeRoot, 'nested-manifest', 'Clue.toml'), nestedManifestText);
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: nestedManifestUri,
        languageId: 'clue',
        version: 1,
        text: nestedManifestText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === nestedManifestUri &&
      message.params.version === 1,
  );
  send({
    jsonrpc: '2.0',
    id: 148,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: nestedManifestUri },
      position: { line: 4, character: 0 },
    },
  });
  const nestedCompletions = await read((message) => message.id === 148);
  for (const key of ['path', 'version', 'git', 'optional']) {
    assert(
      nestedCompletions.result.some((item) => item.label === key),
      `expected [dependencies.<name>] to complete \`${key}\``,
    );
  }
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: nestedManifestUri } },
  });

  const lastBurstVersion = 14;
  for (let version = 3; version <= lastBurstVersion; version += 1) {
    send({
      jsonrpc: '2.0',
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version },
        contentChanges: [{ text: `fun main() { missing_${version}; }` }],
      },
    });
  }
  const latest = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === uri &&
      message.params.version === lastBurstVersion,
  );
  assert.equal(latest.params.diagnostics[0].code, 'E0050');
  assert.equal(
    messages.some(
      (message) =>
        message.method === 'textDocument/publishDiagnostics' &&
        message.params.uri === uri &&
        message.params.version >= 3 &&
        message.params.version < lastBurstVersion,
    ),
    false,
    'stale diagnostics were published during a change burst',
  );
  assert.equal(
    messages.some(
      (message) =>
        message.method === 'textDocument/publishDiagnostics' &&
        message.params.uri === stableUri,
    ),
    false,
    'unchanged diagnostics were published again',
  );

  const completionText = 'fun main() { let c = String::new(); let d = c.i }';
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: completionUri,
        languageId: 'riddle',
        version: 1,
        text: completionText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === completionUri &&
      message.params.version === 1,
  );
  const memberCompletionStarted = performance.now();
  send({
    jsonrpc: '2.0',
    id: 4,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: completionUri },
      position: { line: 0, character: completionText.indexOf('c.i') + 3 },
    },
  });
  const completions = await read((message) => message.id === 4);
  const memberCompletionMs = performance.now() - memberCompletionStarted;
  assert.equal(completions.result.isIncomplete, true);
  assert(
    completions.result.items.some(
      (item) =>
        item.label === 'is_empty' &&
        item.labelDetails.detail === '(&self)' &&
        item.labelDetails.description === 'bool' &&
        item.insertText === 'is_empty' &&
        item.insertTextFormat === 1 &&
        item.textEdit?.newText === 'is_empty' &&
        item.textEdit?.range?.start?.character === completionText.indexOf('c.i') + 2 &&
        item.textEdit?.range?.end?.character === completionText.indexOf('c.i') + 3 &&
        item.kind === 2,
    ),
  );

  // A second request, after the diagnostics pass for this document has settled.
  // The first member completion above is dominated by whichever whole-project
  // analysis is already in flight; this measures the request itself, which is
  // what a user feels while typing once the file has been open for a moment.
  const steadyCompletionStarted = performance.now();
  send({
    jsonrpc: '2.0',
    id: 60,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: completionUri },
      position: { line: 0, character: completionText.indexOf('c.i') + 3 },
    },
  });
  const steadyCompletions = await read((message) => message.id === 60);
  const steadyCompletionMs = performance.now() - steadyCompletionStarted;
  assert(
    steadyCompletions.result.items.some((item) => item.label === 'is_empty'),
    'a repeated completion must still return the same items',
  );

  const memberDot = completionText.indexOf('c.i') + 1;
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri: completionUri, version: 2 },
      contentChanges: [
        {
          range: {
            start: { line: 0, character: memberDot },
            end: { line: 0, character: memberDot + 1 },
          },
          rangeLength: 1,
          text: '',
        },
      ],
    },
  });
  send({
    jsonrpc: '2.0',
    id: 54,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: completionUri },
      position: { line: 0, character: memberDot },
      context: { triggerKind: 3 },
    },
  });
  const clearedCompletions = await read((message) => message.id === 54);
  assert.equal(clearedCompletions.result.isIncomplete, false);
  assert.deepEqual(clearedCompletions.result.items, []);

  const generalCompletionText = 'fun Foo() {} fun main() { f }';
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: generalCompletionUri,
        languageId: 'riddle',
        version: 1,
        text: generalCompletionText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === generalCompletionUri &&
      message.params.version === 1,
  );
  const generalCompletionStarted = performance.now();
  send({
    jsonrpc: '2.0',
    id: 5,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: generalCompletionUri },
      position: { line: 0, character: generalCompletionText.lastIndexOf('f') + 1 },
    },
  });
  const generalCompletions = await read((message) => message.id === 5);
  const generalCompletionMs = performance.now() - generalCompletionStarted;
  assert(
    generalCompletions.result.some(
      (item) => item.label === 'Foo' && item.insertText === 'Foo' && item.kind === 3,
    ),
  );

  send({
    jsonrpc: '2.0',
    id: 6,
    method: 'textDocument/semanticTokens/full',
    params: { textDocument: { uri } },
  });
  const semanticTokens = await read((message) => message.id === 6);
  assert(semanticTokens.result.data.length > 0);
  assert(semanticTokens.result.resultId);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri, version: 15 },
      contentChanges: [
        {
          range: {
            start: { line: 0, character: 13 },
            end: { line: 0, character: 23 },
          },
          rangeLength: 10,
          text: 'true',
        },
      ],
    },
  });
  const incrementallyFixed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === uri &&
      message.params.version === 15,
  );
  assert.deepEqual(incrementallyFixed.params.diagnostics, []);
  send({
    jsonrpc: '2.0',
    id: 7,
    method: 'textDocument/semanticTokens/full/delta',
    params: {
      textDocument: { uri },
      previousResultId: semanticTokens.result.resultId,
    },
  });
  const semanticDelta = await read((message) => message.id === 7);
  assert(semanticDelta.result.resultId);
  assert(Array.isArray(semanticDelta.result.edits));

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: projectMainUri,
        languageId: 'riddle',
        version: 1,
        text: projectMainText,
      },
    },
  });
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: projectUtilUri,
        languageId: 'riddle',
        version: 1,
        text: projectUtilText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === projectMainUri &&
      message.params.version === 1,
  );
  const tokenTypes = initialized.result.capabilities.semanticTokensProvider.legend.tokenTypes;
  const functionTokenType = tokenTypes.indexOf('function');
  const variableTokenType = tokenTypes.indexOf('variable');
  const makeCharacter = projectMainText.split('\n')[1].indexOf('make');
  send({
    jsonrpc: '2.0',
    id: 8,
    method: 'textDocument/semanticTokens/full',
    params: { textDocument: { uri: projectMainUri } },
  });
  const projectTokensBefore = await read((message) => message.id === 8);
  assert.equal(
    semanticTokenTypeAt(projectTokensBefore.result.data, 1, makeCharacter),
    functionTokenType,
  );

  send({
    jsonrpc: '2.0',
    id: 24,
    method: 'textDocument/hover',
    params: {
      textDocument: { uri: projectMainUri },
      position: { line: 1, character: makeCharacter + 1 },
    },
  });
  const projectHover = await read((message) => message.id === 24);
  assert.match(projectHover.result.contents.value, /pub fun make\(\) -> i32/);

  send({
    jsonrpc: '2.0',
    id: 25,
    method: 'textDocument/definition',
    params: {
      textDocument: { uri: projectMainUri },
      position: { line: 1, character: makeCharacter + 1 },
    },
  });
  const projectDefinition = await read((message) => message.id === 25);
  assert.equal(projectDefinition.result.uri, projectUtilUri);
  assert.deepEqual(projectDefinition.result.range, {
    start: { line: 0, character: 8 },
    end: { line: 0, character: 12 },
  });

  send({
    jsonrpc: '2.0',
    id: 47,
    method: 'textDocument/completion',
    params: {
      textDocument: { uri: projectMainUri },
      position: { line: 2, character: projectMainText.split('\n')[2].indexOf('mak') + 3 },
    },
  });
  const autoImports = await read((message) => message.id === 47);
  const importedMake = autoImports.result.find(
    (item) => item.label === 'make' && item.labelDetails?.description === 'util::make',
  );
  assert(importedMake);
  assert.equal(importedMake.insertText, 'make');
  assert.equal(importedMake.additionalTextEdits[0].newText, 'use util::make;\n');

  send({
    jsonrpc: '2.0',
    id: 48,
    method: 'textDocument/prepareCallHierarchy',
    params: { textDocument: { uri: projectMainUri }, position: { line: 1, character: 5 } },
  });
  const preparedMainCall = await read((message) => message.id === 48);
  assert.equal(preparedMainCall.result[0].name, 'main');
  send({
    jsonrpc: '2.0',
    id: 49,
    method: 'callHierarchy/outgoingCalls',
    params: { item: preparedMainCall.result[0] },
  });
  const outgoing = await read((message) => message.id === 49);
  assert.equal(outgoing.result[0].to.name, 'make');

  send({
    jsonrpc: '2.0',
    id: 50,
    method: 'textDocument/prepareCallHierarchy',
    params: { textDocument: { uri: projectUtilUri }, position: { line: 0, character: 9 } },
  });
  const preparedMakeCall = await read((message) => message.id === 50);
  send({
    jsonrpc: '2.0',
    id: 51,
    method: 'callHierarchy/incomingCalls',
    params: { item: preparedMakeCall.result[0] },
  });
  const incoming = await read((message) => message.id === 51);
  assert.equal(incoming.result[0].from.name, 'main');

  send({
    jsonrpc: '2.0',
    id: 52,
    method: 'textDocument/prepareTypeHierarchy',
    params: { textDocument: { uri: projectMainUri }, position: { line: 3, character: 7 } },
  });
  const preparedBaseType = await read((message) => message.id === 52);
  assert.equal(preparedBaseType.result[0].name, 'Base');
  send({
    jsonrpc: '2.0',
    id: 53,
    method: 'typeHierarchy/subtypes',
    params: { item: preparedBaseType.result[0] },
  });
  const subtypes = await read((message) => message.id === 53);
  assert.deepEqual(
    subtypes.result.map((item) => item.name).sort(),
    ['Child', 'Value'],
  );

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: navigationUri,
        languageId: 'riddle',
        version: 1,
        text: navigationText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === navigationUri &&
      message.params.version === 1,
  );
  const traitCallCharacter = navigationText.split('\n')[3].indexOf('show');
  send({
    jsonrpc: '2.0',
    id: 26,
    method: 'textDocument/definition',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
    },
  });
  const traitDefinition = await read((message) => message.id === 26);
  assert.equal(traitDefinition.result.range.start.line, 0);
  assert.equal(traitDefinition.result.range.start.character, navigationText.split('\n')[0].indexOf('show'));

  send({
    jsonrpc: '2.0',
    id: 27,
    method: 'textDocument/implementation',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
    },
  });
  const traitImplementation = await read((message) => message.id === 27);
  assert.equal(traitImplementation.result.length, 1);
  assert.equal(traitImplementation.result[0].range.start.line, 2);

  send({
    jsonrpc: '2.0',
    id: 29,
    method: 'textDocument/references',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
      context: { includeDeclaration: true },
    },
  });
  const traitReferences = await read((message) => message.id === 29);
  assert.equal(traitReferences.result.length, 3);
  assert.deepEqual(
    traitReferences.result.map((location) => location.range.start.line),
    [0, 2, 3],
  );

  send({
    jsonrpc: '2.0',
    id: 30,
    method: 'textDocument/prepareRename',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
    },
  });
  const preparedRename = await read((message) => message.id === 30);
  assert.equal(preparedRename.result.placeholder, 'show');
  assert.deepEqual(preparedRename.result.range, {
    start: { line: 3, character: traitCallCharacter },
    end: { line: 3, character: traitCallCharacter + 4 },
  });

  send({
    jsonrpc: '2.0',
    id: 31,
    method: 'textDocument/rename',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
      newName: 'render',
    },
  });
  const traitRename = await read((message) => message.id === 31);
  assert.equal(traitRename.result.documentChanges.length, 1);
  assert.deepEqual(traitRename.result.documentChanges[0].textDocument, {
    uri: navigationUri,
    version: 1,
  });
  assert.equal(traitRename.result.documentChanges[0].edits.length, 3);
  assert(traitRename.result.documentChanges[0].edits.every((edit) => edit.newText === 'render'));

  send({
    jsonrpc: '2.0',
    id: 32,
    method: 'textDocument/rename',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
      newName: 'struct',
    },
  });
  const invalidRename = await read((message) => message.id === 32);
  assert.equal(invalidRename.error.code, -32602);

  // A rename the server will not perform has to say so. Returning `null` left
  // the user with an action that silently did nothing.
  send({
    jsonrpc: '2.0',
    id: 132,
    method: 'textDocument/rename',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 0, character: 0 },
      newName: 'renamed',
    },
  });
  const unnameableRename = await read((message) => message.id === 132);
  assert.equal(unnameableRename.error.code, -32602);
  assert.equal(unnameableRename.result, undefined);

  const aliasTargetCharacter = navigationText.split('\n')[9].lastIndexOf('Foo');
  send({
    jsonrpc: '2.0',
    id: 28,
    method: 'textDocument/hover',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 9, character: aliasTargetCharacter + 1 },
    },
  });
  const enumHover = await read((message) => message.id === 28);
  assert.equal(
    enumHover.result.contents.value,
    '```riddle\nenum Foo {\n    A,\n    B(i32),\n    C((i32, &Foo)),\n}\n```',
  );

  send({
    jsonrpc: '2.0',
    id: 40,
    method: 'textDocument/signatureHelp',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 5 },
    },
  });
  const signatureHelp = await read((message) => message.id === 40);
  assert.match(signatureHelp.result.signatures[0].label, /fun show\(self: Value\) -> i32/);

  send({
    jsonrpc: '2.0',
    id: 41,
    method: 'textDocument/documentHighlight',
    params: {
      textDocument: { uri: navigationUri },
      position: { line: 3, character: traitCallCharacter + 1 },
    },
  });
  const documentHighlights = await read((message) => message.id === 41);
  assert.equal(documentHighlights.result.length, 3);

  send({
    jsonrpc: '2.0',
    id: 42,
    method: 'textDocument/documentSymbol',
    params: { textDocument: { uri: navigationUri } },
  });
  const documentSymbols = await read((message) => message.id === 42);
  assert(documentSymbols.result.some((symbol) => symbol.name === 'Show'));
  assert(documentSymbols.result.some((symbol) => symbol.name === 'Value'));
  // Impl blocks used to be dropped from the outline, so their methods were
  // reachable through workspace/symbol but invisible in the file's own symbols.
  const implSymbol = documentSymbols.result.find((symbol) => symbol.name === 'Value' && symbol.detail === 'Show');
  assert(implSymbol, 'the outline must contain the impl block for `Show for Value`');
  assert(
    implSymbol.children.some((child) => child.name === 'show'),
    'an impl block must list its methods as children',
  );
  // `range` covers the whole item while `selectionRange` covers just the name.
  assert(
    implSymbol.range.start.line < implSymbol.selectionRange.start.line ||
      implSymbol.range.start.character < implSymbol.selectionRange.start.character ||
      implSymbol.range.end.line > implSymbol.selectionRange.end.line ||
      implSymbol.range.end.character > implSymbol.selectionRange.end.character,
    'an item symbol must be wider than its name',
  );

  send({
    jsonrpc: '2.0',
    id: 43,
    method: 'textDocument/foldingRange',
    params: { textDocument: { uri: navigationUri } },
  });
  const foldingRanges = await read((message) => message.id === 43);
  assert(foldingRanges.result.some((range) => range.startLine === 4 && range.endLine === 8));

  send({
    jsonrpc: '2.0',
    id: 44,
    method: 'textDocument/formatting',
    params: {
      textDocument: { uri: navigationUri },
      options: { tabSize: 4, insertSpaces: true },
    },
  });
  const formatting = await read((message) => message.id === 44);
  assert.equal(formatting.result.length, 1);
  assert.match(formatting.result[0].newText, /trait Show \{/);

  send({
    jsonrpc: '2.0',
    id: 45,
    method: 'workspace/symbol',
    params: { query: 'Foo' },
  });
  const workspaceSymbols = await read((message) => message.id === 45);
  assert(workspaceSymbols.result.some((symbol) => symbol.name === 'Foo'));

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: untitledUri,
        languageId: 'riddle',
        version: 1,
        text: untitledText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === untitledUri &&
      message.params.version === 1,
  );
  send({
    jsonrpc: '2.0',
    id: 46,
    method: 'textDocument/definition',
    params: {
      textDocument: { uri: untitledUri },
      position: { line: 1, character: 14 },
    },
  });
  const untitledDefinition = await read((message) => message.id === 46);
  assert.equal(untitledDefinition.result.uri, untitledUri);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri: projectUtilUri, version: 2 },
      contentChanges: [{ text: 'pub const make: i32 = 1;\n' }],
    },
  });
  send({
    jsonrpc: '2.0',
    id: 9,
    method: 'textDocument/semanticTokens/full',
    params: { textDocument: { uri: projectMainUri } },
  });
  const projectTokensAfter = await read((message) => message.id === 9);
  assert.equal(
    semanticTokenTypeAt(projectTokensAfter.result.data, 1, makeCharacter),
    variableTokenType,
  );
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: projectUtilUri } },
  });

  const fixedTime = new Date('2020-01-01T00:00:00.000Z');
  writeFileSync(projectUtilPath, 'pub fun make() -> i32 { missing_a }\n');
  utimesSync(projectUtilPath, fixedTime, fixedTime);
  send({
    jsonrpc: '2.0',
    method: 'workspace/didChangeWatchedFiles',
    params: { changes: [{ uri: projectUtilUri, type: 2 }] },
  });
  const firstDiskDiagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === projectUtilUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.message.includes('missing_a')),
  );
  assert.equal(firstDiskDiagnostics.params.diagnostics[0].code, 'E0050');
  const firstDiskStat = statSync(projectUtilPath);

  writeFileSync(projectUtilPath, 'pub fun make() -> i32 { missing_b }\n');
  utimesSync(projectUtilPath, fixedTime, fixedTime);
  const secondDiskStat = statSync(projectUtilPath);
  assert.equal(secondDiskStat.size, firstDiskStat.size);
  assert.equal(secondDiskStat.mtimeMs, firstDiskStat.mtimeMs);
  send({
    jsonrpc: '2.0',
    method: 'workspace/didChangeWatchedFiles',
    params: { changes: [{ uri: projectUtilUri, type: 2 }] },
  });
  const secondDiskDiagnostics = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === projectUtilUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.message.includes('missing_b')),
  );
  assert.equal(secondDiskDiagnostics.params.diagnostics[0].code, 'E0050');

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: projectMainUri } },
  });

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri } },
  });
  const closed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === uri &&
      message.params.version == null,
  );
  assert.deepEqual(closed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: stableUri } },
  });
  const stableClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === stableUri &&
      message.params.version == null,
  );
  assert.deepEqual(stableClosed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: fixUri } },
  });
  const fixClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === fixUri &&
      message.params.version == null,
  );
  assert.deepEqual(fixClosed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: completionUri } },
  });
  const completionClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === completionUri &&
      message.params.version == null,
  );
  assert.deepEqual(completionClosed.params.diagnostics, []);

  // A change the server cannot map onto its buffer must be reported, never
  // rendered as "this file is clean". The document stays open and analysable
  // afterwards, so a later valid edit restores real diagnostics.
  const desyncUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-desync.rid')).href;
  const desyncText = 'fun main() {\n    let value = 1;\n}\n';
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: {
        uri: desyncUri,
        languageId: 'riddle',
        version: 1,
        text: desyncText,
      },
    },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === desyncUri &&
      message.params.version === 1,
  );
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri: desyncUri, version: 2 },
      contentChanges: [
        {
          range: { start: { line: 99, character: 0 }, end: { line: 99, character: 1 } },
          rangeLength: 1,
          text: 'x',
        },
      ],
    },
  });
  const desynced = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === desyncUri &&
      message.params.version === 2,
  );
  assert(
    desynced.params.diagnostics.length > 0,
    'an unanalysable buffer must not be published as clean',
  );
  assert(
    desynced.params.diagnostics.some((diagnostic) => diagnostic.code === 'LSP0001'),
    'the out-of-sync document must carry the LSP0001 diagnostic',
  );
  // Recovering with a full-text change clears the state.
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didChange',
    params: {
      textDocument: { uri: desyncUri, version: 3 },
      contentChanges: [{ text: 'fun main() {}\n' }],
    },
  });
  const recovered = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === desyncUri &&
      message.params.version === 3,
  );
  assert.deepEqual(recovered.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: desyncUri } },
  });
  await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === desyncUri &&
      message.params.version == null,
  );

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: generalCompletionUri } },
  });
  const generalCompletionClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === generalCompletionUri &&
      message.params.version == null,
  );
  assert.deepEqual(generalCompletionClosed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: navigationUri } },
  });
  const navigationClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === navigationUri &&
      message.params.version == null,
  );
  assert.deepEqual(navigationClosed.params.diagnostics, []);

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: untitledUri } },
  });
  const untitledClosed = await read(
    (message) =>
      message.method === 'textDocument/publishDiagnostics' &&
      message.params.uri === untitledUri &&
      message.params.version == null,
  );
  assert.deepEqual(untitledClosed.params.diagnostics, []);

  // A buffer that is barely typed yet must still get an answer. The completion
  // path analyses the marked source and then, when the marker is not resolved
  // as a scope reference — the normal case for `f` — the unmodified source in a
  // second pass. Those two passes used to lock the same session, so the second
  // waited on the first forever and the request never returned. The editor sat
  // on it and the server stopped answering every later request too.
  const bareUri = pathToFileURL(join(smokeRoot, 'riddle-lsp-bare.rid')).href;
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: {
      textDocument: { uri: bareUri, languageId: 'riddle', version: 1, text: 'f' },
    },
  });
  const bareStarted = Date.now();
  send({
    jsonrpc: '2.0',
    id: 9,
    method: 'textDocument/completion',
    params: { textDocument: { uri: bareUri }, position: { line: 0, character: 1 } },
  });
  const bareCompletion = await read((message) => message.id === 9, 20_000);
  const bareCompletionMs = Date.now() - bareStarted;
  assert.equal(
    bareCompletion.error,
    undefined,
    `completion on a barely-typed buffer failed: ${JSON.stringify(bareCompletion.error)}`,
  );
  // Answering at all is the point; a buffer this incomplete may offer little.
  const bareItems = Array.isArray(bareCompletion.result)
    ? bareCompletion.result
    : (bareCompletion.result?.items ?? []);
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didClose',
    params: { textDocument: { uri: bareUri } },
  });

  send({ jsonrpc: '2.0', id: 10, method: 'shutdown' });
  const shutdown = await read((message) => message.id === 10);
  assert.equal(shutdown.error, undefined);
  assert.equal(shutdown.result, null);
  send({ jsonrpc: '2.0', method: 'exit' });
  console.log(
    `riddle-lsp stdio handshake passed (first member ${memberCompletionMs.toFixed(1)} ms, steady member ${steadyCompletionMs.toFixed(1)} ms, general ${generalCompletionMs.toFixed(1)} ms, bare buffer ${bareCompletionMs} ms / ${bareItems.length} items)`,
  );
} finally {
  server.stdin.end();
  const exited = await Promise.race([
    new Promise((resolve) => server.once('exit', resolve)),
    new Promise((resolve) => setTimeout(resolve, 2_000)),
  ]);
  if (exited === undefined && server.exitCode === null) server.kill();
  rmSync(smokeRoot, { recursive: true, force: true });
}
