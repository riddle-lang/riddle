use lsp_types::{Position, Range, TextDocumentContentChangeEvent};
use rowan::{TextRange, TextSize};
use std::{
    cell::RefCell,
    path::PathBuf,
    sync::atomic::{AtomicU8, Ordering},
};

pub fn normalized_path(path: PathBuf) -> PathBuf {
    std::fs::canonicalize(&path).unwrap_or_else(|_| {
        path.parent()
            .and_then(|parent| std::fs::canonicalize(parent).ok())
            .and_then(|parent| path.file_name().map(|name| parent.join(name)))
            .unwrap_or(path)
    })
}

/// The position encoding a document's columns are expressed in.
///
/// LSP lets the client choose; the server must answer in the negotiated
/// encoding for *every* position it sends or receives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEncoding {
    Utf8,
    Utf16,
    Utf32,
}

impl PositionEncoding {
    const fn from_code(code: u8) -> Self {
        match code {
            1 => Self::Utf8,
            3 => Self::Utf32,
            _ => Self::Utf16,
        }
    }

    const fn code(self) -> u8 {
        match self {
            Self::Utf8 => 1,
            Self::Utf16 => 2,
            Self::Utf32 => 3,
        }
    }

    /// Number of encoded units one scalar value occupies.
    const fn len(self, ch: char) -> usize {
        match self {
            Self::Utf8 => ch.len_utf8(),
            Self::Utf16 => ch.len_utf16(),
            Self::Utf32 => 1,
        }
    }

    #[must_use]
    pub const fn as_lsp_kind(self) -> lsp_types::PositionEncodingKind {
        match self {
            Self::Utf8 => lsp_types::PositionEncodingKind::UTF8,
            Self::Utf16 => lsp_types::PositionEncodingKind::UTF16,
            Self::Utf32 => lsp_types::PositionEncodingKind::UTF32,
        }
    }

    fn from_lsp_kind(kind: &lsp_types::PositionEncodingKind) -> Self {
        if kind.as_str() == lsp_types::PositionEncodingKind::UTF8.as_str() {
            Self::Utf8
        } else if kind.as_str() == lsp_types::PositionEncodingKind::UTF32.as_str() {
            Self::Utf32
        } else {
            Self::Utf16
        }
    }
}

// The negotiated encoding is a protocol-wide constant: the client sends it once
// during `initialize`, before any document exists, and it never changes for the
// life of the process. Storing it here keeps the ~40 pure mapping helpers that
// build a `LineIndex` from needing a threaded parameter.
static POSITION_ENCODING: AtomicU8 = AtomicU8::new(2);

pub fn set_position_encoding(encoding: PositionEncoding) {
    POSITION_ENCODING.store(encoding.code(), Ordering::SeqCst);
}

#[must_use]
pub fn position_encoding() -> PositionEncoding {
    PositionEncoding::from_code(POSITION_ENCODING.load(Ordering::SeqCst))
}

/// Picks the position encoding to serve the client.
///
/// UTF-16 wins when offered because it is the LSP default and the only encoding
/// every client must support; `None` means the client advertised a set that
/// excludes all three, which is a protocol violation the server must not paper
/// over by answering in the wrong encoding.
#[must_use]
pub fn negotiate_position_encoding(
    client_encodings: Option<&[lsp_types::PositionEncodingKind]>,
) -> Option<PositionEncoding> {
    let Some(encodings) = client_encodings else {
        // Absent, not empty: the specification defines this as UTF-16.
        return Some(PositionEncoding::Utf16);
    };
    [
        PositionEncoding::Utf16,
        PositionEncoding::Utf8,
        PositionEncoding::Utf32,
    ]
    .into_iter()
    .find(|preferred| {
        encodings
            .iter()
            .any(|kind| PositionEncoding::from_lsp_kind(kind) == *preferred)
    })
}

/// Maps byte offsets to LSP positions for one document snapshot.
pub struct LineIndex {
    /// Byte offset of the start of each line.
    starts: Vec<usize>,
    encoding: PositionEncoding,
    /// Whether every scalar in the source is one unit in every encoding.
    ascii_only: bool,
    /// `(byte offset, cumulative column)` at the start of every `char`, used to
    /// keep UTF-16 column lookups O(log n) instead of O(line length).
    char_columns: RefCell<Option<Vec<(u32, u32)>>>,
}

impl LineIndex {
    #[must_use]
    pub(crate) fn new(source: &str) -> Self {
        Self::with_source(source)
    }

    pub(crate) fn with_source(source: &str) -> Self {
        let mut starts = vec![0];
        starts.extend(
            source
                .bytes()
                .enumerate()
                .filter_map(|(offset, byte)| (byte == b'\n').then_some(offset + 1)),
        );
        Self {
            starts,
            encoding: position_encoding(),
            ascii_only: source.is_ascii(),
            char_columns: RefCell::new(None),
        }
    }

    const fn needs_table(&self) -> bool {
        match self.encoding {
            // A UTF-8 column is a byte offset, so start-of-line subtraction is
            // already exact.
            PositionEncoding::Utf8 => false,
            PositionEncoding::Utf16 | PositionEncoding::Utf32 => !self.ascii_only,
        }
    }

    fn line_of(&self, offset: usize) -> usize {
        self.starts
            .partition_point(|start| *start <= offset)
            .saturating_sub(1)
    }

    fn units(&self, source: &str, offset: usize) -> usize {
        if !self.needs_table() {
            return offset - self.starts[self.line_of(offset)];
        }
        let mut table = self.char_columns.borrow_mut();
        let table = table.get_or_insert_with(|| build_char_columns(source, self.encoding));
        let index = table
            .partition_point(|(byte, _)| usize::try_from(*byte).unwrap_or(usize::MAX) <= offset)
            .saturating_sub(1);
        let (base_offset, base_column) = table[index];
        let base_offset = usize::try_from(base_offset).unwrap_or(usize::MAX);
        usize::try_from(base_column).unwrap_or(usize::MAX)
            + source[base_offset..offset]
                .chars()
                .map(|ch| self.encoding.len(ch))
                .sum::<usize>()
    }

    /// Maps a byte offset to an LSP position.
    ///
    /// Offsets are clamped into the source and never rejected: a caller holding
    /// a slightly stale offset should still get a usable position rather than a
    /// dropped diagnostic. Offsets that split a scalar value round down.
    #[must_use]
    pub(crate) fn position(&self, source: &str, offset: usize) -> Option<Position> {
        let mut offset = offset.min(source.len());
        while offset > 0 && !source.is_char_boundary(offset) {
            offset -= 1;
        }
        Some(Position::new(
            u32::try_from(self.line_of(offset)).ok()?,
            u32::try_from(self.units(source, offset)).ok()?,
        ))
    }

    #[must_use]
    pub(crate) fn range(&self, source: &str, range: TextRange) -> Option<Range> {
        Some(Range::new(
            self.position(source, usize::from(range.start()))?,
            self.position(source, usize::from(range.end()))?,
        ))
    }
}

/// `(byte offset, cumulative column)` per `char`, at that character's start.
///
/// A newline resets the column, so each entry is the column of its own line.
fn build_char_columns(source: &str, encoding: PositionEncoding) -> Vec<(u32, u32)> {
    let mut columns = Vec::with_capacity(source.len() + 1);
    let mut column = 0usize;
    for (offset, ch) in source.char_indices() {
        columns.push((
            u32::try_from(offset).unwrap_or(u32::MAX),
            u32::try_from(column).unwrap_or(u32::MAX),
        ));
        if ch == '\n' {
            column = 0;
        } else {
            column += encoding.len(ch);
        }
    }
    columns.push((
        u32::try_from(source.len()).unwrap_or(u32::MAX),
        u32::try_from(column).unwrap_or(u32::MAX),
    ));
    columns
}

/// Applies a batch of content changes, returning `Ok` only when the batch was
/// applied in full.
///
/// The caller must treat `Err` as "the buffered text can no longer be trusted",
/// never as "the document is empty": silently dropping the document makes the
/// next publish report the file as clean.
///
/// # Errors
///
/// Returns `Err` when a change names a line the document does not have, or when
/// a range ends before it starts.
pub fn apply_content_changes(
    text: &mut String,
    changes: Vec<TextDocumentContentChangeEvent>,
) -> Result<(), ApplyError> {
    let mut staged = text.clone();
    for change in changes {
        let Some(range) = change.range else {
            staged = change.text;
            continue;
        };
        let Some(start) = offset_for_position_clamped(&staged, range.start) else {
            return Err(ApplyError::LineOutOfRange {
                line: range.start.line,
            });
        };
        let Some(end) = offset_for_position_clamped(&staged, range.end) else {
            return Err(ApplyError::LineOutOfRange {
                line: range.end.line,
            });
        };
        if start > end {
            return Err(ApplyError::InvertedRange);
        }
        staged.replace_range(start..end, &change.text);
    }
    *text = staged;
    Ok(())
}

/// Why a content-change batch could not be applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApplyError {
    /// The change names a line beyond the end of the buffered text.
    LineOutOfRange { line: u32 },
    /// The change's start offset is after its end offset.
    InvertedRange,
}

impl std::fmt::Display for ApplyError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LineOutOfRange { line } => {
                write!(
                    formatter,
                    "change names line {line}, which is past the document end"
                )
            }
            Self::InvertedRange => write!(formatter, "change range ends before it starts"),
        }
    }
}

pub fn offset_for_position(source: &str, position: Position) -> Option<usize> {
    offset_for_position_inner(source, position, false)
}

/// Maps an LSP position to a byte offset, clamping instead of failing.
///
/// A column inside a scalar value rounds down to that scalar's start, and a
/// column past the line end clamps to the line end. This mirrors what editors
/// do with their own buffers and keeps a slightly out-of-date client from
/// desynchronising the server permanently.
#[must_use]
pub fn offset_for_position_clamped(source: &str, position: Position) -> Option<usize> {
    offset_for_position_inner(source, position, true)
}

fn offset_for_position_inner(source: &str, position: Position, clamp: bool) -> Option<usize> {
    let encoding = position_encoding();
    let mut line_start = 0;
    for _ in 0..position.line {
        line_start += source[line_start..].find('\n')? + 1;
    }
    let line_end = source[line_start..]
        .find('\n')
        .map_or(source.len(), |offset| line_start + offset);
    let line = source[line_start..line_end]
        .strip_suffix('\r')
        .unwrap_or_else(|| &source[line_start..line_end]);
    let mut column = 0;
    for (byte, ch) in line.char_indices() {
        if column == position.character {
            return Some(line_start + byte);
        }
        column += u32::try_from(encoding.len(ch)).unwrap_or(u32::MAX);
        if column > position.character {
            // The requested column sits inside this scalar value.
            return clamp.then_some(line_start + byte);
        }
    }
    if column == position.character {
        return Some(line_start + line.len());
    }
    clamp.then_some(line_start + line.len())
}

pub fn is_identifier_continue(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

pub fn range_is_in_source(range: TextRange, source_len: usize) -> bool {
    usize::from(range.end()) <= source_len
}

pub fn ranges_overlap(a: TextRange, b: TextRange) -> bool {
    a.start() < b.end() && b.start() < a.end()
}

pub fn text_size(value: usize) -> TextSize {
    TextSize::from(u32::try_from(value).expect("source offset should fit in u32"))
}

pub fn text_range(start: usize, end: usize) -> TextRange {
    TextRange::new(text_size(start), text_size(end))
}
