#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone)]
pub enum RiddleError {
    Parse(String, Option<Span>),
    Lowering(String, Option<Span>),
    Name(String, Option<Span>),
    Type(String, Option<Span>),
}

impl RiddleError {
    pub fn report(&self, source: &str) {
        match self {
            RiddleError::Parse(msg, span) => {
                self.print_diagnostic("Parse error", msg, *span, source)
            }
            RiddleError::Lowering(msg, span) => {
                self.print_diagnostic("Lowering error", msg, *span, source)
            }
            RiddleError::Name(msg, span) => self.print_diagnostic("Name error", msg, *span, source),
            RiddleError::Type(msg, span) => self.print_diagnostic("Type error", msg, *span, source),
        }
    }

    fn print_diagnostic(&self, kind: &str, msg: &str, span: Option<Span>, source: &str) {
        let red = "\x1b[31;1m";
        let blue = "\x1b[34;1m";
        let reset = "\x1b[0m";

        if let Some(span) = span {
            let (start_line, start_col) = self.offset_to_line_col(source, span.start);
            let (end_line, end_col) = self.offset_to_line_col(source, span.end);

            eprintln!("{red}{kind}{reset}: {msg}");
            eprintln!(
                "{blue}  -->{reset} input:{}:{}",
                start_line + 1,
                start_col + 1
            );

            let lines: Vec<&str> = source.lines().collect();
            let padding = (end_line + 1).to_string().len();

            eprintln!("{blue}{:>width$} |{reset}", "", width = padding);

            for i in start_line..=end_line {
                if i >= lines.len() {
                    break;
                }
                let line_text = lines[i];
                eprintln!(
                    "{blue}{:>width$} |{reset} {line_text}",
                    i + 1,
                    width = padding
                );

                let mut marker = String::new();
                if start_line == end_line {
                    for _ in 0..start_col {
                        marker.push(' ');
                    }
                    let len = if span.end > span.start {
                        span.end - span.start
                    } else {
                        1
                    };
                    let len = std::cmp::min(len, line_text.len().saturating_sub(start_col));
                    for _ in 0..std::cmp::max(1, len) {
                        marker.push('^');
                    }
                } else if i == start_line {
                    for _ in 0..start_col {
                        marker.push(' ');
                    }
                    for _ in start_col..line_text.len() {
                        marker.push('^');
                    }
                } else if i == end_line {
                    for _ in 0..end_col {
                        marker.push('^');
                    }
                } else {
                    for _ in 0..line_text.len() {
                        marker.push('^');
                    }
                }

                if !marker.trim().is_empty() || i == start_line {
                    eprintln!(
                        "{blue}{:>width$} |{reset} {red}{marker}{reset}",
                        "",
                        width = padding
                    );
                }
            }
            eprintln!("{blue}{:>width$} |{reset}", "", width = padding);
        } else {
            eprintln!("{red}{kind}{reset}: {msg}");
        }
    }

    fn offset_to_line_col(&self, source: &str, offset: usize) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;
        for (i, c) in source.char_indices() {
            if i == offset {
                return (line, col);
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        (line, col)
    }
}

impl std::fmt::Display for RiddleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RiddleError::Parse(s, _) => write!(f, "Parse error: {}", s),
            RiddleError::Lowering(s, _) => write!(f, "Lowering error: {}", s),
            RiddleError::Name(s, _) => write!(f, "Name error: {}", s),
            RiddleError::Type(s, _) => write!(f, "Type error: {}", s),
        }
    }
}

impl std::error::Error for RiddleError {}

pub type Result<T> = std::result::Result<T, RiddleError>;
