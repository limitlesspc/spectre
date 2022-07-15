use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position<'a> {
    source: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Position<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn advance(&mut self) {
        let ch = self.source.chars().nth(self.index).unwrap_or('\0');
        self.index += 1;
        self.column += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
        }
    }
}

impl fmt::Display for Position<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
