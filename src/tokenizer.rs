use unicode_xid::UnicodeXID;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Number(&'a str),
    Hash,
    Separator(char),
    LineBreak(&'a str),
    Whitespace(&'a str),
    Comment(&'a str),
}

#[derive(Clone)]
pub struct Tokenizer<'a>(Scanner<'a>);

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self(Scanner::new(input))
    }

    pub fn input(&self) -> &'a str {
        self.0.input
    }

    pub fn position(&self) -> usize {
        self.0.position
    }

    pub fn line_num(&self) -> usize {
        self.0.line_num
    }

    pub fn column_num(&self) -> usize {
        self.0.position - self.0.line_start
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        let token_start = self.0.position;
        let next = self.0.peek(0)?;

        let token = if next.is_xid_start() {
            self.0.advance(1);
            while matches!(self.0.peek(0), Some(c) if c.is_xid_continue()) {
                self.0.advance(1);
            }
            Token::Identifier(self.0.since(token_start))
        } else if next.is_ascii_digit()
            || (next == '.' && matches!(self.0.peek(1), Some(c) if c.is_ascii_digit()))
        {
            self.0.advance(1);
            while let Some(c) = self.0.peek(0) {
                if (c.to_ascii_lowercase() == 'e' || c.to_ascii_lowercase() == 'p')
                    && matches!(self.0.peek(1), Some('+' | '-'))
                {
                    self.0.advance(2);
                } else if c.is_ascii_alphanumeric() || c == '.' {
                    self.0.advance(1);
                } else {
                    break;
                }
            }
            Token::Number(self.0.since(token_start))
        } else if next == '#' {
            self.0.advance(1);
            Token::Hash
        } else if next == '/' && matches!(self.0.peek(1), Some('/')) {
            self.0.advance(2);

            while let Some(c) = self.0.peek(0) {
                if c == '\r' || c == '\n' {
                    break;
                } else {
                    self.0.advance(1);
                }
            }

            Token::Comment(self.0.since(token_start))
        } else if next == '/' && matches!(self.0.peek(1), Some('*')) {
            self.0.advance(2);
            let mut comment_level = 1;

            loop {
                match (self.0.peek(0), self.0.peek(1)) {
                    (Some('/'), Some('*')) => {
                        comment_level += 1;
                        self.0.advance(2);
                    }
                    (Some('*'), Some('/')) => {
                        comment_level -= 1;
                        self.0.advance(2);
                        if comment_level == 0 {
                            break;
                        }
                    }
                    (Some(_), _) => {
                        self.0.advance(1);
                    }
                    (None, _) => {
                        break;
                    }
                }
            }

            Token::Comment(self.0.since(token_start))
        } else if next == '\r' || next == '\n' {
            self.0.advance(1);
            if next == '\r' && self.0.peek(0) == Some('\n') {
                self.0.advance(1);
            }
            Token::LineBreak(self.0.since(token_start))
        } else if next.is_whitespace() {
            self.0.advance(1);
            while matches!(self.0.peek(0), Some(c) if c.is_whitespace()) {
                self.0.advance(1);
            }
            Token::Whitespace(self.0.since(token_start))
        } else {
            self.0.advance(1);
            Token::Separator(next)
        };

        Some(token)
    }

    pub fn peek(&self) -> Option<Token<'a>> {
        Tokenizer(self.0).next()
    }
}

#[derive(Copy, Clone)]
struct Scanner<'a> {
    input: &'a str,
    position: usize,

    line_num: usize,
    line_start: usize,
    prev_cr: bool,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            position: 0,
            line_num: 0,
            line_start: 0,
            prev_cr: false,
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.input[self.position..].chars().nth(offset)
    }

    fn advance(&mut self, offset: usize) {
        let start = self.position;
        for (index, c) in self.input[start..].char_indices().take(offset) {
            self.position = start + index + c.len_utf8();
            if c == '\r' || (c == '\n' && !self.prev_cr) {
                self.line_num += 1;
                self.line_start = self.position;
            }

            if c == '\r' {
                self.prev_cr = true;
            } else {
                self.prev_cr = false;
            }
        }
    }

    fn since(&self, start_position: usize) -> &'a str {
        &self.input[start_position..self.position]
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;

    fn all_tokens<'a>(s: &'a str) -> impl Iterator<Item = Token<'a>> + 'a {
        let mut tokenizer = Tokenizer::new(s);
        iter::from_fn(move || tokenizer.next())
    }

    #[test]
    fn test_number() {
        assert!(all_tokens("0.1f").eq([Token::Number("0.1f")]));
        assert!(all_tokens(".1h").eq([Token::Number(".1h")]));
        assert!(all_tokens("0+1.f").eq([
            Token::Number("0"),
            Token::Separator('+'),
            Token::Number("1.f"),
        ]));
        assert!(all_tokens("0xf.1e+4f").eq([Token::Number("0xf.1e+4f")]));
        assert!(all_tokens("0xc.1p-eh").eq([Token::Number("0xc.1p-eh")]));
        assert!(all_tokens("0fa-1.f+.4").eq([
            Token::Number("0fa"),
            Token::Separator('-'),
            Token::Number("1.f"),
            Token::Separator('+'),
            Token::Number(".4"),
        ]));
    }

    #[test]
    fn test_comments() {
        assert!(all_tokens("// foo bar\nbaz").eq([
            Token::Comment("// foo bar"),
            Token::LineBreak("\n"),
            Token::Identifier("baz"),
        ]));
        assert!(all_tokens("/***********/").eq([Token::Comment("/***********/"),]));
        assert!(all_tokens("/* /* /* foo */ bar */ baz */ baf").eq([
            Token::Comment("/* /* /* foo */ bar */ baz */"),
            Token::Whitespace(" "),
            Token::Identifier("baf"),
        ]));
    }

    #[test]
    fn test_line_break() {
        assert!(all_tokens("\n\r\n\r").eq([
            Token::LineBreak("\n"),
            Token::LineBreak("\r\n"),
            Token::LineBreak("\r"),
        ]));
    }
}
