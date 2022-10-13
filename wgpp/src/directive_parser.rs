use thiserror::Error;

use crate::tokenizer::{Token, Tokenizer};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Directive<'a> {
    Include(&'a str),
    Define(&'a str, &'a str),
    Undef(&'a str),
    IfDef(&'a str),
    IfNDef(&'a str),
    EndIf,
    PragmaOnce,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenOrDirective<'a> {
    Directive(Directive<'a>),
    Token(Token<'a>),
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct ParseError(String);

pub struct DirectiveParser<'a> {
    tokenizer: Tokenizer<'a>,
    start_of_line: bool,
}

impl<'a> DirectiveParser<'a> {
    pub fn new(input: &'a str) -> Self {
        DirectiveParser {
            tokenizer: Tokenizer::new(input),
            start_of_line: true,
        }
    }

    pub fn line_num(&self) -> usize {
        self.tokenizer.line_num()
    }

    pub fn column_num(&self) -> usize {
        self.tokenizer.column_num()
    }

    pub fn next(&mut self) -> Result<Option<TokenOrDirective<'a>>, ParseError> {
        Ok(match self.tokenizer.next() {
            Some(Token::Hash) if self.start_of_line => {
                skip_whitespace(&mut self.tokenizer);
                let directive = match self.tokenizer.peek() {
                    Some(Token::Identifier("include")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        expect_separator(&mut self.tokenizer, '<')?;
                        let path = scan_to_separator_in_line(&mut self.tokenizer, '>')?;
                        self.tokenizer.next();
                        Directive::Include(path)
                    }
                    Some(Token::Identifier("define")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        let var = expect_some_identifier(&mut self.tokenizer)?;
                        skip_whitespace(&mut self.tokenizer);
                        let value = scan_to_eol(&mut self.tokenizer);
                        Directive::Define(var, value.trim())
                    }
                    Some(Token::Identifier("undef")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        let var = expect_some_identifier(&mut self.tokenizer)?;
                        Directive::Undef(var)
                    }
                    Some(Token::Identifier("ifdef")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        let var = expect_some_identifier(&mut self.tokenizer)?;
                        Directive::IfDef(var)
                    }
                    Some(Token::Identifier("ifndef")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        let var = expect_some_identifier(&mut self.tokenizer)?;
                        Directive::IfNDef(var)
                    }
                    Some(Token::Identifier("endif")) => {
                        self.tokenizer.next();
                        Directive::EndIf
                    }
                    Some(Token::Identifier("pragma")) => {
                        self.tokenizer.next();
                        skip_whitespace(&mut self.tokenizer);
                        expect_identifier(&mut self.tokenizer, "once")?;
                        Directive::PragmaOnce
                    }
                    Some(Token::Identifier(other)) => {
                        return Err(ParseError(format!(
                            "unknown preprocessor directive '{other}'"
                        )))
                    }
                    Some(t) => {
                        return Err(ParseError(format!(
                        "unexpected token '{t:?}' following '#', expected preprocessor directive"
                    )))
                    }
                    None => {
                        return Err(ParseError(
                            "unexpected eof following '#', expected preprocessor directive"
                                .to_string(),
                        ))
                    }
                };
                skip_whitespace(&mut self.tokenizer);
                match self.tokenizer.peek() {
                    Some(Token::LineBreak(_)) | None => {}
                    Some(t) => {
                        return Err(ParseError(format!(
                            "unexpected token following directive {t:?}"
                        )));
                    }
                }
                Some(TokenOrDirective::Directive(directive))
            }
            Some(t @ Token::LineBreak(_)) => {
                self.start_of_line = true;
                Some(TokenOrDirective::Token(t))
            }
            Some(t @ Token::Whitespace(_)) => Some(TokenOrDirective::Token(t)),
            Some(t) => {
                self.start_of_line = false;
                Some(TokenOrDirective::Token(t))
            }
            None => None,
        })
    }
}

fn expect_identifier<'a>(t: &mut Tokenizer<'a>, ident: &str) -> Result<(), ParseError> {
    match t.peek() {
        Some(Token::Identifier(i)) if i == ident => {
            t.next();
            Ok(())
        }
        Some(t) => Err(ParseError(format!(
            "unexpected token {t:?}, expected '{ident}'"
        ))),
        None => Err(ParseError(format!("unexpected eof, expected '{ident}'"))),
    }
}

fn expect_some_identifier<'a>(t: &mut Tokenizer<'a>) -> Result<&'a str, ParseError> {
    match t.peek() {
        Some(Token::Identifier(i)) => {
            t.next();
            Ok(i)
        }
        Some(t) => Err(ParseError(format!(
            "unexpected token {t:?}, expected identifier"
        ))),
        None => Err(ParseError(
            "unexpected eof, expected identifier".to_string(),
        )),
    }
}

fn expect_separator(t: &mut Tokenizer, sep: char) -> Result<(), ParseError> {
    match t.peek() {
        Some(Token::Separator(s)) if s == sep => {
            t.next();
            Ok(())
        }
        Some(t) => Err(ParseError(format!(
            "unexpected token {t:?}, expected '{sep}'"
        ))),
        None => Err(ParseError(format!("unexpected eof, expected '{sep}'"))),
    }
}

fn skip_whitespace(t: &mut Tokenizer) {
    while matches!(t.peek(), Some(Token::Whitespace(_))) {
        t.next();
    }
}

fn scan_to_separator_in_line<'a>(t: &mut Tokenizer<'a>, sep: char) -> Result<&'a str, ParseError> {
    let start = t.position();
    loop {
        match t.peek() {
            Some(Token::LineBreak(_)) | None => {
                return Err(ParseError(format!("no '{sep}' found in current line")))
            }
            Some(Token::Separator(c)) if c == sep => {
                return Ok(&t.input()[start..t.position()]);
            }
            Some(_) => {
                t.next();
            }
        }
    }
}

fn scan_to_eol<'a>(t: &mut Tokenizer<'a>) -> &'a str {
    let start = t.position();
    loop {
        match t.peek() {
            Some(Token::LineBreak(_)) | None => {
                return &t.input()[start..t.position()];
            }
            Some(_) => {
                t.next();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;

    fn non_whitespace_parts(s: &str) -> impl Iterator<Item = TokenOrDirective<'_>> + '_ {
        let mut parser = DirectiveParser::new(s);
        iter::from_fn(move || loop {
            let p = parser.next().unwrap()?;
            if !matches!(
                p,
                TokenOrDirective::Token(Token::Whitespace(_) | Token::LineBreak(_))
            ) {
                return Some(p);
            }
        })
    }

    #[test]
    fn test_parser() {
        assert!(non_whitespace_parts(
            r#"
                1
                #include <foo>
                2
            "#
        )
        .eq([
            TokenOrDirective::Token(Token::Number("1")),
            TokenOrDirective::Directive(Directive::Include("foo")),
            TokenOrDirective::Token(Token::Number("2")),
        ]));
        assert!(non_whitespace_parts(
            r#"
                #pragma once

                42

                #include <foo>
                #include <bar>
                #define BAZ 1
                #ifdef BAZ
                    #define BAF 1 2 3 4
                #endif
                #ifndef BAF
                #endif

                12

            "#
        )
        .eq([
            TokenOrDirective::Directive(Directive::PragmaOnce),
            TokenOrDirective::Token(Token::Number("42")),
            TokenOrDirective::Directive(Directive::Include("foo")),
            TokenOrDirective::Directive(Directive::Include("bar")),
            TokenOrDirective::Directive(Directive::Define("BAZ", "1")),
            TokenOrDirective::Directive(Directive::IfDef("BAZ")),
            TokenOrDirective::Directive(Directive::Define("BAF", "1 2 3 4")),
            TokenOrDirective::Directive(Directive::EndIf),
            TokenOrDirective::Directive(Directive::IfNDef("BAF")),
            TokenOrDirective::Directive(Directive::EndIf),
            TokenOrDirective::Token(Token::Number("12")),
        ]));
        assert!(
            non_whitespace_parts("  \t  #define BAF 1 2 3 4 <> \"   \t",).eq([
                TokenOrDirective::Directive(Directive::Define("BAF", "1 2 3 4 <> \"")),
            ])
        );
    }
}
