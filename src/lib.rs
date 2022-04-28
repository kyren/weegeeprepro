pub mod directive_parser;
pub mod tokenizer;

use std::{
    collections::{HashMap, HashSet},
    error::Error as StdError,
    fmt,
};

use thiserror::Error;

use self::{
    directive_parser::{Directive, DirectiveParser, ParseError, TokenOrDirective},
    tokenizer::Token,
};

pub trait FileProvider {
    type Error: StdError;

    fn load_file(&mut self, path: &str) -> Result<String, Self::Error>;

    fn canonicalize_path(&self, path: &str) -> Result<String, Self::Error> {
        Ok(path.to_owned())
    }
}

impl<'a, T: FileProvider> FileProvider for &'a mut T {
    type Error = T::Error;

    fn load_file(&mut self, path: &str) -> Result<String, Self::Error> {
        T::load_file(self, path)
    }

    fn canonicalize_path(&self, path: &str) -> Result<String, Self::Error> {
        T::canonicalize_path(self, path)
    }
}

#[derive(Debug, Error)]
pub enum Error<E: StdError> {
    #[error("file '{0}' error: {1}")]
    FileError(String, E),
    #[error("error parsing preprocessor directive: {0}")]
    ParseError(ParseError),
    #[error("#endif found with no matching #if, or missing #endif for an #if")]
    MismatchedIf,
}

#[derive(Debug, Error)]
pub struct ContextError<E: StdError> {
    pub error: Error<E>,
    pub contexts: Vec<ErrorContext>,
}

impl<E: StdError> ContextError<E> {
    fn new(error: Error<E>) -> Self {
        Self {
            error,
            contexts: Vec::new(),
        }
    }

    fn new_with_context(error: Error<E>, context: ErrorContext) -> Self {
        Self {
            error,
            contexts: vec![context],
        }
    }

    fn add_context(mut self, context: ErrorContext) -> Self {
        self.contexts.push(context);
        self
    }
}

impl<E: StdError> fmt::Display for ContextError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "preprocessor error: {error}", error = self.error)?;
        for (i, ErrorContext { path, line, column }) in self.contexts.iter().enumerate() {
            writeln!(f)?;
            if i == 0 {
                write!(f, "  in file '{path}' at {line}:{column}'")?;
            } else {
                write!(f, "  included in file '{path}' at {line}:{column}'")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ErrorContext {
    pub path: String,
    pub line: usize,
    pub column: usize,
}

pub struct Preprocessor<F: FileProvider> {
    files: F,
    defines: HashMap<String, String>,
    once_paths: HashSet<String>,
    output: String,
}

impl<F: FileProvider> Preprocessor<F> {
    pub fn preprocess(files: F, path: &str) -> Result<String, ContextError<F::Error>> {
        let mut p = Self::new(files);
        p.include_file(path)?;
        Ok(p.finish())
    }

    pub fn new(files: F) -> Self {
        Self {
            files,
            defines: HashMap::new(),
            once_paths: HashSet::new(),
            output: String::new(),
        }
    }

    pub fn define(&mut self, name: &str, val: &str) {
        self.defines.insert(name.to_owned(), val.to_owned());
    }

    pub fn include_file(&mut self, path: &str) -> Result<(), ContextError<F::Error>> {
        let mut if_level = 0;
        let mut false_level = 0;

        if self.once_paths.contains(
            &self
                .files
                .canonicalize_path(path)
                .map_err(|e| ContextError::new(Error::FileError(path.to_owned(), e)))?,
        ) {
            return Ok(());
        }

        let file = self
            .files
            .load_file(path)
            .map_err(|e| ContextError::new(Error::FileError(path.to_owned(), e)))?;
        let mut parser = DirectiveParser::new(&file);

        while let Some(next) = parser.next().map_err(|e| {
            ContextError::new_with_context(Error::ParseError(e), error_context(path, &parser))
        })? {
            match next {
                TokenOrDirective::Directive(directive) => match directive {
                    Directive::Include(include_path) => {
                        if false_level == 0 {
                            if let Err(err) = self.include_file(include_path) {
                                return Err(err.add_context(error_context(path, &parser)));
                            }
                        }
                    }
                    Directive::Define(name, val) => {
                        if false_level == 0 {
                            self.defines.insert(name.to_owned(), val.to_owned());
                        }
                    }
                    Directive::Undef(name) => {
                        if false_level == 0 {
                            self.defines.remove(name);
                        }
                    }
                    Directive::IfDef(name) => {
                        if_level += 1;
                        if false_level == 0 && !self.defines.contains_key(name) {
                            false_level = if_level;
                        }
                    }
                    Directive::IfNDef(name) => {
                        if_level += 1;
                        if false_level == 0 && self.defines.contains_key(name) {
                            false_level = if_level;
                        }
                    }
                    Directive::EndIf => {
                        if if_level == 0 {
                            return Err(ContextError::new_with_context(
                                Error::MismatchedIf,
                                error_context(path, &parser),
                            ));
                        }
                        if false_level == if_level {
                            false_level = 0;
                        }
                        if_level -= 1;
                    }
                    Directive::PragmaOnce => {
                        if if_level == 0 {
                            self.once_paths
                                .insert(self.files.canonicalize_path(path).map_err(|e| {
                                    ContextError::new_with_context(
                                        Error::FileError(path.to_owned(), e),
                                        error_context(path, &parser),
                                    )
                                })?);
                        }
                    }
                },
                TokenOrDirective::Token(token) => {
                    if false_level == 0 {
                        match token {
                            t @ Token::Identifier(id) => {
                                if let Some(val) = self.defines.get(id) {
                                    write_token(Token::Identifier(val), &mut self.output);
                                } else {
                                    write_token(t, &mut self.output);
                                }
                            }
                            token => {
                                write_token(token, &mut self.output);
                            }
                        }
                    }
                }
            }
        }

        if if_level != 0 {
            Err(ContextError::new_with_context(
                Error::MismatchedIf,
                error_context(path, &parser),
            ))
        } else {
            Ok(())
        }
    }

    pub fn finish(self) -> String {
        self.output
    }
}

fn write_token(token: Token, out: &mut String) {
    match token {
        Token::Identifier(i) => out.push_str(i),
        Token::Number(n) => out.push_str(n),
        Token::Hash => out.push('#'),
        Token::Separator(s) => out.push(s),
        Token::LineBreak(lb) => out.push_str(lb),
        Token::Whitespace(ws) => out.push_str(ws),
        Token::Comment(c) => out.push_str(c),
    }
}

fn error_context(path: &str, parser: &DirectiveParser) -> ErrorContext {
    ErrorContext {
        path: path.to_owned(),
        line: parser.line_num() + 1,
        column: parser.column_num() + 1,
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, iter};

    use crate::tokenizer::Tokenizer;

    use super::*;

    struct Files(HashMap<String, String>);

    impl Files {
        fn new(files: &[(&'static str, &'static str)]) -> Self {
            let mut inner = HashMap::new();
            for &(path, content) in files {
                inner.insert(path.to_owned(), content.to_owned());
            }
            Self(inner)
        }
    }

    #[derive(Debug, Error)]
    #[error("no such file")]
    struct NoSuchFile;

    impl FileProvider for Files {
        type Error = NoSuchFile;

        fn load_file(&mut self, path: &str) -> Result<String, Self::Error> {
            Ok(self.0.get(path).ok_or(NoSuchFile)?.clone())
        }
    }

    fn token_equal(a: &str, b: &str) -> bool {
        fn non_whitespace_tokens<'a>(s: &'a str) -> impl Iterator<Item = Token<'a>> {
            let mut tokenizer = Tokenizer::new(s);
            iter::from_fn(move || loop {
                match tokenizer.next() {
                    Some(Token::Whitespace(_)) => {}
                    Some(Token::LineBreak(_)) => {}
                    Some(t) => return Some(t),
                    None => return None,
                }
            })
        }

        non_whitespace_tokens(a).eq(non_whitespace_tokens(b))
    }

    #[test]
    fn no_directives() {
        let mut files = Files::new(&[(
            "test",
            r#"
                struct Test {
                    color: vec4<f32>,
                };
            "#,
        )]);

        let output = Preprocessor::preprocess(&mut files, "test").unwrap();
        assert!(token_equal(&output, &files.load_file("test").unwrap(),));
    }

    #[test]
    fn simple_include() {
        let mut files = Files::new(&[
            (
                "a",
                r#"
                    struct A {
                        a: f32,
                    };
                "#,
            ),
            (
                "top",
                r#"
                    #include <a>

                    struct B {
                        b: f32,
                    };
                "#,
            ),
        ]);

        let output = Preprocessor::preprocess(&mut files, "top").unwrap();
        assert!(token_equal(
            &output,
            r#"
                struct A {
                    a: f32,
                };

                struct B {
                    b: f32,
                };
            "#,
        ));
    }

    #[test]
    fn simple_define() {
        let mut files = Files::new(&[(
            "top",
            r#"
                #define VAR1
                #define VAR2 a

                #ifdef VAR1
                    struct A {};
                #endif

                #ifdef VAR2
                    struct B {};
                #endif

                #ifdef VAR3
                    struct C {};
                #endif

                #ifndef VAR4
                    struct D {};
                #endif

                #undef VAR2

                #ifdef VAR2
                    struct E {};
                #endif

                #ifndef VAR2
                    struct F {};
                #endif
            "#,
        )]);

        let output = Preprocessor::preprocess(&mut files, "top").unwrap();
        assert!(token_equal(
            &output,
            r#"
                struct A {};
                struct B {};
                struct D {};
                struct F {};
            "#,
        ));
    }

    #[test]
    fn nested_ifs() {
        let mut files = Files::new(&[(
            "top",
            r#"
                #define VAR1
                #define VAR2

                #ifdef VAR1
                    struct A {};

                    #ifdef VAR2
                        struct B {};

                        #ifdef VAR3
                            struct C {};

                            #ifdef VAR1
                                struct D {};
                            #endif

                            struct E {};
                         #endif

                        struct F {};
                    #endif

                    struct G {};
                #endif
            "#,
        )]);

        let output = Preprocessor::preprocess(&mut files, "top").unwrap();
        assert!(token_equal(
            &output,
            r#"
                struct A {};
                struct B {};
                struct F {};
                struct G {};
            "#,
        ));
    }

    #[test]
    fn complex_include() {
        let mut files = Files::new(&[
            (
                "a",
                r#"
                    #pragma once
                    #define VAR
                    struct A {};
                "#,
            ),
            (
                "b",
                r#"
                    #pragma once
                    #include <a>
                    struct B {};
                "#,
            ),
            (
                "c",
                r#"
                    struct C {};
                "#,
            ),
            (
                "d",
                r#"
                    #include <a>
                    #ifdef VAR
                        #include <b>
                    #endif
                    #ifdef VAR2
                        #include <c>
                    #endif
                    struct D {};
                "#,
            ),
        ]);

        let output = Preprocessor::preprocess(&mut files, "d").unwrap();
        assert!(token_equal(
            &output,
            r#"
                struct A {};
                struct B {};
                struct D {};
            "#,
        ));
    }

    #[test]
    fn test_defines() {
        let mut files = Files::new(&[(
            "top",
            r#"
                #define STRUCT_NAME A
                struct STRUCT_NAME {};
            "#,
        )]);

        let output = Preprocessor::preprocess(&mut files, "top").unwrap();
        assert!(token_equal(
            &output,
            r#"
                struct A {};
            "#,
        ));
    }

    #[test]
    fn test_include_separation() {
        let mut files = Files::new(&[("a", "A"), ("b", "#include <a>\nB")]);

        let output = Preprocessor::preprocess(&mut files, "b").unwrap();
        assert!(token_equal(&output, "A B",));
    }

    #[test]
    fn test_external_defines() {
        let mut files = Files::new(&[(
            "top",
            r#"
                struct STRUCT_NAME {};
            "#,
        )]);

        let mut preprocessor = Preprocessor::new(&mut files);
        preprocessor.define("STRUCT_NAME", "A");
        preprocessor.include_file("top").unwrap();
        assert!(token_equal(&preprocessor.finish(), "struct A {};",));
    }
}
