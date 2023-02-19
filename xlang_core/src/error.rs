use std::{error::Error, fmt::Display};

use crate::token::Range;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub range: Range,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for ParseError {}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    InvalidSyntax(String),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidSyntax(s) => write!(f, "Invalid Syntax: {s}"),
        }
    }
}
