use std::{error::Error, fmt::Display};

use colored::Colorize;
use xlang_core::token::Range;

use crate::const_value::Type;

pub enum ErrorLevel {
    Info,
    Warning,
    Error,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLevel::Info => f.write_str(&"info".bright_cyan().bold().to_string()),
            ErrorLevel::Warning => f.write_str(&"warning".bright_yellow().bold().to_string()),
            ErrorLevel::Error => f.write_str(&"error".bright_red().bold().to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvaluationError {
    pub kind: EvaluationErrorKind,
    pub range: Range,
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl EvaluationError {
    pub fn print(&self, file_path: &str, lines: &[&str]) {
        let padding = self.range.start.line_num.to_string().len() + 2;
        println!("{}: {}", self.kind.get_level(), self.kind);
        println!(
            "{:>padding$}{} {}:{}:{}",
            "--".bold().blue(),
            ">".bold().blue(),
            file_path,
            self.range.start.line_num + 1,
            self.range.start.position + 1
        );
        println!("{:>padding$}", "|".bold().blue());
        println!(
            "{} {} {}",
            (self.range.start.line_num + 1).to_string().bold().blue(),
            "|".bold().blue(),
            lines[self.range.start.line_num as usize]
        );
        let start = 1 + self.range.start.position as usize;
        let len = (self.range.end.position - self.range.start.position) as usize;
        println!(
            "{:>padding$}{:>start$}{:>len$} {}",
            "|".bold().blue(),
            " ",
            "^".bold().bright_red(),
            self.kind.get_note().bold().bright_red()
        );

        println!("{:>padding$}", "|".bold().blue());
        println!(
            "{:>padding$} {} {}",
            "=".bold().blue(),
            "note:".bold().bright_white(),
            self.kind.get_note()
        )
    }
}

impl Error for EvaluationError {}

#[derive(Debug, Clone)]
pub enum EvaluationErrorKind {
    TypeMismatch(Type, Type),
}

impl EvaluationErrorKind {
    pub fn get_level(&self) -> ErrorLevel {
        match self {
            EvaluationErrorKind::TypeMismatch(_, _) => ErrorLevel::Error,
        }
    }

    pub fn get_note(&self) -> String {
        match self {
            Self::TypeMismatch(found, expected) => {
                format!(
                    "expected: `{}` found: `{}`",
                    expected.to_string().bold(),
                    found.to_string().bold()
                )
            }
        }
    }
}

impl Display for EvaluationErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(_, _) => {
                f.write_str(&"type mismatch".bold().bright_white().to_string())
            }
        }
    }
}
