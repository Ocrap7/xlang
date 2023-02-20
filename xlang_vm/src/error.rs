use std::{error::Error, fmt::Display};

use colored::Colorize;
use xlang_core::token::{Operator, Range};

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
        let len = (self.range.end.position - self.range.start.position + 1) as usize;
        let notes = self.kind.get_notes();
        if let Some(s) = notes.first() {
            println!(
                "{:>padding$}{:>start$}{:^>len$} {}",
                "|".bold().blue(),
                " ",
                "^".bold().bright_red(),
                s.bold().bright_red()
            );
        }

        println!("{:>padding$}", "|".bold().blue());
        for note in notes.iter() {
            println!(
                "{:>padding$} {} {}",
                "=".bold().blue(),
                "note:".bold().bright_white(),
                note
            )
        }
    }
}

impl Error for EvaluationError {}

#[derive(Debug, Clone)]
pub enum TypeHint {
    Variable,
    Parameter,
    ReturnParameter,
    Function,
    Record,
}

#[derive(Debug, Clone)]
pub enum EvaluationErrorKind {
    TypeMismatch(Type, Type, TypeHint),
    ArgCountMismatch(u8, u8),
    NotInitialized { hint: TypeHint },
    BinExpMismatch(Operator, Type, Type),
}

impl EvaluationErrorKind {
    pub fn get_level(&self) -> ErrorLevel {
        match self {
            EvaluationErrorKind::TypeMismatch(_, _, _) => ErrorLevel::Error,
            EvaluationErrorKind::ArgCountMismatch(_, _) => ErrorLevel::Error,
            EvaluationErrorKind::BinExpMismatch(_, _, _) => ErrorLevel::Error,

            EvaluationErrorKind::NotInitialized { .. } => ErrorLevel::Warning,
        }
    }

    pub fn get_notes(&self) -> Vec<String> {
        match self {
            Self::TypeMismatch(found, expected, ..) => {
                vec![format!(
                    "expected: `{}` found: `{}`",
                    expected.to_string().bold(),
                    found.to_string().bold()
                )]
            }
            Self::ArgCountMismatch(found, expected) => {
                vec![format!(
                    "expected: `{}` found: `{}`",
                    expected.to_string().bold(),
                    found.to_string().bold()
                )]
            }
            Self::NotInitialized {
                hint: TypeHint::Variable,
            } => {
                vec![format!("variable never initialized",)]
            }
            Self::NotInitialized {
                hint: TypeHint::ReturnParameter,
            } => {
                vec![format!("return value never initialized",)]
            }
            Self::NotInitialized {
                hint: TypeHint::Parameter,
            } => {
                vec![format!("param never initialized",)]
            }
            Self::BinExpMismatch(o, l, r) => {
                vec![format!(
                    "cannot apply operator `{}` to types `{}` and `{}`",
                    o.as_str().to_string().bold(),
                    l.to_string().bold(),
                    r.to_string().bold()
                )]
            }
            _ => vec![],
        }
    }
}

impl Display for EvaluationErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(_, _, TypeHint::ReturnParameter) => {
                f.write_str(&"return type mismatch".bold().bright_white().to_string())
            }
            Self::TypeMismatch(_, _, TypeHint::Parameter) => {
                f.write_str(&"parameter type mismatch".bold().bright_white().to_string())
            }
            Self::TypeMismatch(_, _, TypeHint::Variable) => {
                f.write_str(&"variable type mismatch".bold().bright_white().to_string())
            }
            Self::TypeMismatch(_, _, TypeHint::Function) => {
                f.write_str(&"function type mismatch".bold().bright_white().to_string())
            }
            Self::TypeMismatch(_, _, _) => {
                f.write_str(&"type mismatch".bold().bright_white().to_string())
            }
            Self::ArgCountMismatch(_, _) => {
                f.write_str(&"type mismatch".bold().bright_white().to_string())
            }
            Self::NotInitialized { .. } => {
                f.write_str(&"never initialized".bold().bright_white().to_string())
            }
            Self::BinExpMismatch { .. } => {
                f.write_str(&"operation cannot be evaluated".bold().bright_white().to_string())
            }
        }
    }
}
