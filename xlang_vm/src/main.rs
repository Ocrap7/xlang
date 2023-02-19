use std::{fs::File, io::Read, rc::Rc};

use colored::Colorize;
use evaluator::Evaluator;
use xlang_core::Module;
use xlang_util::format::TreeDisplay;

use crate::pass::CodePass;

mod const_value;
mod error;
mod evaluator;
mod pass;
mod scope;

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";

fn main() {
    let file_path = "test_files/test.xl";
    let mut file = File::open(file_path).unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input);
    for error in errors {
        println!("{error}")
    }
    let module = Rc::new(module);

    let code_pass = CodePass::new(module.clone());
    let scope_manager = code_pass.run();

    println!("{}", scope_manager.format());

    let evaluator = Evaluator::new(module, scope_manager);

    let values = evaluator.evaluate();

    let lines: Vec<&str> = input.split(LINE_ENDING).collect();
    for error in &evaluator.state.read().unwrap().errors {
        error.print(file_path, &lines);
    }

    for value in values {
        println!("{value}");
    }

    println!("{}", evaluator.state.read().unwrap().scope.format());
}
