use std::{fs::File, io::Read, rc::Rc};

use evaluator::Evaluator;
use xlang_core::Module;
use xlang_util::{format::TreeDisplay, Rf};

use crate::{
    pass::CodePass,
    scope::{Scope, ScopeValue},
    stdlib::{std_module, fill_module},
};

mod const_value;
mod error;
mod evaluator;
mod pass;
mod scope;
mod stdlib;

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";

fn main() {
    let file_path = "test_files/test.xl";
    let mut file = File::open(file_path).unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input, "mymod");
    for error in errors {
        println!("{error}")
    }
    let module = Rc::new(module);

    let lines: Vec<&str> = input.split(LINE_ENDING).collect();

    let symbol_tree = Rf::new(Scope::new(ScopeValue::Root));

    {
        let std_module = std_module();

        let code_pass = CodePass::new(symbol_tree.clone(), std_module.clone());
        let code_pass_state = code_pass.run();
        let std_mod_scope = code_pass_state.scope.module.clone();

        let evaluator = Evaluator::new(std_module.clone(), code_pass_state.scope);
        let values = evaluator.evaluate();

        for error in &code_pass_state.errors {
            error.print(file_path, &lines);
        }

        for error in &evaluator.state.read().unwrap().errors {
            error.print(file_path, &lines);
        }

        for value in values {
            println!("{value}");
        }

        fill_module(std_mod_scope);
    }

    let code_pass = CodePass::new(symbol_tree.clone(), module.clone());
    let code_pass_state = code_pass.run();

    let evaluator = Evaluator::new(module, code_pass_state.scope);
    let values = evaluator.evaluate();

    for error in &code_pass_state.errors {
        error.print(file_path, &lines);
    }

    for error in &evaluator.state.read().unwrap().errors {
        error.print(file_path, &lines);
    }

    for value in values {
        println!("{value}");
    }

    println!("{}", symbol_tree.format());
}
