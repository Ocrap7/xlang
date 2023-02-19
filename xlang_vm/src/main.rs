use std::{fs::File, io::Read};

use evaluator::Evaluator;
use xlang_core::Module;
use xlang_util::format::TreeDisplay;

mod const_value;
mod evaluator;
mod scope;

fn main() {
    let mut file = File::open("test_files/test.xl").unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input);
    for error in errors {
        println!("{error}")
    }

    let evaluator = Evaluator::new(module);

    let values = evaluator.evaluate();

    for value in values {
        println!("{value}");
    }

    println!("{}", evaluator.state.read().unwrap().scope.format());
}
