use std::{fs::File, io::Read};

use xlang_core::{Module};

fn main() {
    let mut file = File::open("test_files/test.xl").unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let _parsed = Module::parse_str(&input);
}

