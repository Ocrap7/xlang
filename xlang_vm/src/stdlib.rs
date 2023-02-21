use std::{fs::File, io::Read, sync::Arc};

use linked_hash_map::LinkedHashMap;
use xlang_core::Module;
use xlang_util::Rf;

use crate::{
    const_value::{ConstValue, ConstValueKind, Type},
    scope::{Scope, ScopeValue},
};

pub fn std_module() -> Arc<Module> {
    let file_path = "test_files/std.xl";
    let mut file = File::open(file_path).unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input, "std");
    for error in errors {
        println!("{error}")
    }

    Arc::new(module)
}

pub fn fill_module(module: Rf<Scope>) {
    let mut module = module.borrow_mut();

    // module.insert("print", ScopeValue::)

    create_func(
        &mut module,
        "print",
        [("name".to_string(), Type::Empty)].into_iter(),
        [].into_iter(),
        Arc::new(|params| {
            println!("{}", params.get("name").unwrap());
            LinkedHashMap::new()
        }),
    );
}

fn create_func<P: Iterator<Item = (String, Type)>, R: Iterator<Item = (String, Type)>>(
    module: &mut Scope,
    name: &str,
    p: P,
    r: R,
    func: Arc<
        dyn Fn(&LinkedHashMap<String, ConstValue>) -> LinkedHashMap<String, ConstValue>
            + Sync
            + Send,
    >,
) -> Rf<Scope> {
    // let rf = Rf::new(func as (dyn (FnMut(LinkedHashMap<String, ConstValue>) -> LinkedHashMap<String, ConstValue>) + 'static));

    let sym = module.insert(name, ScopeValue::Root, 0);

    let cv = ScopeValue::ConstValue(ConstValue {
        kind: ConstValueKind::NativeFunction {
            rf: sym,
            callback: func,
        },
        ty: Type::Function {
            parameters: LinkedHashMap::from_iter(p),
            return_parameters: LinkedHashMap::from_iter(r),
        },
    });

    module.update(name, cv).unwrap()
}
