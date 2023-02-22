#![feature(trait_upcasting)]

use ast::{Expression, ParamaterList, Statement, Type};
use lexer::Lexer;
use linked_hash_map::LinkedHashMap;
use log::{Log, SetLoggerError};
use parser::Parser;
use xlang_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parse_expression;
pub mod parser;
pub mod token;

use error::ParseError;
pub use pollster;

impl Module {
    pub fn parse_str(input: &str, mod_name: &str) -> (Module, Vec<ParseError>) {
        let mut lexer = Lexer {};
        let tokens = lexer.lex(input);
        for p in &tokens {
            println!("{:#?}", p);
        }

        let parser = Parser::new(tokens);
        let parsed = parser.parse().unwrap();
        for p in &parsed {
            println!("{}", p.format());
        }

        let er = parser.get_errors().clone();

        // let mods = Symbol::new_root();
        // let md = ModuleDescender::new(mods.clone())
        //     .with_on_parameters(|params, ud| {
        //         for param in params.iter_items() {
        //             if let (Some(SpannedToken(_, Token::Ident(ident))), Some(ty)) =
        //                 (&param.name, &param.ty)
        //             {
        //                 Symbol::insert(&ud, ident, SymbolKind::Parameter { ty: ty.clone() });
        //             }
        //         }
        //         ud
        //     })
        //     .with_on_return_parameters(|params, ud| {
        //         for param in params.iter_items() {
        //             if let (Some(SpannedToken(_, Token::Ident(ident))), Some(ty)) =
        //                 (&param.name, &param.ty)
        //             {
        //                 Symbol::insert(&ud, ident, SymbolKind::ReturnParameter { ty: ty.clone() });
        //             }
        //         }
        //         ud
        //     })
        //     .with_on_statement(|st, ud| {
        //         match st {
        //             Statement::Decleration {
        //                 ident: SpannedToken(_, Token::Ident(id)),
        //                 expr,
        //                 ..
        //             } => {
        //                 let cd = match expr {
        //                     Some(Expression::Record { .. }) => {
        //                         Symbol::insert(&ud, id, SymbolKind::Record)
        //                     }
        //                     Some(Expression::Function { .. }) => {
        //                         Symbol::insert(&ud, id, SymbolKind::Function {})
        //                     }
        //                     // _ => Symbol::insert(&ud, id, SymbolKind::Variable {}),
        //                     _ => ud.clone(),
        //                 };

        //                 return (cd, ud);
        //             }
        //             Statement::UseStatement { args, .. } => {
        //                 let res: Option<Vec<String>> = args
        //                     .iter_items()
        //                     .inspect(|f| println!("{:?}", f))
        //                     .map(|a| match a {
        //                         SpannedToken(_, Token::Ident(i)) => Some(i.clone()),
        //                         _ => None,
        //                     })
        //                     .collect();
        //                 if let Some(res) = res {
        //                     let cd = Symbol::insert(&ud, "use", SymbolKind::Use(res));
        //                     return (cd, ud);
        //                 }
        //             }
        //             _ => (),
        //         }
        //         (ud.clone(), ud)
        //     });

        // md.descend(&parsed);

        (
            Module {
                name: mod_name.to_string(),
                content: input.to_string(),
                stmts: parsed,
            },
            er,
        )
    }
}

pub fn set_logger(logger: Box<dyn Log>) -> Result<(), SetLoggerError> {
    log::set_boxed_logger(logger)
}

pub struct Module {
    pub name: String,
    pub content: String,
    pub stmts: Vec<Statement>,
}

impl Module {
    pub fn empty(name: &str) -> Module {
        Module {
            name: name.to_string(),
            content: "".to_string(),
            stmts: Vec::new()
        }
    }

    pub fn format(&self) -> String {
        self.stmts
            .iter()
            .map(|f| format!("{}\n", f.format()))
            .collect()
    }

    // pub fn resolve_symbol_in_scope<'a>(
    //     &self,
    //     symbol: &str,
    //     scope: impl Iterator<Item = &'a String>,
    // ) -> Option<Rf<Symbol>> {
    //     let Some(sym) = self.resolve_symbol_chain_string(scope) else {
    //         return None
    //     };
    //     self.impl_resolve_symbol_in_scope(symbol, &sym)
    // }

    // pub fn impl_resolve_symbol_in_scope(
    //     &self,
    //     symbol: &str,
    //     node: &Rf<Symbol>,
    // ) -> Option<Rf<Symbol>> {
    //     let nodev = node.borrow();
    //     if let SymbolKind::Use(_) = &nodev.kind {
    //         return None;
    //     }
    //     if let Some(child) = nodev.children.get(symbol) {
    //         Some(child.clone())
    //     } else {
    //         for (_, child) in &nodev.children {
    //             let child = child.borrow();
    //             if let SymbolKind::Use(scp) = &child.kind {
    //                 return self.resolve_symbol_in_scope(symbol, scp.iter());
    //             }
    //         }
    //         None
    //     }
    // }

    // pub fn resolve_symbol(&self, node: &Rf<Symbol>, symbol_name: &str) -> Option<Rf<Symbol>> {
    //     if let Some(node) = self.impl_resolve_symbol_in_scope(symbol_name, node) {
    //         Some(node)
    //     } else {
    //         let Some(parent) = ({ &node.borrow().parent }) else {
    //                 return None
    //             };

    //         self.resolve_symbol(parent, symbol_name)
    //     }
    // }

    // pub fn resolve_symbol_chain_indicies<'a>(
    //     &self,
    //     iter: impl Iterator<Item = &'a usize>,
    // ) -> Option<Rf<Symbol>> {
    //     Module::impl_resolve_symbol_chain_indicies(&self.symbol_tree, iter).ok()
    // }

    // fn impl_resolve_symbol_chain_indicies<'a>(
    //     last: &Rf<Symbol>,
    //     mut iter: impl Iterator<Item = &'a usize>,
    // ) -> Result<Rf<Symbol>, bool> {
    //     if let Some(index) = iter.next() {
    //         if let Some(s) = last.borrow().children.values().nth(*index) {
    //             match Module::impl_resolve_symbol_chain_indicies(s, iter) {
    //                 Ok(n) => return Ok(n),
    //                 Err(true) => return Ok(s.clone()),
    //                 _ => (),
    //             }
    //         }
    //     } else {
    //         return Err(true);
    //     }
    //     Err(false)
    // }

    // pub fn resolve_symbol_chain<'a>(
    //     &self,
    //     iter: impl Iterator<Item = &'a SpannedToken>,
    // ) -> Option<Rf<Symbol>> {
    //     Module::impl_resolve_from_iter(&self.symbol_tree, iter).ok()
    // }

    // pub fn resolve_symbol_chain_string<'a>(
    //     &self,
    //     iter: impl Iterator<Item = &'a String>,
    // ) -> Option<Rf<Symbol>> {
    //     Module::impl_resolve_from_iter_string(&self.symbol_tree, iter).ok()
    // }

    // pub fn iter_symbol<'a, F: FnMut(&SpannedToken, &Rf<Symbol>)>(
    //     &self,
    //     iter: impl Iterator<Item = &'a SpannedToken>,
    //     f: F,
    // ) {
    //     Module::impl_iter_symbol(&self.symbol_tree, iter, f);
    // }

    // fn impl_iter_symbol<'a, F: FnMut(&SpannedToken, &Rf<Symbol>)>(
    //     last: &Rf<Symbol>,
    //     mut iter: impl Iterator<Item = &'a SpannedToken>,
    //     mut f: F,
    // ) {
    //     if let Some(tok @ SpannedToken(_, Token::Ident(i))) = iter.next() {
    //         if let Some(s) = last.borrow().children.get(i) {
    //             f(tok, s);
    //             Module::impl_iter_symbol(s, iter, f);
    //         }
    //     }
    // }

    // fn impl_resolve_from_iter<'a>(
    //     last: &Rf<Symbol>,
    //     mut iter: impl Iterator<Item = &'a SpannedToken>,
    // ) -> Result<Rf<Symbol>, bool> {
    //     if let Some(SpannedToken(_, Token::Ident(i))) = iter.next() {
    //         if let Some(s) = last.borrow().children.get(i) {
    //             match Module::impl_resolve_from_iter(s, iter) {
    //                 Ok(n) => return Ok(n),
    //                 Err(true) => return Ok(s.clone()),
    //                 _ => (),
    //             }
    //         }
    //     } else {
    //         return Err(true);
    //     }
    //     Err(false)
    // }

    // fn impl_resolve_from_iter_string<'a>(
    //     last: &Rf<Symbol>,
    //     mut iter: impl Iterator<Item = &'a String>,
    // ) -> Result<Rf<Symbol>, bool> {
    //     if let Some(i) = iter.next() {
    //         if let Some(s) = last.borrow().children.get(i) {
    //             match Module::impl_resolve_from_iter_string(s, iter) {
    //                 Ok(n) => return Ok(n),
    //                 Err(true) => return Ok(s.clone()),
    //                 _ => (),
    //             }
    //         }
    //     } else {
    //         return Err(true);
    //     }
    //     Err(false)
    // }
}

pub enum SymbolKind {
    Record,
    Function,
    Variable,
    Parameter { ty: Type },
    ReturnParameter { ty: Type },
    Use(Vec<String>),
    Root,
}

pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub parent: Option<Rf<Symbol>>,
    pub children: LinkedHashMap<String, Rf<Symbol>>,
}

impl NodeDisplay for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            SymbolKind::Root => f.write_str("Root"),
            SymbolKind::Record { .. } => write!(f, "Record `{}`", self.name),
            SymbolKind::Function { .. } => write!(f, "Function `{}`", self.name),
            SymbolKind::Variable { .. } => write!(f, "Variable `{}`", self.name),
            SymbolKind::Parameter { .. } => write!(f, "Parameter `{}`", self.name),
            SymbolKind::ReturnParameter { .. } => write!(f, "Return Parameter`{}`", self.name),
            SymbolKind::Use(_) => write!(f, "Use"),
        }
    }
}

impl TreeDisplay for Symbol {
    fn num_children(&self) -> usize {
        match &self.kind {
            SymbolKind::Parameter { .. } => 1,
            SymbolKind::ReturnParameter { .. } => 1,
            _ => self.children.len(),
        }
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match &self.kind {
            SymbolKind::Parameter { ty } => Some(ty),
            SymbolKind::ReturnParameter { ty } => Some(ty),
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay + 'a> {
        let p = self.children.values().nth(index).unwrap().borrow(); //.map(|f| &*f.borrow())

        Box::new(p)
    }
}

impl Symbol {
    pub fn new_root() -> Rf<Symbol> {
        Rf::new(Symbol {
            name: "root".to_string(),
            kind: SymbolKind::Root,
            parent: None,
            children: LinkedHashMap::new(),
        })
    }

    pub fn insert_unnamed(symb: &Rf<Symbol>, name: &str, kind: SymbolKind) -> Option<Rf<Symbol>> {
        let insert_index = {
            let symb = symb.borrow();

            // Find free index; max 128
            [0; 128]
                .into_iter()
                .enumerate()
                .map(|(i, _)| i)
                .find_map(|v| {
                    let val = format!("{v}");
                    if symb.children.get(&val).is_none() {
                        Some(val)
                    } else {
                        None
                    }
                })
        };

        if let Some(insert_index) = insert_index {
            let new = Rf::new(Symbol {
                name: name.into(),
                kind,
                parent: Some(symb.clone()),
                children: LinkedHashMap::new(),
            });

            symb.borrow_mut().children.insert(insert_index, new.clone());

            Some(new)
        } else {
            None
        }
    }

    pub fn insert(symb: &Rf<Symbol>, name: &str, kind: SymbolKind) -> Rf<Symbol> {
        let new = Rf::new(Symbol {
            name: name.to_string(),
            kind,
            parent: Some(symb.clone()),
            children: LinkedHashMap::new(),
        });

        symb.borrow_mut()
            .children
            .insert(name.to_string(), new.clone());

        new
    }
}

#[derive(Default)]
pub struct ModuleDescender<U: Clone> {
    user_data: U,
    on_statement: Option<Box<dyn FnMut(&Statement, U) -> (U, U)>>,
    on_expression: Option<Box<dyn FnMut(&Expression, U) -> U>>,
    on_parameters: Option<Box<dyn FnMut(&ParamaterList, U) -> U>>,
    on_return_parameters: Option<Box<dyn FnMut(&ParamaterList, U) -> U>>,
}

impl<U: Clone> ModuleDescender<U> {
    pub fn new(user_data: U) -> ModuleDescender<U> {
        ModuleDescender {
            user_data,
            on_statement: None,
            on_expression: None,
            on_parameters: None,
            on_return_parameters: None,
        }
    }

    pub fn with_on_statement(
        mut self,
        on_statement: impl FnMut(&Statement, U) -> (U, U) + 'static,
    ) -> ModuleDescender<U> {
        self.on_statement = Some(Box::new(on_statement));
        self
    }

    pub fn with_on_expression(
        mut self,
        on_value: impl FnMut(&Expression, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_expression = Some(Box::new(on_value));
        self
    }

    pub fn with_on_parameters(
        mut self,
        on_parameters: impl FnMut(&ParamaterList, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_parameters = Some(Box::new(on_parameters));
        self
    }

    pub fn with_on_return_parameters(
        mut self,
        on_return_parameters: impl FnMut(&ParamaterList, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_return_parameters = Some(Box::new(on_return_parameters));
        self
    }

    pub fn descend(mut self, node: &Vec<Statement>) -> U {
        for node in node {
            self.descend_statement(node)
        }
        self.user_data
    }

    pub fn descend_expression(&mut self, node: &Expression) {
        match node {
            Expression::Function {
                parameters,
                return_parameters,
                ..
            } => {
                if let Some(on_prm) = &mut self.on_parameters {
                    on_prm(parameters, self.user_data.clone());
                }

                if let Some(on_prm) = &mut self.on_return_parameters {
                    on_prm(return_parameters, self.user_data.clone());
                }
            }
            Expression::Record { parameters } => {
                if let Some(on_prm) = &mut self.on_parameters {
                    on_prm(parameters, self.user_data.clone());
                }
            }
            _ => (),
        }
        if let Some(on_value) = &mut self.on_expression {
            self.user_data = on_value(node, self.user_data.clone())
        }
    }

    pub fn descend_statement(&mut self, node: &Statement) {
        let sets = if let Some(on_statement) = &mut self.on_statement {
            Some(on_statement(node, self.user_data.clone()))
        } else {
            None
        };
        let sets = if let Some(sets) = sets {
            self.user_data = sets.0;
            Some(sets.1)
        } else {
            None
        };
        match node {
            Statement::Decleration {
                expr: Some(expr), ..
            } => self.descend_expression(expr),
            Statement::Expression(e) => self.descend_expression(e),
            _ => (),
        }
        if let Some(sets) = sets {
            self.user_data = sets;
        }
    }
}

#[derive(Default)]
pub struct MutModuleDescender<U: Clone> {
    callback_first: bool,
    user_data: U,
    on_statement: Option<Box<dyn FnMut(&mut Statement, U) -> (U, U)>>,
    on_expression: Option<Box<dyn FnMut(&mut Expression, U) -> U>>,
}

impl<U: Clone> MutModuleDescender<U> {
    pub fn new(user_data: U) -> MutModuleDescender<U> {
        MutModuleDescender {
            callback_first: true,
            user_data,
            on_statement: None,
            on_expression: None,
        }
    }

    pub fn with_on_statement(
        mut self,
        on_statement: impl FnMut(&mut Statement, U) -> (U, U) + 'static,
    ) -> MutModuleDescender<U> {
        self.on_statement = Some(Box::new(on_statement));
        self
    }

    pub fn with_on_expression(
        mut self,
        on_value: impl FnMut(&mut Expression, U) -> U + 'static,
    ) -> MutModuleDescender<U> {
        self.on_expression = Some(Box::new(on_value));
        self
    }

    pub fn with_callback_first(mut self, callback_first: bool) -> MutModuleDescender<U> {
        self.callback_first = callback_first;
        self
    }

    pub fn descend(mut self, node: &mut Vec<Statement>) -> U {
        for node in node {
            self.descend_statement(node)
        }
        self.user_data
    }

    pub fn descend_expression(&mut self, node: &mut Expression) {
        if let Some(on_value) = &mut self.on_expression {
            self.user_data = on_value(node, self.user_data.clone())
        }
    }

    pub fn descend_statement(&mut self, node: &mut Statement) {
        if self.callback_first {
            let sets = if let Some(on_statement) = &mut self.on_statement {
                Some(on_statement(node, self.user_data.clone()))
            } else {
                None
            };
            let sets = if let Some(sets) = sets {
                self.user_data = sets.0;
                Some(sets.1)
            } else {
                None
            };
            match node {
                Statement::Decleration {
                    expr: Some(expr), ..
                } => self.descend_expression(expr),
                Statement::Expression(e) => self.descend_expression(e),
                _ => (),
            }
            if let Some(sets) = sets {
                self.user_data = sets;
            }
        } else {
            match node {
                Statement::Decleration {
                    expr: Some(expr), ..
                } => self.descend_expression(expr),
                Statement::Expression(e) => self.descend_expression(e),
                _ => (),
            }
            if let Some(on_statement) = &mut self.on_statement {
                self.user_data = on_statement(node, self.user_data.clone()).1
            }
        }
    }
}
