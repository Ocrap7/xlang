use std::{
    rc::Rc,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard, Arc},
};

use linked_hash_map::LinkedHashMap;
use xlang_core::{
    ast::{Expression, ParamaterList, Statement},
    token::{SpannedToken, Token},
    Module,
};
use xlang_util::Rf;

use crate::{
    const_value::{ConstValue, Type},
    error::EvaluationError,
    scope::{Scope, ScopeManager, ScopeValue},
};

pub enum PassType {
    TypeOnly,
    Members,
}

pub struct CodePassState {
    pub scope: ScopeManager,
    pub errors: Vec<EvaluationError>,
}

pub struct CodePass {
    module: Arc<Module>,
    state: RwLock<CodePassState>,
    pass: PassType,
}

impl CodePass {
    pub fn new(root: Rf<Scope>, module: Arc<Module>) -> CodePass {
        let scope = Rf::new(Scope::new(ScopeValue::Module(module.clone())));
        CodePass {
            module,
            state: RwLock::new(CodePassState {
                scope: ScopeManager::new(root, scope),
                errors: Vec::new(),
            }),
            pass: PassType::TypeOnly,
        }
    }

    fn rstate(&self) -> RwLockReadGuard<'_, CodePassState> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, CodePassState> {
        self.state.write().unwrap()
    }
}

impl CodePass {
    pub fn run(mut self) -> CodePassState {
        for stmt in &self.module.stmts {
            self.evaluate_statement(stmt);
        }

        self.pass = PassType::Members;
        for stmt in &self.module.stmts {
            self.evaluate_statement(stmt);
        }

        self.state.into_inner().unwrap()
    }

    pub fn evaluate_statement(&self, statement: &Statement) {
        match statement {
            Statement::Decleration {
                ident: SpannedToken(_, Token::Ident(id)),
                expr: Some(Expression::Record { parameters }),
                ..
            } => {
                match self.pass {
                    PassType::TypeOnly => {
                        self.wstate().scope.insert_value(
                            id,
                            ScopeValue::Record {
                                ident: id.to_string(),
                                members: LinkedHashMap::default(),
                            },
                        );
                    }
                    PassType::Members => {
                        let emembers = self.evaluate_params(parameters);
                        if let Some(sym) = self.wstate().scope.find_symbol(id) {
                            let mut sym = sym.borrow_mut();
                            if let ScopeValue::Record { members, .. } = &mut sym.value {
                                *members = emembers
                            }
                        }
                    }
                };
                // self.wstate()
                //     .scope
                //     .update_value(id, ScopeValue::Record { members });
            }
            Statement::Decleration {
                ident: SpannedToken(_, Token::Ident(id)),
                expr:
                    Some(Expression::Function {
                        parameters,
                        return_parameters,
                        body: Some(body),
                        ..
                    }),
                ..
            } => match self.pass {
                PassType::TypeOnly => {
                    let sym = self
                        .wstate()
                        .scope
                        .insert_value(id, ScopeValue::ConstValue(ConstValue::empty()));

                    self.wstate().scope.insert_value(
                        id,
                        ScopeValue::ConstValue(ConstValue::func(
                            Statement::clone(body),
                            LinkedHashMap::default(),
                            LinkedHashMap::default(),
                            sym,
                        )),
                    );
                }
                PassType::Members => {
                    let eparameters = self.evaluate_params(parameters);
                    let ereturn_parameters = self.evaluate_params(return_parameters);

                    if let Some(sym) = self.wstate().scope.find_symbol(id) {
                        let mut sym = sym.borrow_mut();
                        if let ScopeValue::ConstValue(ConstValue {
                            ty:
                                Type::Function {
                                    parameters,
                                    return_parameters,
                                },
                            ..
                        }) = &mut sym.value
                        {
                            *parameters = eparameters;
                            *return_parameters = ereturn_parameters;
                        }
                    }
                }
            },
            _ => (),
        }
    }

    pub fn evaluate_params(&self, params: &ParamaterList) -> LinkedHashMap<String, Type> {
        let iter = params.items.iter_items().filter_map(|f| {
            if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
                Some((ident.as_str().to_string(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        LinkedHashMap::from_iter(iter)
    }

    fn evaluate_type(&self, ty: &xlang_core::ast::Type) -> Type {
        match ty {
            xlang_core::ast::Type::Integer { width, signed, .. } => Type::Integer {
                width: *width,
                signed: *signed,
            },
            xlang_core::ast::Type::Float { width, .. } => Type::Float { width: *width },
            xlang_core::ast::Type::Ident(id) => {
                if let Some(sym) = { self.rstate().scope.find_symbol(id.as_str()) } {
                    return Type::Symbol(sym);
                }
                // self.add_error(EvaluationError {
                //     kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
                //     range: id.get_range(),
                // });
                return Type::Empty;
            }
        }
    }

    fn add_error(&self, error: EvaluationError) {
        self.wstate().errors.push(error)
    }
}
