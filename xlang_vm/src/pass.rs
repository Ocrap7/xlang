use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

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
    // Variables,
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
    pub fn new(root: Rf<Scope>, module: Arc<Module>, index: usize) -> CodePass {
        let scope = Rf::new(Scope::new(ScopeValue::Module(module.clone()), index));
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
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index);
        }

        self.pass = PassType::Members;
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index);
        }

        self.state.into_inner().unwrap()
    }

    pub fn evaluate_statement(&self, statement: &Statement, index: usize) {
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
                            index,
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
                    let sym = self.wstate().scope.insert_value(
                        id,
                        ScopeValue::ConstValue(ConstValue::empty()),
                        index,
                    );

                    let eparameters = self.evaluate_params(parameters);
                    let ereturn_parameters = self.evaluate_params(return_parameters);

                    self.wstate().scope.insert_value(
                        id,
                        ScopeValue::ConstValue(ConstValue::func(
                            Statement::clone(body),
                            eparameters,
                            ereturn_parameters,
                            sym,
                        )),
                        index,
                    );
                }
                PassType::Members => {
                    let Some(rf) = self.rstate().scope.find_symbol(id.as_str()) else {
                        return;
                    };
                    let (pvals, rvals) = {
                        let ScopeValue::ConstValue(ConstValue {
                        ty: Type::Function { parameters, return_parameters },
                        ..
                    }) = &rf.borrow().value else {
                        return;
                    };

                        let pvals: Vec<_> = parameters
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    ScopeValue::ConstValue(ConstValue::default_for(ty)),
                                )
                            })
                            .collect();

                        let rvals: Vec<_> = return_parameters
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    ScopeValue::ConstValue(ConstValue::default_for(ty)),
                                )
                            })
                            .collect();
                        (pvals, rvals)
                    };

                    self.wstate().scope.push_scope(rf);

                    for (name, ty) in pvals {
                        self.wstate().scope.update_value(
                            &name,
                            ty,
                            index,
                        );
                    }

                    for (name, ty) in rvals {
                        self.wstate().scope.update_value(
                            &name,
                            ty,
                            index,
                        );
                    }

                    self.wstate().scope.pop_scope();
                }
            },
            Statement::Decleration { ident, .. } => match self.pass {
                PassType::TypeOnly => {
                    self.wstate().scope.insert_value(
                        ident.as_str(),
                        ScopeValue::ConstValue(ConstValue::empty()),
                        index,
                    );
                }
                _ => (),
            },
            Statement::UseStatement { args, .. } => match self.pass {
                PassType::TypeOnly => {
                    let path = args
                        .iter_items()
                        .map(|sym| sym.as_str().to_string())
                        .collect();
                    self.wstate().scope.add_use(path)
                }
                _ => (),
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
                Type::Empty
            }
        }
    }

    // fn add_error(&self, error: EvaluationError) {
    //     self.wstate().errors.push(error)
    // }
}
