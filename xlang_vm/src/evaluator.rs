use std::{
    rc::Rc,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use linked_hash_map::LinkedHashMap;
use xlang_core::{
    ast::{ArgList, AstNode, Expression, ParamaterList, Statement},
    token::{Operator, Range, SpannedToken, Token},
    Module,
};
use xlang_util::format::TreeDisplay;

use crate::{
    const_value::{ConstValue, ConstValueKind, Type},
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    scope::{ScopeManager, ScopeValue},
};

pub struct EvaluatorState {
    pub scope: ScopeManager,
    pub errors: Vec<EvaluationError>,
}

pub struct Evaluator {
    module: Rc<Module>,
    pub state: RwLock<EvaluatorState>,
}

impl Evaluator {
    pub fn new(module: Rc<Module>, scope_manager: ScopeManager) -> Evaluator {
        Evaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: scope_manager,
                errors: Vec::new(),
            }),
        }
    }

    fn rstate(&self) -> RwLockReadGuard<'_, EvaluatorState> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, EvaluatorState> {
        self.state.write().unwrap()
    }
}

impl Evaluator {
    pub fn evaluate(&self) -> Vec<ConstValue> {
        let vals = self
            .module
            .stmts
            .iter()
            .map(|stmt| self.evaluate_statement(stmt))
            .collect();

        vals
    }

    pub fn evaluate_statement(&self, statement: &Statement) -> ConstValue {
        match statement {
            Statement::Decleration {
                ident: SpannedToken(_, Token::Ident(id)),
                expr: Some(Expression::Record { parameters }),
                ..
            } => {
                let members = self.evaluate_params(parameters);
                self.wstate().scope.update_value(
                    id,
                    ScopeValue::Record {
                        members,
                        ident: id.to_string(),
                    },
                );
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
            } => {
                let parameters = self.evaluate_params(parameters);
                let return_parameters = self.evaluate_params(return_parameters);

                self.wstate()
                    .scope
                    .update_value(id, ScopeValue::ConstValue(ConstValue::empty()));
                let sym = self.wstate().scope.find_symbol(id).unwrap();
                self.wstate().scope.update_value(
                    id,
                    ScopeValue::ConstValue(ConstValue::func(
                        Statement::clone(body),
                        parameters,
                        return_parameters,
                        sym,
                    )),
                );
            }
            Statement::Decleration {
                ident,
                expr: Some(expr),
                ..
            } => {
                let expr = self.evaluate_expression(expr);
                self.wstate()
                    .scope
                    .update_value(ident.as_str(), ScopeValue::ConstValue(expr));
            }
            Statement::Expression(expr) => return self.evaluate_expression(expr),
            Statement::List(list) => {
                if list.num_children() == 1 {
                    let item = list
                        .iter_items()
                        .next()
                        .expect("Value should have been present. This is probably a rustc bug");
                    return self.evaluate_statement(item);
                } else {
                    let values: Vec<_> = list
                        .iter_items()
                        .map(|stmt| self.evaluate_statement(stmt))
                        .collect();
                    return ConstValue::tuple(values);
                }
            }

            Statement::UseStatement { args, .. } => {
                let path = args
                    .iter_items()
                    .map(|sym| sym.as_str().to_string())
                    .collect();
                self.wstate().scope.add_use(path)
            }
            _ => (),
        }
        ConstValue::empty()
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

    pub fn evaluate_expression(&self, expression: &Expression) -> ConstValue {
        match expression {
            Expression::Integer(val, _, _) => ConstValue::cinteger(*val),
            Expression::Float(val, _, _) => ConstValue::cfloat(*val),
            Expression::Ident(tok @ SpannedToken(_, Token::Ident(id))) => {
                let sym = self.rstate().scope.find_symbol(id);
                if let Some(sym) = sym {
                    let symv = sym.borrow();
                    match &symv.value {
                        ScopeValue::ConstValue(cv) => cv.clone(),
                        ScopeValue::Record { .. } => ConstValue {
                            ty: Type::Symbol(sym.clone()),
                            kind: ConstValueKind::Empty,
                        },
                        _ => ConstValue::empty(),
                    }
                } else {
                    self.add_error(EvaluationError {
                        kind: EvaluationErrorKind::SymbolNotFound(id.to_string()),
                        range: tok.get_range(),
                    });
                    ConstValue::empty()
                }
            }
            Expression::BinaryExpression {
                left: Some(left),
                right: Some(right),
                op_token: Some(SpannedToken(_, Token::Operator(o))),
            } => self.evaluate_binary_expression(left, o, right),
            Expression::FunctionCall {
                expr,
                args: raw_args,
            } => {
                let expr = self.evaluate_expression(expr);
                let args = self.evaluate_args(raw_args);

                match (expr.ty, expr.kind) {
                    // Function is called
                    (
                        Type::Function {
                            parameters: ptypes,
                            return_parameters: rptypes,
                        },
                        ConstValueKind::Function { body, rf },
                    ) => {
                        self.wstate().scope.push_scope(rf.clone());

                        let has_args: Option<Vec<_>> = args
                            .into_iter()
                            .zip(ptypes.into_iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

                                if arg.ty != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.ty,
                                            ty,
                                            TypeHint::Parameter,
                                        ),
                                        range: raw_args
                                            .items
                                            .iter_items()
                                            .nth(i)
                                            .unwrap()
                                            .get_range(),
                                    });
                                    return None;
                                }
                                self.wstate()
                                    .scope
                                    .update_value(&name, ScopeValue::ConstValue(arg));

                                Some(())
                            })
                            .collect();

                        if has_args.is_none() {
                            return ConstValue::empty();
                        }

                        let _ = self.evaluate_statement(&body);

                        // TODO: verify types here as well

                        let return_values: LinkedHashMap<_, _> = rptypes
                            .into_iter()
                            .map(|(name, ty)| {
                                let sym = self.rstate().scope.find_symbol_local(&name);
                                let vl = if let Some(sym) = sym {
                                    let sym = sym.borrow();
                                    if let ScopeValue::ConstValue(cv) = &sym.value {
                                        if cv.ty == ty {
                                            cv.clone()
                                        } else {
                                            // TODO: error handling
                                            ConstValue::empty()
                                        }
                                    } else {
                                        // TODO: error handling
                                        ConstValue::empty()
                                    }
                                } else {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::NotInitialized {
                                            hint: TypeHint::ReturnParameter,
                                        },
                                        range: expression.get_range(),
                                    });
                                    ConstValue::default_for(ty)
                                };
                                (name, vl)
                            })
                            .collect();

                        let value = ConstValue::record_instance(rf.clone(), return_values);

                        self.wstate().scope.pop_scope();

                        value
                    }
                    (
                        Type::Function {
                            parameters: ptypes,
                            ..
                        },
                        ConstValueKind::NativeFunction { rf, callback },
                    ) => {
                        let arglen = args.len();
                        let plen = ptypes.len();

                        let has_args: Option<LinkedHashMap<_, _>> = args
                            .into_iter()
                            .zip(ptypes.into_iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

                                if arg.ty != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.ty,
                                            ty,
                                            TypeHint::Parameter,
                                        ),
                                        range: raw_args
                                            .items
                                            .iter_items()
                                            .nth(i)
                                            .unwrap()
                                            .get_range(),
                                    });
                                    return None;
                                }

                                Some((name, arg))
                            })
                            .collect();

                        if has_args.is_none() {
                            return ConstValue::empty();
                        } else if arglen != plen {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::ArgCountMismatch(arglen as _, plen as _),
                                range: raw_args.get_range(),
                            });
                            return ConstValue::empty();
                        }

                        let return_vals = callback(has_args.as_ref().unwrap());

                        let value = ConstValue::record_instance(rf.clone(), return_vals);

                        value
                    }
                    // Record is instantiated
                    (Type::Symbol(sym), _) => {
                        if let ScopeValue::Record { members, .. } = &sym.borrow().value {
                            let len_off = members.len() != args.len();
                            let args_vals: LinkedHashMap<_, _> = members
                                .iter()
                                .zip(args.into_iter())
                                .enumerate()
                                .filter_map(|(i, ((name, ty), arg))| {
                                    let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);
                                    if &arg.ty == ty {
                                        Some((name.clone(), arg))
                                    } else {
                                        self.add_error(EvaluationError {
                                            kind: EvaluationErrorKind::TypeMismatch(
                                                arg.ty,
                                                ty.clone(),
                                                TypeHint::Parameter,
                                            ),
                                            range: raw_args
                                                .items
                                                .iter_items()
                                                .nth(i)
                                                .unwrap()
                                                .get_range(),
                                        });
                                        None
                                    }
                                })
                                .collect();

                            if len_off {
                                // If the number of arguments doesn't match the record
                                self.add_error(EvaluationError {
                                    kind: EvaluationErrorKind::ArgCountMismatch(
                                        raw_args.len() as _,
                                        members.len() as _,
                                    ),
                                    range: raw_args.get_range(),
                                });
                            } else if args_vals.len() == members.len() {
                                // Everything good!
                                return ConstValue::record_instance(sym.clone(), args_vals);
                            }
                        }
                        ConstValue::empty()
                    }
                    // TODO: throw error
                    _ => ConstValue::empty(),
                }
            }
            _ => ConstValue::empty(),
        }
    }

    pub fn evaluate_binary_expression(
        &self,
        raw_left: &Expression,
        op: &Operator,
        raw_right: &Expression,
    ) -> ConstValue {
        match (op, raw_left) {
            (Operator::Equals, Expression::Ident(SpannedToken(_, Token::Ident(name)))) => {
                let right = self.evaluate_expression(raw_right);
                self.wstate()
                    .scope
                    .update_value(name, ScopeValue::ConstValue(right.clone()));
                return right;
            }
            (
                Operator::Equals,
                Expression::BinaryExpression {
                    op_token: Some(SpannedToken(_, Token::Operator(Operator::Dot))),
                    left: Some(dleft),
                    right: Some(dright),
                },
            ) => {
                let right = self.evaluate_expression(raw_right);
                let scope = &mut self.wstate().scope;
                let updated_value = scope.follow_member_access_mut(dleft, dright, |cv| {
                    *cv = right.clone();
                });
                if !updated_value {
                    return ConstValue::empty();
                };
                return right;
            }
            (Operator::Dot, _) => {
                let left = self.evaluate_expression(raw_left);
                match (left.kind, raw_right) {
                    (
                        ConstValueKind::RecordInstance { members, .. },
                        Expression::Ident(SpannedToken(_, Token::Ident(member))),
                    ) => {
                        if let Some(val) = members.get(member) {
                            return val.clone();
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        let left = self.evaluate_expression(raw_left);
        let right = self.evaluate_expression(raw_right);

        let res = match (&left.ty, &right.ty) {
            (Type::CoercibleInteger, Type::CoercibleInteger) => match op {
                Operator::Plus => {
                    ConstValue::cinteger(left.kind.as_integer() + right.kind.as_integer())
                }
                Operator::Minus => {
                    ConstValue::cinteger(left.kind.as_integer() - right.kind.as_integer())
                }
                Operator::Multiply => {
                    ConstValue::cinteger(left.kind.as_integer() * right.kind.as_integer())
                }
                Operator::Divide => {
                    ConstValue::cinteger(left.kind.as_integer() / right.kind.as_integer())
                }
                Operator::Exponent => {
                    ConstValue::cinteger(left.kind.as_integer().pow(right.kind.as_integer() as _))
                }
                _ => ConstValue::empty(),
            },
            (Type::Integer { width, signed }, Type::CoercibleInteger)
            | (Type::CoercibleInteger, Type::Integer { width, signed }) => match op {
                Operator::Plus => ConstValue::integer(
                    left.kind.as_integer() + right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Minus => ConstValue::integer(
                    left.kind.as_integer() - right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Multiply => ConstValue::integer(
                    left.kind.as_integer() * right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Divide => ConstValue::integer(
                    left.kind.as_integer() / right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Exponent => ConstValue::integer(
                    left.kind.as_integer().pow(right.kind.as_integer() as _),
                    *width,
                    *signed,
                ),
                _ => ConstValue::empty(),
            },
            (
                Type::Integer { width, signed },
                Type::Integer {
                    width: rw,
                    signed: rs,
                },
            ) if width == rw && signed == rs => match op {
                Operator::Plus => ConstValue::integer(
                    left.kind.as_integer() + right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Minus => ConstValue::integer(
                    left.kind.as_integer() - right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Multiply => ConstValue::integer(
                    left.kind.as_integer() * right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Divide => ConstValue::integer(
                    left.kind.as_integer() / right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Exponent => ConstValue::integer(
                    left.kind.as_integer().pow(right.kind.as_integer() as _),
                    *width,
                    *signed,
                ),
                _ => ConstValue::empty(),
            },
            (Type::CoercibleFloat, Type::CoercibleFloat) => match op {
                Operator::Plus => ConstValue::cfloat(left.kind.as_float() + right.kind.as_float()),
                Operator::Minus => ConstValue::cfloat(left.kind.as_float() - right.kind.as_float()),
                Operator::Multiply => {
                    ConstValue::cfloat(left.kind.as_float() * right.kind.as_float())
                }
                Operator::Divide => {
                    ConstValue::cfloat(left.kind.as_float() / right.kind.as_float())
                }
                Operator::Exponent => {
                    ConstValue::cfloat(left.kind.as_float().powf(right.kind.as_float()))
                }
                _ => ConstValue::empty(),
            },
            (Type::Float { width }, Type::CoercibleFloat)
            | (Type::CoercibleFloat, Type::Float { width }) => match op {
                Operator::Plus => {
                    ConstValue::float(left.kind.as_float() + right.kind.as_float(), *width)
                }
                Operator::Minus => {
                    ConstValue::float(left.kind.as_float() - right.kind.as_float(), *width)
                }
                Operator::Multiply => {
                    ConstValue::float(left.kind.as_float() * right.kind.as_float(), *width)
                }
                Operator::Divide => {
                    ConstValue::float(left.kind.as_float() / right.kind.as_float(), *width)
                }
                Operator::Exponent => {
                    ConstValue::float(left.kind.as_float().powf(right.kind.as_float()), *width)
                }
                _ => ConstValue::empty(),
            },
            (Type::Float { width }, Type::Float { width: rw }) if width == rw => match op {
                Operator::Plus => {
                    ConstValue::float(left.kind.as_float() + right.kind.as_float(), *width)
                }
                Operator::Minus => {
                    ConstValue::float(left.kind.as_float() - right.kind.as_float(), *width)
                }
                Operator::Multiply => {
                    ConstValue::float(left.kind.as_float() * right.kind.as_float(), *width)
                }
                Operator::Divide => {
                    ConstValue::float(left.kind.as_float() / right.kind.as_float(), *width)
                }
                Operator::Exponent => {
                    ConstValue::float(left.kind.as_float().powf(right.kind.as_float()), *width)
                }
                _ => ConstValue::empty(),
            },
            _ => ConstValue::empty(),
        };

        if let Type::Empty = &res.ty {
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::BinExpMismatch(op.clone(), left.ty, right.ty),
                range: Range::from((&raw_left.get_range(), &raw_right.get_range())),
            });
            ConstValue::empty()
        } else {
            return res;
        }
    }

    fn evaluate_args(&self, args: &ArgList) -> Vec<ConstValue> {
        args.iter_items()
            .map(|expr| self.evaluate_expression(expr))
            .collect()
    }

    fn evaluate_type(&self, ty: &xlang_core::ast::Type) -> Type {
        match ty {
            xlang_core::ast::Type::Integer { width, signed, .. } => Type::Integer {
                width: *width,
                signed: *signed,
            },
            xlang_core::ast::Type::Float { width, .. } => Type::Float { width: *width },
            xlang_core::ast::Type::Ident(id) => {
                if let Some(sym) = self.rstate().scope.find_symbol(id.as_str()) {
                    return Type::Symbol(sym);
                }
                self.add_error(EvaluationError {
                    kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
                    range: id.get_range(),
                });
                Type::Empty
            }
        }
    }

    fn add_error(&self, error: EvaluationError) {
        self.wstate().errors.push(error)
    }
}
