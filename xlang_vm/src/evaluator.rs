use std::{
    collections::HashMap,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use xlang_core::{
    ast::{ArgList, Expression, ParamaterList, Statement},
    token::{Operator, SpannedToken, Token},
    Module,
};
use xlang_util::format::TreeDisplay;

use crate::{
    const_value::{ConstValue, ConstValueKind, Type},
    scope::{ScopeManager, ScopeValue},
};

pub struct EvaluatorState {
    scope: ScopeManager,
}

pub struct Evaluator {
    module: Module,
    state: RwLock<EvaluatorState>,
}

impl Evaluator {
    pub fn new(module: Module) -> Evaluator {
        Evaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: ScopeManager::new(),
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
        self.wstate().scope.push_scope();

        for stmt in &self.module.stmts {
            self.evaluate_statement(stmt);
        }

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
                self.wstate()
                    .scope
                    .update_value(id, ScopeValue::Record { members });
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

                self.wstate().scope.update_value(
                    id,
                    ScopeValue::ConstValue(ConstValue::func(
                        Statement::clone(body),
                        parameters,
                        return_parameters,
                    )),
                );
            }
            Statement::Decleration {
                ident: SpannedToken(_, Token::Ident(id)),
                expr: Some(expr),
                ..
            } => {
                let expr = self.evaluate_expression(expr);
                self.wstate()
                    .scope
                    .update_value(id, ScopeValue::ConstValue(expr));
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
            _ => (),
        }
        ConstValue::empty()
    }

    pub fn evaluate_params(&self, params: &ParamaterList) -> HashMap<String, Type> {
        let iter = params.items.iter_items().filter_map(|f| {
            if let (Some(SpannedToken(_, Token::Ident(name))), Some(ty)) = (&f.name, &f.ty) {
                Some((name.clone(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        HashMap::from_iter(iter)
    }

    pub fn evaluate_expression(&self, expression: &Expression) -> ConstValue {
        match expression {
            Expression::Integer(val, _, _) => ConstValue::cinteger(*val),
            Expression::Float(val, _, _) => ConstValue::cfloat(*val),
            Expression::Ident(SpannedToken(_, Token::Ident(id))) => {
                if let Some(ScopeValue::ConstValue(symb)) = self.rstate().scope.find_symbol(id) {
                    symb.clone()
                } else {
                    ConstValue::empty()
                }
            }
            Expression::BinaryExpression {
                left: Some(left),
                right: Some(right),
                op_token: Some(SpannedToken(_, Token::Operator(o))),
            } => self.evaluate_binary_expression(left, o, right),
            Expression::FunctionCall { expr, args } => {
                let expr = self.evaluate_expression(&*expr);
                let args = self.evaluate_args(args);

                match (expr.ty, expr.kind) {
                    (
                        Type::Function {
                            parameters: ptypes,
                            return_parameters: rptypes,
                        },
                        ConstValueKind::Function { body },
                    ) => {
                        self.wstate().scope.push_scope();
                        // TODO: verify types and such
                        for (arg, (name, ty)) in args.into_iter().zip(ptypes.into_iter()) {
                            self.wstate()
                                .scope
                                .update_value(&name, ScopeValue::ConstValue(arg));
                        }

                        let _ = self.evaluate_statement(&body);

                        // TODO: verify types here as well
                        let value = if rptypes.len() == 1 {
                            let Some((name, ty)) = rptypes
                                .into_iter().next() else {
                                    return ConstValue::empty()
                                };
                            if let Some(ScopeValue::ConstValue(cv)) =
                                self.rstate().scope.find_symbol(&name)
                            {
                                cv.clone()
                            } else {
                                ConstValue::default_for(ty)
                            }
                        } else {
                            let return_values: Vec<_> = rptypes
                                .into_iter()
                                .map(|(name, ty)| {
                                    if let Some(ScopeValue::ConstValue(cv)) =
                                        self.rstate().scope.find_symbol(&name)
                                    {
                                        cv.clone()
                                    } else {
                                        ConstValue::default_for(ty)
                                    }
                                })
                                .collect();

                            ConstValue::tuple(return_values)
                        };

                        self.wstate().scope.pop_scope();

                        value
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
        left: &Expression,
        op: &Operator,
        right: &Expression,
    ) -> ConstValue {
        match (op, left) {
            (Operator::Equals, Expression::Ident(SpannedToken(_, Token::Ident(name)))) => {
                let right = self.evaluate_expression(right);
                self.wstate()
                    .scope
                    .update_value(name, ScopeValue::ConstValue(right.clone()));
                return right;
            }
            _ => (),
        }
        let left = self.evaluate_expression(left);
        let right = self.evaluate_expression(right);

        match (left.ty, right.ty) {
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
                    width,
                    signed,
                ),
                Operator::Minus => ConstValue::integer(
                    left.kind.as_integer() - right.kind.as_integer(),
                    width,
                    signed,
                ),
                Operator::Multiply => ConstValue::integer(
                    left.kind.as_integer() * right.kind.as_integer(),
                    width,
                    signed,
                ),
                Operator::Divide => ConstValue::integer(
                    left.kind.as_integer() / right.kind.as_integer(),
                    width,
                    signed,
                ),
                Operator::Exponent => ConstValue::integer(
                    left.kind.as_integer().pow(right.kind.as_integer() as _),
                    width,
                    signed,
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
                    ConstValue::float(left.kind.as_float() + right.kind.as_float(), width)
                }
                Operator::Minus => {
                    ConstValue::float(left.kind.as_float() - right.kind.as_float(), width)
                }
                Operator::Multiply => {
                    ConstValue::float(left.kind.as_float() * right.kind.as_float(), width)
                }
                Operator::Divide => {
                    ConstValue::float(left.kind.as_float() / right.kind.as_float(), width)
                }
                Operator::Exponent => {
                    ConstValue::float(left.kind.as_float().powf(right.kind.as_float()), width)
                }
                _ => ConstValue::empty(),
            },
            _ => ConstValue::empty(),
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
            xlang_core::ast::Type::Ident(SpannedToken(_, Token::Ident(id))) => {
                Type::Ident(id.clone())
            }
            _ => Type::Empty,
        }
    }
}
