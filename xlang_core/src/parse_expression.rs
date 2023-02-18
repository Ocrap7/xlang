use crate::{
    ast::{Expression, PunctuationList, Statement, Type},
    error::{ParseError, ParseErrorKind},
    parser::Parser,
    token::{Operator, Range, SpannedToken, Token},
};

impl Parser {
    pub fn parse_expression(&self, last_prec: u32) -> Option<Expression> {
        let mut left = self.parse_primary_expression();

        while let Some(t) = self.tokens.peek() {
            left = match t {
                Token::Operator(o) => {
                    let prec = self.precedence_of_operator(o);
                    if prec <= last_prec || prec == 0 {
                        break;
                    }

                    match (o, left) {
                        (Operator::OpenParen, Some(expr)) => {
                            let (cl, skip) = self.parse_function_call(expr);
                            left = Some(cl);
                            if skip {
                                continue;
                            }
                        }
                        (_, l) => left = l,
                    };

                    let op_token = self.tokens.next().cloned();

                    let right = self.parse_expression(prec);

                    Some(Expression::BinaryExpression {
                        left: left.map(|f| Box::new(f)),
                        right: right.map(|f| Box::new(f)),
                        op_token,
                    })
                }
                _ => break,
            }
        }

        left
    }

    pub fn parse_primary_expression(&self) -> Option<Expression> {
        if let Some(Token::Operator(Operator::OpenParen)) = self.tokens.peek() {
            let state = self.save_state();
            if let Some(func) = self.parse_function() {
                return Some(func);
            }
            state.restore(&self.tokens);

            let _open = self.tokens.next().unwrap();
            let expr = self.parse_expression(0);
            let _close = self.tokens.next().unwrap(); // TODO: error

            expr
        } else {
            self.parse_literal()
        }
    }

    pub fn parse_function_call(&self, expression: Expression) -> (Expression, bool) {
        let Some(args) = self.parse_arguments() else {
            return (expression, false);
        };

        (
            Expression::FunctionCall {
                expr: Box::new(expression),
                args,
            },
            true,
        )
    }

    pub fn parse_function(&self) -> Option<Expression> {
        let parameters = self.parse_parameters();

        let parameters = if let Some(Token::Operator(Operator::Arrow)) = self.tokens.peek() {
            let arrow = self.tokens.next().unwrap().clone();
            let return_parameters = self.parse_parameters();

            match (parameters, return_parameters) {
                (Some(parameters), Some(return_parameters)) => {
                    if let Some((comma, body)) = self.parse_function_body() {
                        return Some(Expression::Function {
                            parameters,
                            arrow,
                            return_parameters,
                            comma,
                            body: Some(Box::new(Statement::List(body))),
                        });
                    } else {
                        return Some(Expression::Function {
                            parameters,
                            arrow,
                            return_parameters,
                            comma: None,
                            body: None,
                        });
                    }
                }
                (l, _) => l,
            }
        } else {
            parameters
        };

        if let Some(parameters) = parameters {
            Some(Expression::Record { parameters })
        } else {
            None
        }
    }

    pub fn parse_function_body(
        &self,
    ) -> Option<(Option<SpannedToken>, PunctuationList<Statement>)> {
        let first_comma = self.expect_operator(Operator::Comma).cloned();

        let mut stmts = PunctuationList::new();

        while let Some(stmt) = self.parse_statement() {
            let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                self.tokens.next().cloned()
            } else {
                None
            };
            if let Some(Token::Newline) = self.tokens.peek() {
                stmts.push(stmt, comma);
                break;
            }
            if comma.is_none() {
                self.add_error(ParseError {
                    kind: ParseErrorKind::InvalidSyntax(format!(
                        "Expected comma in function body!"
                    )),
                    range: Range::default(),
                });
                stmts.push(stmt, comma);
                return Some((first_comma, stmts));
            }
            stmts.push(stmt, comma);
        }

        Some((first_comma, stmts))
    }

    pub fn parse_literal(&self) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Integer(i)) => Some(Expression::Integer(
                *i,
                None,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Float(f)) => Some(Expression::Float(
                *f,
                None,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Ident(_)) => Some(Expression::Ident(self.tokens.next().unwrap().clone())),
            _ => None,
        }
    }

    pub fn parse_type(&self) -> Option<Type> {
        match self.tokens.peek() {
            Some(Token::Ident(id)) => match id.as_str() {
                "i8" => Some(Type::Integer {
                    width: 8,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i16" => Some(Type::Integer {
                    width: 16,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i32" => Some(Type::Integer {
                    width: 32,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i64" => Some(Type::Integer {
                    width: 64,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u8" => Some(Type::Integer {
                    width: 8,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u16" => Some(Type::Integer {
                    width: 16,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u32" => Some(Type::Integer {
                    width: 32,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u64" => Some(Type::Integer {
                    width: 64,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "f32" => Some(Type::Float {
                    width: 32,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "f64" => Some(Type::Float {
                    width: 64,
                    token: self.tokens.next().unwrap().clone(),
                }),
                _ => Some(Type::Ident(self.tokens.next().unwrap().clone())),
            },
            _ => None,
        }
    }

    pub fn precedence_of_operator(&self, operator: &Operator) -> u32 {
        match operator {
            Operator::Equals => 1,
            Operator::Plus => 2,
            Operator::Minus => 2,
            Operator::Multiply => 3,
            Operator::Divide => 3,
            Operator::Exponent => 4,
            Operator::Dot => 5,
            Operator::OpenParen => 6,
            _ => 0, // TODO: error
        }
    }
}
