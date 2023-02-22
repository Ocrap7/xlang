use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::{
    ast::{ArgList, Param, ParamaterList, PunctuationList, Statement},
    error::{ParseError, ParseErrorKind},
    token::{Operator, Range, SpannedToken, Token, TokenIndex, TokenStream},
};

pub struct Parser {
    pub(crate) tokens: TokenStream,
    pub(crate) errors: RwLock<Vec<ParseError>>,
}

impl Parser {
    pub fn new(token_stream: impl Into<TokenStream>) -> Self {
        Self {
            tokens: token_stream.into(),
            errors: RwLock::new(Vec::new()),
        }
    }

    pub fn get_errors(&self) -> RwLockReadGuard<'_, Vec<ParseError>> {
        self.errors.read().unwrap()
    }

    pub fn get_errors_mut(&self) -> RwLockWriteGuard<'_, Vec<ParseError>> {
        self.errors.write().unwrap()
    }

    pub fn add_error(&self, error: ParseError) {
        let p = &mut *self.errors.write().unwrap();
        p.push(error);
    }

    pub fn parse(&self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();
        self.ignore_ws();
        while let Some(stmt) = self.parse_statement() {
            statements.push(stmt);

            if let Some(Token::Newline) = self.tokens.peek() {
                self.tokens.next();
            }
            self.ignore_ws();
        }

        Some(statements)
    }

    pub fn parse_statement(&self) -> Option<Statement> {
        match self.tokens.peek() {
            Some(Token::Ident(s)) if s == "use" => {
                if let Some(us) = self.parse_use() {
                    return Some(us);
                }
            }
            Some(Token::Ident(_)) => {
                if let Some(decl) = self.parse_decleration() {
                    return Some(decl);
                }
            }
            _ => (),
        };

        let expression = self.parse_expression(0);
        if expression.is_some() {
            return expression.map(Statement::Expression);
        }

        None
    }

    pub fn parse_decleration(&self) -> Option<Statement> {
        let ident = match self.tokens.next() {
            Some(tok @ SpannedToken(_, Token::Ident(_))) => tok.clone(),
            _ => {
                self.tokens.back();
                return None;
            }
        };
        let Some(colon) = self.expect_operator(Operator::Colon).cloned() else {
            self.tokens.back();
            return None;
        };
        let expr = self.parse_expression(0);

        Some(Statement::Decleration { ident, colon, expr })
    }

    pub fn parse_use(&self) -> Option<Statement> {
        let token = self.tokens.next();
        let mut args = PunctuationList::default();
        let mut last_line = token.map(|l| l.span().line_num);
        while let Some(Token::Ident(_)) = self.tokens.peek() {
            let tok = self.tokens.next();

            match (self.tokens.peek(), tok) {
                (Some(Token::Operator(Operator::Dot)), Some(id)) => {
                    let dot = self.tokens.next();
                    args.push(id.clone(), dot.cloned());
                }
                (_, Some(id)) => {
                    let lline = *last_line.get_or_insert(id.span().line_num);
                    if lline == id.span().line_num {
                        args.push(id.clone(), None);
                    } else {
                        self.tokens.back();
                    }
                    break;
                }
                _ => break,
            }
        }
        Some(Statement::UseStatement {
            token: token.cloned(),
            args,
        })
    }

    pub fn parse_parameters(&self) -> Option<ParamaterList> {
        let open = self.expect_operator(Operator::OpenParen);

        let args = match self.tokens.peek() {
            Some(Token::Operator(Operator::CloseParen)) => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some(arg) = self.parse_parameter() {
                    if arg.name.is_none() && arg.ty.is_none() {
                        return None;
                    }
                    let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                        self.tokens.next().cloned()
                    } else {
                        None
                    };
                    if let Some(Token::Operator(Operator::CloseParen)) = self.tokens.peek() {
                        args.push(arg, comma);
                        break;
                    }
                    if comma.is_none() {
                        self.add_error(ParseError {
                            kind: ParseErrorKind::InvalidSyntax(
                                "Expected comma in arguments!".to_string(),
                            ),
                            range: Range::default(),
                        });
                    }
                    args.push(arg, comma);
                }
                args
            }
        };

        let close = self.expect_operator(Operator::CloseParen);

        if let (Some(open), Some(close)) = (open, close) {
            Some(ParamaterList {
                items: args,
                range: Range {
                    start: open.0,
                    end: close.0,
                },
            })
        } else {
            self.add_error(ParseError {
                kind: ParseErrorKind::InvalidSyntax("Unable to parse arg brackets!".to_string()),
                range: Range::default(),
            });
            Some(ParamaterList {
                items: args,
                range: Range::default(),
            })
        }
    }

    fn parse_parameter(&self) -> Option<Param> {
        let ty = self.parse_type();
        let ident = self.expect(Token::Ident("".into()));

        match (ident, ty) {
            (Some(ident), Some(ty)) => Some(Param {
                ty: Some(ty),
                name: Some(ident.clone()),
            }),
            (ident, ty) => {
                self.add_error(ParseError {
                    kind: ParseErrorKind::InvalidSyntax("Unable to parse arg fields!".to_string()),
                    range: Range::default(),
                });
                Some(Param {
                    ty,
                    name: ident.cloned(),
                })
            }
        }
    }

    pub fn parse_arguments(&self) -> Option<ArgList> {
        let open = self.expect_operator(Operator::OpenParen);

        let args = match self.tokens.peek() {
            Some(Token::Operator(Operator::CloseParen)) => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some(arg) = self.parse_expression(0) {
                    let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                        self.tokens.next().cloned()
                    } else {
                        None
                    };
                    if let Some(Token::Operator(Operator::CloseParen)) = self.tokens.peek() {
                        args.push(arg, comma);
                        break;
                    }
                    if comma.is_none() {
                        self.add_error(ParseError {
                            kind: ParseErrorKind::InvalidSyntax(
                                "Expected comma in arguments!".to_string(),
                            ),
                            range: Range::default(),
                        });
                    }
                    args.push_sep(arg, comma.unwrap());
                }
                args
            }
        };

        let close = self.expect_operator(Operator::CloseParen);

        if let (Some(open), Some(close)) = (open, close) {
            Some(ArgList {
                items: args,
                range: Range {
                    start: open.0,
                    end: close.0,
                },
            })
        } else {
            self.add_error(ParseError {
                kind: ParseErrorKind::InvalidSyntax("Unable to parse arg brackets!".to_string()),
                range: Range::default(),
            });
            Some(ArgList {
                items: args,
                range: Range::default(),
            })
        }
    }

    // fn parse_expression(&self) -> Option<Expression> {
    //     self.parse_literal()
    // }

    // fn parse_literal(&self) -> Option<Expression> {
    //     match self.tokens.peek() {
    //         Some(Token::Ident(_)) => Some(Expression::Ident(self.tokens.next().cloned().unwrap())),
    //         _ => None,
    //     }
    // }

    pub(crate) fn expect_operator(&self, operator: Operator) -> Option<&SpannedToken> {
        self.ignore_ws();
        let Some(Token::Operator(o)) = self.tokens.peek() else {
            return None;
        };

        if o == &operator {
            return self.tokens.next();
        }

        None
    }

    pub fn ignore_ws(&self) {
        while let Some(Token::Newline) = self.tokens.peek() {
            self.tokens.next();
        }
    }

    pub fn save_state(&self) -> TokenIndex {
        self.tokens.get_index()
    }

    pub(crate) fn expect(&self, token_type: Token) -> Option<&SpannedToken> {
        self.ignore_ws();
        let Some(tok) = self.tokens.peek() else {
            return None;
        };
        if std::mem::discriminant(tok) == std::mem::discriminant(&token_type) {
            return self.tokens.next();
        }

        None
    }
}
