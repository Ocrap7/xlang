use crate::token::{Operator, Span, SpannedToken, Token};

pub struct Lexer {}

impl Lexer {
    pub fn lex(&self, input: &str) -> Vec<SpannedToken> {
        let mut start_index = 0;
        let mut end_index = 1;

        let mut line_num = 0;
        let mut position = 0;

        let mut tokens: Vec<SpannedToken> = Vec::new();
        let mut string = false;

        while start_index < input.len() && end_index <= input.len() {
            let sub_str = &input[start_index..end_index];
            let next = input.chars().nth(end_index);

            if let Some(token) = self.try_lex(sub_str, next, string) {
                match token {
                    Token::Operator(Operator::Quote) => string = true,
                    Token::String => {
                        let tok = self.parse_template(
                            &sub_str[0..sub_str.len() - 1],
                            line_num,
                            position,
                            tokens.len() as _,
                        );

                        let token = SpannedToken::new(tok.1, tok.0);

                        position += token.0.length;
                        tokens.push(token);

                        string = false
                    }
                    Token::Whitespace => position += 1,
                    Token::Newline => {
                        let _ce = end_index - 1;
                        if &input[start_index..end_index + 1] == "\r\n" {
                            end_index += 1;
                        }

                        line_num += 1;
                        position = 0;
                    }
                    Token::Ident(_) => {
                        let token = SpannedToken::new(
                            token,
                            Span {
                                line_num,
                                position,
                                length: (end_index - start_index) as u32,
                                token_index: tokens.len() as u32,
                            },
                        );

                        tokens.push(token);
                        position += (end_index - start_index) as u32;
                    }
                    token => {
                        let token = SpannedToken::new(
                            token,
                            Span {
                                line_num,
                                position,
                                length: (end_index - start_index) as u32,
                                token_index: tokens.len() as u32,
                            },
                        );

                        tokens.push(token);
                        position += (end_index - start_index) as u32;
                    }
                }

                start_index = end_index;
                end_index = start_index + 1;
            } else {
                end_index += 1;
            }
        }

        tokens.push(SpannedToken::new(
            Token::Newline,
            Span {
                line_num,
                position,
                length: 1,
                token_index: tokens.len() as u32,
            },
        ));

        tokens
    }

    pub fn try_lex(&self, input: &str, next: Option<char>, string: bool) -> Option<Token> {
        if string {
            if let Some('"') = input.chars().last() {
                return Some(Token::String);
            }

            return None;
        }

        if input.len() == 1 {
            // match single character symbols
            match input.chars().next() {
                Some('[') => return Some(Token::Operator(Operator::OpenSquare)),
                Some(']') => return Some(Token::Operator(Operator::CloseSquare)),
                Some('(') => return Some(Token::Operator(Operator::OpenParen)),
                Some(')') => return Some(Token::Operator(Operator::CloseParen)),
                Some('{') => return Some(Token::Operator(Operator::OpenBrace)),
                Some('}') => return Some(Token::Operator(Operator::CloseBrace)),
                Some('"') => return Some(Token::Operator(Operator::Quote)),

                Some(':') => return Some(Token::Operator(Operator::Colon)),
                Some('.') => return Some(Token::Operator(Operator::Dot)),
                Some(',') => return Some(Token::Operator(Operator::Comma)),

                Some('+') => return Some(Token::Operator(Operator::Plus)),
                Some('-') => match next {
                    Some('>') => return None,
                    _ => return Some(Token::Operator(Operator::Minus)),
                },
                Some('*') => match next {
                    Some('*') => return None,
                    _ => return Some(Token::Operator(Operator::Multiply)),
                },
                Some('/') => return Some(Token::Operator(Operator::Divide)),
                Some('=') => return Some(Token::Operator(Operator::Equals)),

                Some('\r' | '\n') => return Some(Token::Newline),
                Some(c) if c.is_whitespace() => return Some(Token::Whitespace),
                _ => (),
            }
        }

        match (input.chars().next(), input.chars().nth(1)) {
            (Some('*'), Some('*')) => return Some(Token::Operator(Operator::Exponent)),
            (Some('-'), Some('>')) => return Some(Token::Operator(Operator::Arrow)),
            _ => (),
        }

        let del = next.map(|c| !(c.is_numeric() || c == '.')).unwrap_or(true);

        let cnt = input
            .chars()
            .fold(0u8, |acc, c| if c == '.' { 1 + acc } else { acc });
        if !input.chars().any(|c| !(c.is_numeric() || c == '.')) && cnt <= 1 && del {
            if cnt == 1 {
                let val = input.parse().unwrap_or(0.0f64);
                return Some(Token::Float(val));
            } else {
                let val = input.parse().unwrap_or(0u64);
                return Some(Token::Integer(val));
            }
        }

        // If the next character is a delimeter
        let del = next
            .map(|c| !(c.is_alphanumeric() || c == '_'))
            .unwrap_or(true);

        // match identifiers
        if input
            .chars()
            .next()
            .filter(|f| f.is_alphabetic() || *f == '_')
            .is_some()
            & !input.chars().any(|c| !(c.is_alphanumeric() || c == '_'))
            && del
        {
            return Some(Token::Ident(input.to_string()));
        }

        None
    }

    pub fn parse_template(
        &self,
        input: &str,
        line: u32,
        mut position: u32,
        token_index: u32,
    ) -> (Span, Token) {
        let mut iindex = 0;
        let mut last_index = 0;
        let mut last_span = Span::default();
        let mut open = 0;

        let mut toks = Vec::new();
        let sindex = position + 1;

        for i in 0..input.len() {
            let c = input.chars().nth(i).unwrap();

            if c == '}' {
                open -= 1;
            }

            if c == '{' && open == 0 {
                let st = input[last_index..i].to_string();
                let len = st.len();
                last_span = Span {
                    line_num: line,
                    position: position + len as u32,
                    length: 1,
                    token_index,
                };

                let token = SpannedToken::new(
                    Token::Ident(st),
                    Span {
                        line_num: line,
                        position: position + 1,
                        length: len as u32,
                        token_index,
                    },
                );

                position += len as u32 + 1;

                toks.push(Template::String(token));
                iindex = i;
                open += 1
            } else if c == '}' && open == 0 {
                let st = &input[iindex + 1..i];
                let utoks = self.lex(st);
                let tok_len = utoks.len();
                let p = utoks
                    .into_iter()
                    .map(|f| {
                        SpannedToken::new(
                            f.1,
                            Span {
                                line_num: f.0.line_num + line,
                                position: f.0.position + position + 1, // + 2 for { and "
                                length: f.0.length,
                                token_index,
                            },
                        )
                    })
                    .take(tok_len)
                    .collect::<Vec<_>>();

                last_index = i + 1;
                position += st.len() as u32 + 2;

                toks.push(Template::Template(
                    p,
                    SpannedToken::new(Token::Operator(Operator::OpenBrace), last_span),
                    SpannedToken::new(
                        Token::Operator(Operator::CloseBrace),
                        Span {
                            line_num: line,
                            position: position - 1,
                            length: 1,
                            token_index,
                        },
                    ),
                ));
            }
        }

        {
            let st = input[last_index..input.len()].to_string();
            let len = st.len();

            let token = SpannedToken::new(
                Token::Ident(st),
                Span {
                    line_num: line,
                    position,
                    length: len as u32,
                    token_index,
                },
            );

            toks.push(Template::String(token));
        }

        (
            Span {
                length: input.len() as u32 + 2,
                line_num: line,
                position: sindex - 1,
                token_index,
            },
            Token::TemplateString(toks),
        )
    }
}

#[derive(Clone, Debug)]
pub enum Template {
    String(SpannedToken),
    Template(Vec<SpannedToken>, SpannedToken, SpannedToken),
}
