use std::fmt::Debug;

use xlang_util::format::{NodeDisplay, StringFormatter, TreeDisplay};

use crate::token::{Range, SpannedToken, Token, Unit};

pub trait AstNode: TreeDisplay {
    fn get_range(&self) -> Range;
}

macro_rules! addup {
    ($($e:expr),*) => {{
        $((if let Some(_) = $e { 1 } else { 0 })+)* 0
    }};
}

macro_rules! switchon {
    ($index:expr, $($e:expr),*) => {{
        let mut ind = 0;
        $(if let Some(v) = $e {
            if $index == ind {
                return Some(v)
            }
            ind += 1;
        })*
        ind
    }};
}

impl AstNode for SpannedToken {
    fn get_range(&self) -> Range {
        self.0.into()
    }
}

#[derive(Clone)]
pub struct PunctuationList<T: AstNode> {
    tokens: Vec<(T, Option<SpannedToken>)>,
}

impl<T: AstNode> PunctuationList<T> {
    pub fn new() -> PunctuationList<T> {
        PunctuationList { tokens: Vec::new() }
    }

    pub fn push(&mut self, val: T, separator: Option<SpannedToken>) {
        self.tokens.push((val, separator))
    }

    pub fn push_sep(&mut self, val: T, separator: SpannedToken) {
        self.tokens.push((val, Some(separator)))
    }

    pub fn push_term(&mut self, val: T) {
        self.tokens.push((val, None))
    }

    pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
        self.tokens.iter().map(|(v, _)| v)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(T, Option<SpannedToken>)> + '_ {
        self.tokens.iter()
    }
}

impl<T> AstNode for PunctuationList<T>
where
    T: AstNode,
{
    fn get_range(&self) -> Range {
        match (self.iter().next(), self.iter().last()) {
            (Some((_, Some(f))), Some((_, Some(l)))) => Range::from((*f.span(), *l.span())),
            (Some((_, Some(f))), Some((l, _))) => Range::from((*f.span(), &l.get_range())),
            (Some((f, _)), Some((_, Some(l)))) => Range::from((&f.get_range(), *l.span())),
            (Some((f, _)), Some((l, _))) => Range::from((&f.get_range(), &l.get_range())),
            _ => Range::default(),
        }
    }
}

impl<T> NodeDisplay for PunctuationList<T>
where
    T: NodeDisplay + AstNode,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Punctuation List")?;
        write!(f, " {}", self.tokens.len())
    }
}

impl<T> TreeDisplay for PunctuationList<T>
where
    T: TreeDisplay + AstNode,
{
    fn num_children(&self) -> usize {
        if let Some((_, Some(_))) = self.tokens.last() {
            self.tokens.len() * 2
        } else if self.tokens.len() > 0 {
            self.tokens.len() * 2 - 1
        } else {
            0
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        let p = &self.tokens[index / 2];
        if index % 2 == 0 {
            Some(&p.0)
        } else {
            // Some(p.1.as_ref().unwrap())
            None
        }
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        let sz = if let Some((_, Some(_))) = self.tokens.last() {
            self.tokens.len() * 2
        } else if self.tokens.len() > 0 {
            self.tokens.len() * 2 - 1
        } else {
            0
        };
        Box::new(StringFormatter(format!(
            "Index::::::{} {} {}",
            _index,
            sz,
            self.tokens.len()
        )))
    }
}

#[derive(Clone)]
pub struct ParamaterList {
    pub range: Range,
    pub items: PunctuationList<Param>,
}

impl AstNode for ParamaterList {
    fn get_range(&self) -> Range {
        self.range
    }
}

impl ParamaterList {
    pub fn iter_items(&self) -> impl Iterator<Item = &Param> + '_ {
        self.items.iter_items()
    }
}

impl NodeDisplay for ParamaterList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Element Parameters")
    }
}

impl TreeDisplay for ParamaterList {
    fn num_children(&self) -> usize {
        2
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        match index {
            0 => Some(&self.range),
            1 => Some(&self.items),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct Param {
    pub ty: Option<Type>,
    pub name: Option<SpannedToken>,
}

impl AstNode for Param {
    fn get_range(&self) -> Range {
        match (&self.name, &self.ty) {
            (Some(name), None) => Range::from(*name.span()),
            (Some(name), Some(value)) => Range::from((name, &value.get_range())),
            _ => Range::default(),
        }
    }
}

impl Param {
    pub fn name(&self) -> &String {
        match &self.name {
            Some(SpannedToken(_, Token::Ident(s))) => s,
            _ => panic!(),
        }
    }
}

impl NodeDisplay for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Parameter")
    }
}

impl TreeDisplay for Param {
    fn num_children(&self) -> usize {
        addup!(self.ty, self.name)
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        switchon!(index, &self.ty, &self.name);
        None
    }
}

#[derive(Clone)]
pub struct ArgList {
    pub range: Range,
    pub items: PunctuationList<Expression>,
}

impl AstNode for ArgList {
    fn get_range(&self) -> Range {
        self.range
    }
}

impl ArgList {
    pub fn iter_items(&self) -> impl Iterator<Item = &Expression> + '_ {
        self.items.iter_items()
    }
}

impl NodeDisplay for ArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Arg Parameters")
    }
}

impl TreeDisplay for ArgList {
    fn num_children(&self) -> usize {
        2
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        match index {
            0 => Some(&self.range),
            1 => Some(&self.items),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub enum Type {
    Integer {
        width: u8,
        signed: bool,
        token: SpannedToken,
    },
    Float {
        width: u8,
        token: SpannedToken,
    },
    Ident(SpannedToken),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Integer {
                    width: l_width,
                    signed: l_signed,
                    ..
                },
                Self::Integer {
                    width: r_width,
                    signed: r_signed,
                    ..
                },
            ) => l_width == r_width && l_signed == r_signed,
            (Self::Float { width: l_width, .. }, Self::Float { width: r_width, .. }) => {
                l_width == r_width
            }
            (
                Self::Ident(SpannedToken(_, Token::Ident(a))),
                Self::Ident(SpannedToken(_, Token::Ident(b))),
            ) => a == b,
            _ => false,
        }
    }
}

impl AstNode for Type {
    fn get_range(&self) -> Range {
        match self {
            Self::Integer { token, .. } => token.span().into(),
            Self::Float { token, .. } => token.span().into(),
            Self::Ident(ident) => ident.span().into(),
        }
    }
}

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Float { width, .. } => write!(f, "f{}", width),
            Self::Integer {
                width,
                signed: true,
                ..
            } => write!(f, "i{}", width),
            Self::Integer {
                width,
                signed: false,
                ..
            } => write!(f, "u{}", width),
            Self::Ident(ident) => <SpannedToken as NodeDisplay>::fmt(ident, f),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Type as NodeDisplay>::fmt(&self, f)
    }
}

impl TreeDisplay for Type {
    fn num_children(&self) -> usize {
        0
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        None
    }
}

#[derive(Clone)]
pub enum Expression {
    BinaryExpression {
        left: Option<Box<Expression>>,
        right: Option<Box<Expression>>,
        op_token: Option<SpannedToken>,
    },
    Integer(u64, Option<Unit>, SpannedToken),
    Float(f64, Option<Unit>, SpannedToken),
    Ident(SpannedToken),
    FunctionCall {
        expr: Box<Expression>,
        args: ArgList,
    },
    Tuple(Vec<Expression>),
    Array {
        values: PunctuationList<Expression>,
        range: Range,
    },
    Function {
        parameters: ParamaterList,
        arrow: SpannedToken,
        return_parameters: ParamaterList,
        comma: Option<SpannedToken>,
        body: Option<Box<Statement>>,
    },
    Record {
        parameters: ParamaterList,
    },
}

impl Expression {
    pub fn as_function(&self) -> Option<(&Expression, &ArgList)> {
        match self {
            Expression::FunctionCall { expr, args } => return Some((expr, args)),
            _ => None,
        }
    }
}

impl AstNode for Expression {
    fn get_range(&self) -> Range {
        match self {
            Self::Record { parameters } => parameters.get_range(),
            Self::Function {
                parameters,
                body: Some(body),
                ..
            } => Range::from((&parameters.get_range(), &body.get_range())),
            Self::Function {
                parameters,
                comma: Some(comma),
                ..
            } => Range::from((&parameters.get_range(), *comma.span())),
            Self::Function {
                parameters,
                return_parameters,
                ..
            } => Range::from((&parameters.get_range(), &return_parameters.get_range())),
            Self::BinaryExpression {
                left: Some(left),
                right: Some(right),
                ..
            } => Range::from((&left.get_range(), &right.get_range())),
            Self::BinaryExpression {
                left: Some(left),
                op_token: Some(op),
                ..
            } => Range::from((&left.get_range(), *op.span())),
            Self::BinaryExpression {
                op_token: Some(op),
                right: Some(right),
                ..
            } => Range::from((*op.span(), &right.get_range())),
            Self::BinaryExpression {
                op_token: Some(op), ..
            } => Range::from(*op.span()),
            Self::Tuple(s) => match (s.first(), s.last()) {
                (Some(s), Some(e)) => Range::from((&s.get_range(), &e.get_range())),
                _ => Range::default(),
            },
            Self::Array { range, .. } => range.clone(),
            Self::Integer(_, _, s) => s.0.into(),
            Self::Float(_, _, s) => s.0.into(),
            Self::Ident(s) => s.0.into(),
            Self::FunctionCall { expr, args } => {
                Range::from((&expr.get_range(), &args.get_range()))
            }
            _ => Range::default(),
        }
    }
}

impl NodeDisplay for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Function { .. } => f.write_str("Function"),
            Self::Record { .. } => f.write_str("Record"),
            Self::BinaryExpression {
                op_token: Some(SpannedToken(_, Token::Operator(op))),
                ..
            } => write!(f, "BinExp {}", op.as_str()),
            Self::BinaryExpression { .. } => write!(f, "BinExp"),
            Self::Integer(i, Some(u), _) => write!(f, "{}{}", i, u),
            Self::Float(i, Some(u), _) => write!(f, "{}{}", i, u),
            Self::Integer(i, None, _) => write!(f, "{}", i),
            Self::Float(i, None, _) => write!(f, "{}", i),
            Self::Ident(SpannedToken(_, Token::Ident(i))) => write!(f, "{}", i),
            Self::FunctionCall { .. } => write!(f, "FunctionCall"),
            Self::Array { .. } => f.write_str("Array"),
            _ => panic!(),
        }
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Expression as NodeDisplay>::fmt(&self, f)
    }
}

impl TreeDisplay for Expression {
    fn num_children(&self) -> usize {
        match self {
            Self::Record { .. } => 1,
            Self::Function { body: Some(_), .. } => 3,
            Self::Function { .. } => 2,
            Self::FunctionCall { .. } => 2,
            Self::Array { values, .. } => values.num_children(),
            Self::BinaryExpression {
                left: Some(_),
                right: Some(_),
                ..
            } => 2,
            Self::BinaryExpression { left: Some(_), .. } => 1,
            Self::BinaryExpression { right: Some(_), .. } => 1,
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            Self::Record { parameters } => Some(parameters),
            Self::Function {
                parameters,
                return_parameters,
                body: Some(body),
                ..
            } => match index {
                0 => Some(parameters),
                1 => Some(return_parameters),
                2 => Some(&**body),
                _ => None,
            },
            Self::Function {
                parameters,
                return_parameters,
                ..
            } => match index {
                0 => Some(parameters),
                1 => Some(return_parameters),
                _ => None,
            },

            Self::FunctionCall { expr, args, .. } => match index {
                0 => Some(&**expr),
                1 => Some(args),
                _ => None,
            },
            Self::Array { values, .. } => values.child_at(index),
            Self::BinaryExpression {
                left: Some(l),
                right: Some(r),
                ..
            } => match index {
                0 => Some(&**l),
                1 => Some(&**r),
                _ => None,
            },
            Self::BinaryExpression { left: Some(l), .. } => Some(&**l),
            Self::BinaryExpression { right: Some(r), .. } => Some(&**r),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum Statement {
    Expression(Expression),
    Decleration {
        ident: SpannedToken,
        colon: SpannedToken,
        expr: Option<Expression>,
    },
    UseStatement {
        token: Option<SpannedToken>,
        args: PunctuationList<SpannedToken>,
    },
    List(PunctuationList<Statement>),
}

impl AstNode for Statement {
    fn get_range(&self) -> Range {
        match self {
            Self::Expression(e) => e.get_range(),
            Self::UseStatement {
                token: Some(token),
                args,
            } => match args.iter().last() {
                Some((_, Some(tok))) => Range::from((*token.span(), *tok.span())),
                Some((tok, _)) => Range::from((*token.span(), *tok.span())),
                _ => Range::from(*token.span()),
            },
            Self::List(list) => list.get_range(),
            _ => Range::default(),
        }
    }
}

impl NodeDisplay for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Statement")
    }
}

impl TreeDisplay for Statement {
    fn num_children(&self) -> usize {
        match self {
            Self::Decleration { .. } => 2,
            Self::UseStatement { token, args } => addup!(token) + args.num_children(), // Self::Expression(_) => 1,
            Self::Expression(_) => 1,
            Self::List(list) => list.num_children(),
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            Self::Decleration {
                ident,
                expr: Some(expr),
                ..
            } => match index {
                0 => Some(ident),
                1 => Some(expr),
                _ => None,
            },
            Self::Decleration { ident, .. } => Some(ident),
            Self::UseStatement { token, args } => {
                let ind = switchon!(index, token);
                args.child_at(index - ind)
            }
            Self::Expression(e) => Some(e),
            Self::List(list) => list.child_at(index),
        }
    }

    fn child_at_bx<'b>(&'b self, index: usize) -> Box<dyn TreeDisplay + 'b> {
        match self {
            Self::Expression(e) => e.child_at_bx(index),
            _ => panic!(),
        }
    }
}
