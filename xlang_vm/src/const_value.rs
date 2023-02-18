use std::fmt::{Debug, Write};

use xlang_core::ast::Statement;
use xlang_util::format::{NodeDisplay, TreeDisplay};

#[derive(Clone)]
pub enum Type {
    Empty,
    CoercibleInteger,
    CoercibleFloat,
    Integer { width: u8, signed: bool },
    Float { width: u8 },
    Ident(String),
}

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::CoercibleInteger => write!(f, "Coercible Integer"),
            Self::CoercibleFloat => write!(f, "Coercible Float"),
            Self::Float { width, .. } => write!(f, "f{}", width),
            Self::Integer {
                width,
                signed: true,
            } => write!(f, "i{}", width),
            Self::Integer {
                width,
                signed: false,
            } => write!(f, "u{}", width),
            Self::Ident(ident) => f.write_str(ident),
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
pub enum ConstValueKind {
    Empty,
    Integer {
        value: u64,
    },
    Float {
        value: f64,
    },
    Function {
        parameters: Vec<ConstValue>,
        return_parameters: Vec<ConstValue>,
        body: Statement,
    },
}

impl ConstValueKind {
    pub fn as_integer(&self) -> u64 {
        match self {
            ConstValueKind::Integer { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            ConstValueKind::Float { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_empty(&self) -> bool {
        match self {
            ConstValueKind::Empty => true,
            _ => false,
        }
    }
}

impl NodeDisplay for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => write!(f, "Empty"),
            ConstValueKind::Integer { value } => write!(f, "Integer: {}", value),
            ConstValueKind::Float { value } => write!(f, "Float: {}", value),
            ConstValueKind::Function { .. } => write!(f, "Function"),
        }
    }
}

impl TreeDisplay for ConstValueKind {
    fn num_children(&self) -> usize {
        match self {
            ConstValueKind::Function { .. } => 3,
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ConstValueKind::Function {
                parameters,
                return_parameters,
                body,
            } => match index {
                0 => Some(parameters),
                1 => Some(return_parameters),
                2 => Some(body),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct ConstValue {
    pub ty: Type,
    pub kind: ConstValueKind,
}

impl ConstValue {
    pub fn empty() -> ConstValue {
        ConstValue {
            ty: Type::Empty,
            kind: ConstValueKind::Empty,
        }
    }

    pub fn integer(value: u64, width: u8, signed: bool) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Integer { value: value },
            ty: Type::Integer { width, signed },
        }
    }

    pub fn cinteger(value: u64) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Integer { value },
            ty: Type::CoercibleInteger,
        }
    }

    pub fn float(value: f64, width: u8) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Float { value: value },
            ty: Type::Float { width },
        }
    }

    pub fn cfloat(value: f64) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Float { value },
            ty: Type::CoercibleFloat,
        }
    }
}

impl NodeDisplay for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Const Value")
    }
}

impl TreeDisplay for ConstValue {
    fn num_children(&self) -> usize {
        2
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match index {
            0 => Some(&self.ty),
            1 => Some(&self.kind),
            _ => None,
        }
    }
}
