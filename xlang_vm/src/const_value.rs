use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use xlang_core::ast::Statement;
use xlang_util::format::{NodeDisplay, TreeDisplay};


#[derive(Clone, PartialEq)]
pub enum Type {
    Empty,
    CoercibleInteger,
    CoercibleFloat,
    Integer {
        width: u8,
        signed: bool,
    },
    Float {
        width: u8,
    },
    Function {
        parameters: HashMap<String, Type>,
        return_parameters: HashMap<String, Type>,
    },
    Ident(String),
    Tuple(Vec<Type>),
    RecordInstance {
        members: HashMap<String, Type>,
    },
}

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::RecordInstance { .. } => write!(f, "Record Instance"),
            Self::Tuple(_) => write!(f, "Tuple"),
            Self::Empty => write!(f, "Empty"),
            Self::CoercibleInteger => write!(f, "Coercible Integer"),
            Self::CoercibleFloat => write!(f, "Coercible Float"),
            Self::Float { width, .. } => write!(f, "f{width}"),
            Self::Integer {
                width,
                signed: true,
            } => write!(f, "i{width}"),
            Self::Integer {
                width,
                signed: false,
            } => write!(f, "u{width}"),
            Self::Function { .. } => f.write_str("Function"),
            Self::Ident(ident) => f.write_str(ident),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Type as NodeDisplay>::fmt(self, f)
    }
}

impl TreeDisplay for Type {
    fn num_children(&self) -> usize {
        match self {
            Type::Function { .. } => 2,
            Type::Tuple(tu) => tu.len(),
            Type::RecordInstance { members } => members.len(),
            _ => 0,
        }
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            Type::Function {
                parameters,
                return_parameters,
            } => match _index {
                0 => Some(parameters),
                1 => Some(return_parameters),
                _ => None,
            },
            Type::Tuple(tu) => {
                if let Some(ty) = tu.get(_index) {
                    Some(ty)
                } else {
                    None
                }
            }
            Type::RecordInstance { .. } => None,
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            Type::RecordInstance { members } => members.child_at_bx(_index),
            _ => panic!(),
        }
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
        body: Statement,
    },
    Tuple(Vec<ConstValue>),
    RecordInstance {
        members: HashMap<String, ConstValue>,
    },
}

impl Display for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => f.write_str("()"),
            ConstValueKind::Integer { value } => write!(f, "{value}"),
            ConstValueKind::Float { value } => write!(f, "{value}"),
            ConstValueKind::Function { body } => write!(f, "{}", body.format()),
            ConstValueKind::Tuple(list) => {
                let mut iter = list.iter();
                let Some(item) = iter.next() else {
                    return writeln!(f, "()");
                };
                write!(f, "{}", item.kind)?;
                for item in iter {
                    write!(f, ", {}", item.kind)?;
                }
                Ok(())
            }
            ConstValueKind::RecordInstance { members } => {
                let mut iter = members.iter();
                write!(f, "{{ ")?;
                let Some(item) = iter.next() else {
                    return writeln!(f, "() }}");
                };
                write!(f, "{}: {}", item.0, item.1)?;
                for item in iter {
                    write!(f, ", {}: {}", item.0, item.1)?;
                }
                write!(f, " }}")?;
                Ok(())
            }
        }
    }
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

    pub fn as_record_instance(&self) -> &HashMap<String, ConstValue> {
        match self {
            ConstValueKind::RecordInstance { members } => members,
            _ => panic!(),
        }
    }

    pub fn as_empty(&self) -> bool {
        matches!(self, ConstValueKind::Empty)
    }
}

impl NodeDisplay for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => write!(f, "Empty"),
            ConstValueKind::Integer { value } => write!(f, "Integer: {value}"),
            ConstValueKind::Float { value } => write!(f, "Float: {value}"),
            ConstValueKind::Function { .. } => write!(f, "Function"),
            ConstValueKind::Tuple(_) => write!(f, "Tuple"),
            ConstValueKind::RecordInstance { .. } => write!(f, "Record Instance"),
        }
    }
}

impl TreeDisplay for ConstValueKind {
    fn num_children(&self) -> usize {
        match self {
            ConstValueKind::Function { .. } => 1,
            ConstValueKind::Tuple(list) => list.len(),
            ConstValueKind::RecordInstance { members } => members.len(),
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ConstValueKind::Function { body } => match index {
                0 => Some(body),
                _ => None,
            },
            ConstValueKind::Tuple(tu) => {
                if let Some(val) = tu.get(index) {
                    Some(val)
                } else {
                    None
                }
            }
            ConstValueKind::RecordInstance { .. } => None,
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            ConstValueKind::RecordInstance { members } => members.child_at_bx(index),
            _ => panic!(),
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

    pub fn default_for(ty: Type) -> ConstValue {
        let kind = match ty {
            Type::Empty => ConstValueKind::Empty,
            Type::Integer { .. } => ConstValueKind::Integer { value: 0 },
            Type::Float { .. } => ConstValueKind::Float { value: 0.0 },
            _ => ConstValueKind::Empty,
        };

        ConstValue { ty, kind }
    }

    // pub fn scope_ref(scope: &ScopeManager, name: &str) -> ConstValue {
    //     ConstValue {
    //         ty: Type::Empty,
    //         kind: ConstValueKind::Ref {
    //             scope_ref: scope.get_symbol_ref(name).unwrap(),
    //             member: None,
    //         },
    //     }
    // }

    // pub fn member_ref(scope: &ScopeManager, name: &str, member: &str) -> ConstValue {
    //     ConstValue {
    //         ty: Type::Empty,
    //         kind: ConstValueKind::Ref {
    //             scope_ref: scope.get_symbol_ref(name).unwrap(),
    //             member: Some(member.to_string()),
    //         },
    //     }
    // }

    pub fn integer(value: u64, width: u8, signed: bool) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Integer { value },
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
            kind: ConstValueKind::Float { value },
            ty: Type::Float { width },
        }
    }

    pub fn cfloat(value: f64) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Float { value },
            ty: Type::CoercibleFloat,
        }
    }

    pub fn func(
        body: Statement,
        parameters: HashMap<String, Type>,
        return_parameters: HashMap<String, Type>,
    ) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Function { body },
            ty: Type::Function {
                parameters,
                return_parameters,
            },
        }
    }

    pub fn tuple(values: Vec<ConstValue>) -> ConstValue {
        let types: Vec<_> = values.iter().map(|val| val.ty.clone()).collect();
        ConstValue {
            kind: ConstValueKind::Tuple(values),
            ty: Type::Tuple(types),
        }
    }

    pub fn record_instance(values: HashMap<String, ConstValue>) -> ConstValue {
        let types = values.values().map(|val| val.ty.clone());
        let ty = HashMap::from_iter(values.keys().cloned().zip(types));

        ConstValue {
            ty: Type::RecordInstance { members: ty },
            kind: ConstValueKind::RecordInstance { members: values },
        }
    }
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
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
