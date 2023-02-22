use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use linked_hash_map::LinkedHashMap;
use xlang_core::ast::Statement;
use xlang_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use crate::scope::{Scope, ScopeValue};

#[derive(Clone)]
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
        parameters: LinkedHashMap<String, Type>,
        return_parameters: LinkedHashMap<String, Type>,
    },
    String,
    Symbol(Rf<Scope>),
    Ident(String),
    Tuple(Vec<Type>),
    RecordInstance {
        rf: Option<Rf<Scope>>,
        members: LinkedHashMap<String, Type>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Integer {
                    width: l_width,
                    signed: l_signed,
                },
                Self::Integer {
                    width: r_width,
                    signed: r_signed,
                },
            ) => l_width == r_width && l_signed == r_signed,
            (Self::Float { width: l_width }, Self::Float { width: r_width }) => l_width == r_width,
            (
                Self::Function {
                    parameters: l_parameters,
                    return_parameters: l_return_parameters,
                },
                Self::Function {
                    parameters: r_parameters,
                    return_parameters: r_return_parameters,
                },
            ) => l_parameters == r_parameters && l_return_parameters == r_return_parameters,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (
                Self::RecordInstance {
                    rf: l_rf,
                    members: l_members,
                },
                Self::RecordInstance {
                    rf: r_rf,
                    members: r_members,
                },
            ) => l_rf == r_rf && l_members == r_members,
            (Self::RecordInstance { rf: Some(l_rf), .. }, Self::Symbol(sym)) => l_rf == sym,
            (Self::Symbol(sym), Self::RecordInstance { rf: Some(l_rf), .. }) => sym == l_rf,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// impl PartialEq for Type {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Integer { width: l_width, signed: l_signed }, Self::Integer { width: r_width, signed: r_signed }) => l_width == r_width && l_signed == r_signed,
//             (Self::Float { width: l_width }, Self::Float { width: r_width }) => l_width == r_width,
//             (Self::Function { parameters: l_parameters, return_parameters: l_return_parameters }, Self::Function { parameters: r_parameters, return_parameters: r_return_parameters }) => l_parameters == r_parameters && l_return_parameters == r_return_parameters,
//             (Self::Symbol(l0), Self::Symbol(r0)) => Rf::eq(l0, r0),
//             (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
//             (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
//             (Self::RecordInstance { members: l_members }, Self::RecordInstance { members: r_members }) => l_members == r_members,
//             _ => core::mem::discriminant(self) == core::mem::discriminant(other),
//         }
//     }
// }

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::Float { width, .. } => write!(f, "f{width}"),
            Self::Integer {
                width,
                signed: true,
                ..
            } => write!(f, "i{width}"),
            Self::Integer {
                width,
                signed: false,
                ..
            } => write!(f, "u{width}"),
            Self::Empty => f.write_str("()"),
            Self::CoercibleInteger => f.write_str("{integer}"),
            Self::CoercibleFloat => f.write_str("{float}"),
            Self::Function {
                parameters,
                return_parameters,
            } => {
                write!(f, "(")?;
                let mut iter = parameters.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ") -> (")?;
                let mut iter = return_parameters.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ")")
            }
            Self::Symbol(rs) => {
                let rs = rs.borrow();
                if let ScopeValue::Record { ident, members } = &rs.value {
                    write!(f, "{}: (", ident)?;
                    let mut iter = members.iter();

                    if let Some(val) = iter.next() {
                        write!(f, "{} {}", val.1, val.0)?;
                    }
                    for val in iter {
                        write!(f, " ,{} {}", val.1, val.0)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::RecordInstance { members, .. } => {
                write!(f, "(")?;
                let mut iter = members.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ")")
            }
            Self::Ident(i) => f.write_str(i),
            Self::Tuple(ty) => {
                write!(f, "(")?;
                let mut iter = ty.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{}", val)?;
                }
                for val in iter {
                    write!(f, " ,{}", val)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::Symbol { .. } => write!(f, "Symbol"),
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
            Type::RecordInstance { members, .. } => members.len(),
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
            Type::RecordInstance { members, .. } => members.child_at_bx(_index),
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
    String {
        string: String,
    },
    Function {
        rf: Rf<Scope>,
        body: Statement,
    },
    NativeFunction {
        rf: Rf<Scope>,
        callback: Arc<
            dyn Fn(&LinkedHashMap<String, ConstValue>) -> LinkedHashMap<String, ConstValue>
                + Sync
                + Send,
        >,
    },
    Tuple(Vec<ConstValue>),
    RecordInstance {
        rf: Rf<Scope>,
        members: LinkedHashMap<String, ConstValue>,
    },
}

impl Display for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => f.write_str("()"),
            ConstValueKind::Integer { value } => write!(f, "{value}"),
            ConstValueKind::Float { value } => write!(f, "{value}"),
            ConstValueKind::String { string } => write!(f, "{string}"),
            ConstValueKind::Function { body, .. } => write!(f, "{}", body.format()),
            ConstValueKind::NativeFunction { .. } => write!(f, "Native Function"),
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
            ConstValueKind::RecordInstance { members, .. } => {
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

    pub fn as_record_instance(&self) -> (&Rf<Scope>, &LinkedHashMap<String, ConstValue>) {
        match self {
            ConstValueKind::RecordInstance { rf, members } => (rf, members),
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
            ConstValueKind::String { string } => write!(f, "String: {string}"),
            ConstValueKind::Function { .. } => write!(f, "Function"),
            ConstValueKind::NativeFunction { .. } => write!(f, "Native Function"),
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
            ConstValueKind::RecordInstance { members, .. } => members.len(),
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ConstValueKind::Function { body, .. } => match index {
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
            ConstValueKind::RecordInstance { members, .. } => members.child_at_bx(index),
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

    pub fn default_for(ty: &Type) -> ConstValue {
        let kind = match ty {
            Type::Empty => ConstValueKind::Empty,
            Type::Integer { .. } => ConstValueKind::Integer { value: 0 },
            Type::Float { .. } => ConstValueKind::Float { value: 0.0 },
            _ => ConstValueKind::Empty,
        };

        ConstValue {
            ty: ty.clone(),
            kind,
        }
    }

    pub fn string(str: String) -> ConstValue {
        ConstValue {
            ty: Type::String,
            kind: ConstValueKind::String { string: str },
        }
    }

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
        parameters: LinkedHashMap<String, Type>,
        return_parameters: LinkedHashMap<String, Type>,
        node: Rf<Scope>,
    ) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Function { body, rf: node },
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

    pub fn record_instance(
        sym: Rf<Scope>,
        values: LinkedHashMap<String, ConstValue>,
    ) -> ConstValue {
        let types = values.values().map(|val| val.ty.clone());
        let ty = LinkedHashMap::from_iter(values.keys().cloned().zip(types));

        ConstValue {
            ty: Type::RecordInstance {
                rf: Some(sym.clone()),
                members: ty,
            },
            kind: ConstValueKind::RecordInstance {
                rf: sym,
                members: values,
            },
        }
    }

    pub fn try_implicit_cast(&self, ty: &Type) -> Option<ConstValue> {
        match (self, ty) {
            (
                ConstValue {
                    kind: ConstValueKind::Integer { value },
                    ty: Type::CoercibleInteger,
                },
                Type::Integer { width, signed },
            ) => Some(ConstValue::integer(*value, *width, *signed)),
            (
                ConstValue {
                    kind: ConstValueKind::Float { value },
                    ty: Type::CoercibleFloat,
                },
                Type::Float { width },
            ) => Some(ConstValue::float(*value, *width)),
            _ => None,
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
