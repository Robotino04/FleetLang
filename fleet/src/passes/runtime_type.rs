use std::fmt::Display;

use itertools::{EitherOrBoth, Itertools};

use crate::{
    passes::union_find_set::{UnionFindSet, UnionFindSetMergeResult, UnionFindSetPtr},
    tokenizer::NamedSourceRange,
};

#[derive(Clone, Debug)]
pub struct RuntimeType {
    pub kind: RuntimeTypeKind,
    pub definition_range: Option<NamedSourceRange>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeTypeKind {
    Number {
        signed: Option<bool>,
        integer: Option<bool>,
    },
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Boolean,
    Unit,
    Unknown,
    Error,
    ArrayOf {
        subtype: UnionFindSetPtr<RuntimeType>,
        size: Option<usize>,
    },
    Struct {
        members: Vec<(String, NamedSourceRange, UnionFindSetPtr<RuntimeType>)>,
        /// indicates which struct{} type definition this one originated from
        source_hash: Option<u64>,
    },
}

impl RuntimeTypeKind {
    pub fn unwrap_arrayof(&self) -> (UnionFindSetPtr<RuntimeType>, Option<usize>) {
        if let Self::ArrayOf { subtype, size } = self {
            (*subtype, *size)
        } else {
            panic!("Expected RuntimeType::ArrayOf, but got {self:?}");
        }
    }

    pub fn stringify(&self, types: &UnionFindSet<RuntimeType>) -> String {
        match self {
            RuntimeTypeKind::Number { signed, integer } => format!(
                "{{{}{}}}",
                match signed {
                    Some(true) => "signed ",
                    Some(false) => "unsigned ",
                    None => "",
                },
                match integer {
                    Some(true) => "integer",
                    Some(false) => "float",
                    None => "number",
                }
            ),
            RuntimeTypeKind::I8 => "i8".to_string(),
            RuntimeTypeKind::I16 => "i16".to_string(),
            RuntimeTypeKind::I32 => "i32".to_string(),
            RuntimeTypeKind::I64 => "i64".to_string(),
            RuntimeTypeKind::U8 => "u8".to_string(),
            RuntimeTypeKind::U16 => "u16".to_string(),
            RuntimeTypeKind::U32 => "u32".to_string(),
            RuntimeTypeKind::U64 => "u64".to_string(),
            RuntimeTypeKind::F32 => "f32".to_string(),
            RuntimeTypeKind::F64 => "f64".to_string(),
            RuntimeTypeKind::Unit => "()".to_string(),
            RuntimeTypeKind::Boolean => "bool".to_string(),
            RuntimeTypeKind::Unknown => "{unknown}".to_string(),
            RuntimeTypeKind::Error => "{error}".to_string(),
            RuntimeTypeKind::ArrayOf {
                subtype,
                size: None,
            } => format!("{}[]", types.get(*subtype).kind.stringify(types)),
            RuntimeTypeKind::ArrayOf {
                subtype,
                size: Some(size),
            } => format!("{}[{}]", types.get(*subtype).kind.stringify(types), size),
            RuntimeTypeKind::Struct {
                members,
                source_hash,
            } => format!(
                "struct (hash: {source_hash:x?}) {{ {} }}",
                members
                    .iter()
                    .map(|(name, _range, type_)| format!(
                        "{name}: {},",
                        types.get(*type_).kind.stringify(types)
                    ))
                    .join("\n")
            ),
        }
    }
    pub fn could_be_float(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: None,
                signed: None | Some(true),
            } => true,
            _ => self.is_float(),
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: None | Some(false),
                signed: None | Some(true),
            } => true,
            RuntimeTypeKind::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeTypeKind::I8 => false,
            RuntimeTypeKind::I16 => false,
            RuntimeTypeKind::I32 => false,
            RuntimeTypeKind::I64 => false,
            RuntimeTypeKind::U8 => false,
            RuntimeTypeKind::U16 => false,
            RuntimeTypeKind::U32 => false,
            RuntimeTypeKind::U64 => false,
            RuntimeTypeKind::F32 => true,
            RuntimeTypeKind::F64 => true,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Boolean => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_signed(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: _,
                signed: None,
            } => true,
            _ => self.is_signed(),
        }
    }
    pub fn is_signed(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                signed: None | Some(true),
                integer: _,
            } => true,
            RuntimeTypeKind::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeTypeKind::I8 => true,
            RuntimeTypeKind::I16 => true,
            RuntimeTypeKind::I32 => true,
            RuntimeTypeKind::I64 => true,
            RuntimeTypeKind::U8 => false,
            RuntimeTypeKind::U16 => false,
            RuntimeTypeKind::U32 => false,
            RuntimeTypeKind::U64 => false,
            RuntimeTypeKind::F32 => true,
            RuntimeTypeKind::F64 => true,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Boolean => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_unsigned(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: None | Some(true), // only ints can be unsigned
                signed: None,
            } => true,
            _ => self.is_unsigned(),
        }
    }
    pub fn is_unsigned(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                signed: None | Some(false),
                integer: _,
            } => true,
            RuntimeTypeKind::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeTypeKind::I8 => false,
            RuntimeTypeKind::I16 => false,
            RuntimeTypeKind::I32 => false,
            RuntimeTypeKind::I64 => false,
            RuntimeTypeKind::U8 => true,
            RuntimeTypeKind::U16 => true,
            RuntimeTypeKind::U32 => true,
            RuntimeTypeKind::U64 => true,
            RuntimeTypeKind::F32 => false,
            RuntimeTypeKind::F64 => false,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Boolean => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_integer(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: None,
                signed: _,
            } => true,
            _ => self.is_integer(),
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: Some(true),
                signed: _,
            } => true,
            RuntimeTypeKind::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeTypeKind::I8 => true,
            RuntimeTypeKind::I16 => true,
            RuntimeTypeKind::I32 => true,
            RuntimeTypeKind::I64 => true,
            RuntimeTypeKind::U8 => true,
            RuntimeTypeKind::U16 => true,
            RuntimeTypeKind::U32 => true,
            RuntimeTypeKind::U64 => true,
            RuntimeTypeKind::F32 => false,
            RuntimeTypeKind::F64 => false,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Boolean => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                integer: _,
                signed: _,
            } => true,
            RuntimeTypeKind::I8 => true,
            RuntimeTypeKind::I16 => true,
            RuntimeTypeKind::I32 => true,
            RuntimeTypeKind::I64 => true,
            RuntimeTypeKind::U8 => true,
            RuntimeTypeKind::U16 => true,
            RuntimeTypeKind::U32 => true,
            RuntimeTypeKind::U64 => true,
            RuntimeTypeKind::F32 => true,
            RuntimeTypeKind::F64 => true,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Boolean => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            RuntimeTypeKind::Number {
                signed: _,
                integer: _,
            } => false,
            RuntimeTypeKind::I8 => false,
            RuntimeTypeKind::I16 => false,
            RuntimeTypeKind::I32 => false,
            RuntimeTypeKind::I64 => false,
            RuntimeTypeKind::U8 => false,
            RuntimeTypeKind::U16 => false,
            RuntimeTypeKind::U32 => false,
            RuntimeTypeKind::U64 => false,
            RuntimeTypeKind::F32 => false,
            RuntimeTypeKind::F64 => false,
            RuntimeTypeKind::Boolean => true,
            RuntimeTypeKind::Unit => false,
            RuntimeTypeKind::Unknown => false,
            RuntimeTypeKind::Error => false,
            RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeTypeKind::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }

    /// true means the specialization succeeded
    pub fn specialize_number_size(&mut self) -> bool {
        if let RuntimeTypeKind::Number { .. } = self {
            match *self {
                RuntimeTypeKind::Number {
                    signed: None | Some(true),
                    integer: None | Some(true),
                } => *self = RuntimeTypeKind::I32,
                RuntimeTypeKind::Number {
                    signed: Some(false),
                    integer: None | Some(true),
                } => *self = RuntimeTypeKind::U32,
                RuntimeTypeKind::Number {
                    signed: Some(true),
                    integer: Some(false),
                } => *self = RuntimeTypeKind::F32,
                RuntimeTypeKind::Number {
                    signed: Some(false),
                    integer: Some(false),
                } => panic!("Unsigned floats shouldn't ever exist"),
                _ => unreachable!("All Number cases should be covered"),
            }
            true
        } else {
            false
        }
    }

    /// Returns true on successful merge
    #[must_use = "Failed merges usually indicate a compile error"]
    pub fn merge_types(
        a: UnionFindSetPtr<RuntimeType>,
        b: UnionFindSetPtr<RuntimeType>,
        types: &mut UnionFindSet<RuntimeType>,
    ) -> bool {
        types.try_merge(a, b, |mut a_full, b_full, types| {
            use UnionFindSetMergeResult as R;
            let a = &mut a_full.kind;
            let b = &b_full.kind;

            if *b == RuntimeTypeKind::Unknown {
                return R::Merged(a_full);
            }

            if *a == RuntimeTypeKind::Error {
                return R::Merged(a_full);
            }
            if *b == RuntimeTypeKind::Error {
                return R::Merged(b_full);
            }

            match a {
                _ if a == b => R::Merged(a_full),

                RuntimeTypeKind::Number { signed, integer } => {
                    if !match signed {
                        Some(true) => b.could_be_signed(),
                        Some(false) => b.could_be_unsigned(),
                        None => true,
                    } {
                        return R::NotMerged {
                            a: a_full,
                            b: b_full,
                        };
                    }

                    if !match integer {
                        Some(true) => b.could_be_integer(),
                        Some(false) => b.could_be_float(),
                        None => b.is_numeric(),
                    } {
                        return R::NotMerged {
                            a: a_full,
                            b: b_full,
                        };
                    }

                    match b {
                        RuntimeTypeKind::Number {
                            signed: b_signed,
                            integer: b_integer,
                        } => R::Merged(RuntimeType {
                            kind: RuntimeTypeKind::Number {
                                signed: match (*signed, *b_signed) {
                                    (None, None) => None,
                                    (None, Some(x)) => Some(x),
                                    (Some(x), None) => Some(x),
                                    (Some(x), Some(y)) => {
                                        assert_eq!(x, y);
                                        Some(x)
                                    }
                                },
                                integer: match (*integer, *b_integer) {
                                    (None, None) => None,
                                    (None, Some(x)) => Some(x),
                                    (Some(x), None) => Some(x),
                                    (Some(x), Some(y)) => {
                                        assert_eq!(x, y);
                                        Some(x)
                                    }
                                },
                            },
                            definition_range: a_full.definition_range.or(b_full.definition_range),
                        }),
                        // we already checked that signedness and int/float are compatible, so
                        // this has to be a number. It also has to be a fully specialized one
                        // because partially specialized ones are handled above.
                        _ => R::Merged(b_full),
                    }
                }

                RuntimeTypeKind::I8
                | RuntimeTypeKind::I16
                | RuntimeTypeKind::I32
                | RuntimeTypeKind::I64 => {
                    if let RuntimeTypeKind::Number { .. } = b
                        && b.could_be_integer()
                        && b.could_be_signed()
                    {
                        R::Merged(a_full)
                    } else if a == b {
                        R::Merged(a_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }
                RuntimeTypeKind::U8
                | RuntimeTypeKind::U16
                | RuntimeTypeKind::U32
                | RuntimeTypeKind::U64 => {
                    if let RuntimeTypeKind::Number { .. } = b
                        && b.could_be_integer()
                        && b.could_be_unsigned()
                    {
                        R::Merged(a_full)
                    } else if a == b {
                        R::Merged(a_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }

                RuntimeTypeKind::F32 | RuntimeTypeKind::F64 => {
                    if b.could_be_float() {
                        R::Merged(a_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }

                RuntimeTypeKind::Boolean => {
                    if b.is_boolean() {
                        R::Merged(b_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }
                RuntimeTypeKind::Unit => {
                    if *b == RuntimeTypeKind::Unit {
                        R::Merged(b_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }
                RuntimeTypeKind::Unknown => R::Merged(b_full),
                RuntimeTypeKind::Error => R::Merged(a_full),
                RuntimeTypeKind::ArrayOf {
                    subtype: a_subtype,
                    size: a_size,
                } => {
                    if let RuntimeTypeKind::ArrayOf {
                        subtype: b_subtype,
                        size: b_size,
                    } = b
                    {
                        if !RuntimeTypeKind::merge_types(*a_subtype, *b_subtype, types) {
                            return R::NotMerged {
                                a: a_full,
                                b: b_full,
                            };
                        }
                        match (*a_size, *b_size) {
                            (None, None) => {}
                            (None, Some(_)) => {
                                *a_size = *b_size;
                            }
                            (Some(_), None) => {}
                            (Some(a_size), Some(b_size)) => {
                                if a_size != b_size {
                                    return R::NotMerged {
                                        a: a_full,
                                        b: b_full,
                                    };
                                }
                            }
                        }

                        R::Merged(a_full)
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }
                RuntimeTypeKind::Struct {
                    members,
                    source_hash,
                } => {
                    if let RuntimeTypeKind::Struct {
                        members: members_2,
                        source_hash: source_hash_2,
                    } = b
                        && (source_hash == source_hash_2
                            || source_hash.is_none()
                            || source_hash_2.is_none())
                    {
                        for member_pair in members.iter().zip_longest(members_2) {
                            match member_pair {
                                EitherOrBoth::Both(
                                    (member1_name, _range1, member1_type),
                                    (member2_name, _range2, member2_type),
                                ) => {
                                    if !RuntimeTypeKind::merge_types(
                                        *member1_type,
                                        *member2_type,
                                        types,
                                    ) {
                                        return R::NotMerged {
                                            a: a_full,
                                            b: b_full,
                                        };
                                    }
                                    if member1_name != member2_name {
                                        return R::NotMerged {
                                            a: a_full,
                                            b: b_full,
                                        };
                                    }
                                }
                                EitherOrBoth::Left(_) => {
                                    return R::NotMerged {
                                        a: a_full,
                                        b: b_full,
                                    };
                                }
                                EitherOrBoth::Right(_) => {
                                    return R::NotMerged {
                                        a: a_full,
                                        b: b_full,
                                    };
                                }
                            }
                        }

                        if source_hash.is_some() {
                            R::Merged(a_full)
                        } else {
                            R::Merged(b_full)
                        }
                    } else {
                        R::NotMerged {
                            a: a_full,
                            b: b_full,
                        }
                    }
                }
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConcreteRuntimeType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Boolean,
    Unit,
    ArrayOf {
        subtype: Box<ConcreteRuntimeType>,
        size: usize,
    },
    Struct {
        members: Vec<(String, ConcreteRuntimeType)>,
        /// indicates which struct{} type definition this one originated from
        source_hash: u64,
    },
}

impl Display for ConcreteRuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConcreteRuntimeType::I8 => f.write_str("i8"),
            ConcreteRuntimeType::I16 => f.write_str("i16"),
            ConcreteRuntimeType::I32 => f.write_str("i32"),
            ConcreteRuntimeType::I64 => f.write_str("i64"),
            ConcreteRuntimeType::U8 => f.write_str("u8"),
            ConcreteRuntimeType::U16 => f.write_str("u16"),
            ConcreteRuntimeType::U32 => f.write_str("u32"),
            ConcreteRuntimeType::U64 => f.write_str("u64"),
            ConcreteRuntimeType::F32 => f.write_str("f32"),
            ConcreteRuntimeType::F64 => f.write_str("f64"),
            ConcreteRuntimeType::Unit => f.write_str("()"),
            ConcreteRuntimeType::Boolean => f.write_str("bool"),
            ConcreteRuntimeType::ArrayOf { subtype, size } => {
                write!(f, "{subtype}[{size}]")
            }
            ConcreteRuntimeType::Struct {
                members,
                source_hash,
            } => {
                write!(f, "struct (hash: {source_hash:x?}) {{ ")?;
                for (name, type_) in members {
                    write!(f, "{name}: {type_},",)?;
                }
                write!(f, " }}")
            }
        }
    }
}

impl ConcreteRuntimeType {
    pub fn is_float(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::F32 | T::F64 => true,
            T::I8
            | T::I16
            | T::I32
            | T::I64
            | T::U8
            | T::U16
            | T::U32
            | T::U64
            | T::Unit
            | T::Boolean
            | T::ArrayOf { .. }
            | T::Struct { .. } => false,
        }
    }
    pub fn is_signed(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::I8 | T::I16 | T::I32 | T::I64 | T::F32 | T::F64 => true,
            T::U8
            | T::U16
            | T::U32
            | T::U64
            | T::Unit
            | T::Boolean
            | T::ArrayOf { .. }
            | T::Struct { .. } => false,
        }
    }
    pub fn is_unsigned(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::U8 | T::U16 | T::U32 | T::U64 => true,
            T::I8
            | T::I16
            | T::I32
            | T::I64
            | T::F32
            | T::F64
            | T::Unit
            | T::Boolean
            | T::ArrayOf { .. }
            | T::Struct { .. } => false,
        }
    }
    pub fn is_integer(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::I8 | T::I16 | T::I32 | T::I64 | T::U8 | T::U16 | T::U32 | T::U64 => true,
            T::F32 | T::F64 | T::Unit | T::Boolean | T::ArrayOf { .. } | T::Struct { .. } => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::I8
            | T::I16
            | T::I32
            | T::I64
            | T::U8
            | T::U16
            | T::U32
            | T::U64
            | T::F32
            | T::F64 => true,
            T::Unit | T::Boolean | T::ArrayOf { .. } | T::Struct { .. } => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        use ConcreteRuntimeType as T;
        match self {
            T::Boolean => true,
            T::I8
            | T::I16
            | T::I32
            | T::I64
            | T::U8
            | T::U16
            | T::U32
            | T::U64
            | T::F32
            | T::F64
            | T::Unit
            | T::ArrayOf { .. }
            | T::Struct { .. } => false,
        }
    }
}
