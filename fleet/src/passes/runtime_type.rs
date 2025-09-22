use itertools::{EitherOrBoth, Itertools};

use crate::passes::union_find_set::{UnionFindSet, UnionFindSetMergeResult, UnionFindSetPtr};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeType {
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
        members: Vec<(String, UnionFindSetPtr<RuntimeType>)>,
        /// indicates which struct{} type definition this one originated from
        source_hash: Option<u64>,
    },
}

impl RuntimeType {
    pub fn unwrap_arrayof(&self) -> (UnionFindSetPtr<RuntimeType>, Option<usize>) {
        if let Self::ArrayOf { subtype, size } = self {
            (*subtype, *size)
        } else {
            panic!("Expected RuntimeType::ArrayOf, but got {self:?}");
        }
    }

    pub fn stringify(&self, types: &UnionFindSet<RuntimeType>) -> String {
        match self {
            RuntimeType::Number { signed, integer } => format!(
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
            RuntimeType::I8 => "i8".to_string(),
            RuntimeType::I16 => "i16".to_string(),
            RuntimeType::I32 => "i32".to_string(),
            RuntimeType::I64 => "i64".to_string(),
            RuntimeType::U8 => "u8".to_string(),
            RuntimeType::U16 => "u16".to_string(),
            RuntimeType::U32 => "u32".to_string(),
            RuntimeType::U64 => "u64".to_string(),
            RuntimeType::F32 => "f32".to_string(),
            RuntimeType::F64 => "f64".to_string(),
            RuntimeType::Unit => "()".to_string(),
            RuntimeType::Boolean => "bool".to_string(),
            RuntimeType::Unknown => "{unknown}".to_string(),
            RuntimeType::Error => "{error}".to_string(),
            RuntimeType::ArrayOf {
                subtype,
                size: None,
            } => format!("{}[]", types.get(*subtype).stringify(types)),
            RuntimeType::ArrayOf {
                subtype,
                size: Some(size),
            } => format!("{}[{}]", types.get(*subtype).stringify(types), size),
            RuntimeType::Struct {
                members,
                source_hash,
            } => format!(
                "struct (hash: {source_hash:x?}) {{\n{}\n}}",
                indent::indent_all_by(
                    4,
                    members
                        .iter()
                        .map(|(name, type_)| format!(
                            "{name}: {},",
                            types.get(*type_).stringify(types)
                        ))
                        .join("\n")
                )
            ),
        }
    }
    pub fn could_be_float(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: None,
                signed: None | Some(true),
            } => true,
            _ => self.is_float(),
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: None | Some(false),
                signed: None | Some(true),
            } => true,
            RuntimeType::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::U8 => false,
            RuntimeType::U16 => false,
            RuntimeType::U32 => false,
            RuntimeType::U64 => false,
            RuntimeType::F32 => true,
            RuntimeType::F64 => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_signed(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: _,
                signed: None,
            } => true,
            _ => self.is_signed(),
        }
    }
    pub fn is_signed(&self) -> bool {
        match self {
            RuntimeType::Number {
                signed: None | Some(true),
                integer: _,
            } => true,
            RuntimeType::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::U8 => false,
            RuntimeType::U16 => false,
            RuntimeType::U32 => false,
            RuntimeType::U64 => false,
            RuntimeType::F32 => true,
            RuntimeType::F64 => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_unsigned(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: None | Some(true), // only ints can be unsigned
                signed: None,
            } => true,
            _ => self.is_unsigned(),
        }
    }
    pub fn is_unsigned(&self) -> bool {
        match self {
            RuntimeType::Number {
                signed: None | Some(false),
                integer: _,
            } => true,
            RuntimeType::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::U8 => true,
            RuntimeType::U16 => true,
            RuntimeType::U32 => true,
            RuntimeType::U64 => true,
            RuntimeType::F32 => false,
            RuntimeType::F64 => false,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn could_be_integer(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: None,
                signed: _,
            } => true,
            _ => self.is_integer(),
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: Some(true),
                signed: _,
            } => true,
            RuntimeType::Number {
                integer: _,
                signed: _,
            } => false,
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::U8 => true,
            RuntimeType::U16 => true,
            RuntimeType::U32 => true,
            RuntimeType::U64 => true,
            RuntimeType::F32 => false,
            RuntimeType::F64 => false,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            RuntimeType::Number {
                integer: _,
                signed: _,
            } => true,
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::U8 => true,
            RuntimeType::U16 => true,
            RuntimeType::U32 => true,
            RuntimeType::U64 => true,
            RuntimeType::F32 => true,
            RuntimeType::F64 => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            RuntimeType::Number {
                signed: _,
                integer: _,
            } => false,
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::U8 => false,
            RuntimeType::U16 => false,
            RuntimeType::U32 => false,
            RuntimeType::U64 => false,
            RuntimeType::F32 => false,
            RuntimeType::F64 => false,
            RuntimeType::Boolean => true,
            RuntimeType::Unit => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
            RuntimeType::Struct {
                members: _,
                source_hash: _,
            } => false,
        }
    }

    /// true means the specialization succeeded
    pub fn specialize_number_size(&mut self) -> bool {
        if let RuntimeType::Number { .. } = self {
            match *self {
                RuntimeType::Number {
                    signed: None | Some(true),
                    integer: None | Some(true),
                } => *self = RuntimeType::I32,
                RuntimeType::Number {
                    signed: Some(false),
                    integer: None | Some(true),
                } => *self = RuntimeType::U32,
                RuntimeType::Number {
                    signed: Some(true),
                    integer: Some(false),
                } => *self = RuntimeType::F32,
                RuntimeType::Number {
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
        types.try_merge(a, b, |mut a, b, types| {
            use UnionFindSetMergeResult::*;
            if b == RuntimeType::Unknown {
                return Merged(a);
            }
            if b == RuntimeType::Error {
                return Merged(b);
            }
            match a {
                _ if a == b => Merged(a),

                RuntimeType::Number { signed, integer } => {
                    if !match signed {
                        Some(true) => b.could_be_signed(),
                        Some(false) => b.could_be_unsigned(),
                        None => true,
                    } {
                        return NotMerged { a, b };
                    }

                    if !match integer {
                        Some(true) => b.could_be_integer(),
                        Some(false) => b.could_be_float(),
                        None => b.is_numeric(),
                    } {
                        return NotMerged { a, b };
                    }

                    match b {
                        RuntimeType::Number {
                            signed: b_signed,
                            integer: b_integer,
                        } => Merged(RuntimeType::Number {
                            signed: match (signed, b_signed) {
                                (None, None) => None,
                                (None, Some(x)) => Some(x),
                                (Some(x), None) => Some(x),
                                (Some(x), Some(y)) => {
                                    assert_eq!(x, y);
                                    Some(x)
                                }
                            },
                            integer: match (integer, b_integer) {
                                (None, None) => None,
                                (None, Some(x)) => Some(x),
                                (Some(x), None) => Some(x),
                                (Some(x), Some(y)) => {
                                    assert_eq!(x, y);
                                    Some(x)
                                }
                            },
                        }),
                        // we already checked that signedness and int/float are compatible, so
                        // this has to be a number. It also has to be a fully specialized one
                        // because partially specialized ones are handled above.
                        _ => Merged(b),
                    }
                }

                RuntimeType::I8 | RuntimeType::I16 | RuntimeType::I32 | RuntimeType::I64 => {
                    if let RuntimeType::Number { .. } = b
                        && b.could_be_integer()
                        && b.could_be_signed()
                    {
                        Merged(a)
                    } else if a == b {
                        Merged(a)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::U8 | RuntimeType::U16 | RuntimeType::U32 | RuntimeType::U64 => {
                    if let RuntimeType::Number { .. } = b
                        && b.could_be_integer()
                        && b.could_be_unsigned()
                    {
                        Merged(a)
                    } else if a == b {
                        Merged(a)
                    } else {
                        NotMerged { a, b }
                    }
                }

                RuntimeType::F32 | RuntimeType::F64 => {
                    if b.could_be_float() {
                        Merged(a)
                    } else {
                        NotMerged { a, b }
                    }
                }

                RuntimeType::Boolean => {
                    if b.is_boolean() {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Unit => {
                    if b == RuntimeType::Unit {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Unknown => Merged(b),
                RuntimeType::Error => Merged(a),
                RuntimeType::ArrayOf {
                    subtype: a_subtype,
                    size: ref mut a_size,
                } => {
                    if let RuntimeType::ArrayOf {
                        subtype: b_subtype,
                        size: b_size,
                    } = b
                    {
                        if !RuntimeType::merge_types(a_subtype, b_subtype, types) {
                            return NotMerged { a, b };
                        }
                        match (*a_size, b_size) {
                            (None, None) => {}
                            (None, Some(_)) => {
                                *a_size = b_size;
                            }
                            (Some(_), None) => {}
                            (Some(a_size), Some(b_size)) => {
                                if a_size != b_size {
                                    return NotMerged { a, b };
                                }
                            }
                        }

                        Merged(a)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Struct {
                    ref members,
                    source_hash,
                } => {
                    if let RuntimeType::Struct {
                        members: ref members_2,
                        source_hash: source_hash_2,
                    } = b
                        && (source_hash == source_hash_2
                            || source_hash.is_none()
                            || source_hash_2.is_none())
                    {
                        for member_pair in members.iter().zip_longest(members_2) {
                            match member_pair {
                                EitherOrBoth::Both(
                                    (member1_name, member1_type),
                                    (member2_name, member2_type),
                                ) => {
                                    if !RuntimeType::merge_types(
                                        *member1_type,
                                        *member2_type,
                                        types,
                                    ) {
                                        return NotMerged { a, b };
                                    }
                                    if member1_name != member2_name {
                                        return NotMerged { a, b };
                                    }
                                }
                                EitherOrBoth::Left(_) => return NotMerged { a, b },
                                EitherOrBoth::Right(_) => return NotMerged { a, b },
                            }
                        }

                        if source_hash.is_some() {
                            Merged(a)
                        } else {
                            Merged(b)
                        }
                    } else {
                        NotMerged { a, b }
                    }
                }
            }
        })
    }
}
