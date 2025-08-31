use crate::passes::union_find_set::{UnionFindSet, UnionFindSetMergeResult, UnionFindSetPtr};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuntimeType {
    Number,
    I8,
    I16,
    I32,
    I64,
    UnsizedInteger,
    F32,
    F64,
    UnsizedFloat,
    Boolean,
    Unit,
    Unknown,
    Error,
    ArrayOf {
        subtype: UnionFindSetPtr<RuntimeType>,
        size: Option<usize>,
    },
}

impl RuntimeType {
    pub fn unwrap_arrayof(self, types: &UnionFindSet<RuntimeType>) -> (RuntimeType, Option<usize>) {
        if let Self::ArrayOf { subtype, size } = self {
            (*types.get(subtype), size)
        } else {
            panic!("Expected RuntimeType::ArrayOf, but got {self:?}");
        }
    }

    pub fn stringify(&self, types: &UnionFindSet<RuntimeType>) -> String {
        match *self {
            RuntimeType::Number => "{number}".to_string(),
            RuntimeType::I8 => "i8".to_string(),
            RuntimeType::I16 => "i16".to_string(),
            RuntimeType::I32 => "i32".to_string(),
            RuntimeType::I64 => "i64".to_string(),
            RuntimeType::UnsizedInteger => "{unsized integer}".to_string(),
            RuntimeType::F32 => "f32".to_string(),
            RuntimeType::F64 => "f64".to_string(),
            RuntimeType::UnsizedFloat => "{unsized float}".to_string(),
            RuntimeType::Unit => "()".to_string(),
            RuntimeType::Boolean => "bool".to_string(),
            RuntimeType::Unknown => "{unknown}".to_string(),
            RuntimeType::Error => "{error}".to_string(),
            RuntimeType::ArrayOf {
                subtype,
                size: None,
            } => format!("{}[]", types.get(subtype).stringify(types)),
            RuntimeType::ArrayOf {
                subtype,
                size: Some(size),
            } => format!("{}[{}]", types.get(subtype).stringify(types), size),
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            RuntimeType::Number => true,
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::UnsizedInteger => false,
            RuntimeType::F32 => true,
            RuntimeType::F64 => true,
            RuntimeType::UnsizedFloat => false,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            RuntimeType::Number => true,
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::UnsizedInteger => true,
            RuntimeType::F32 => false,
            RuntimeType::F64 => false,
            RuntimeType::UnsizedFloat => false,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            RuntimeType::Number => true,
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::UnsizedInteger => true,
            RuntimeType::F32 => true,
            RuntimeType::F64 => true,
            RuntimeType::UnsizedFloat => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            RuntimeType::Number => false,
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::UnsizedInteger => false,
            RuntimeType::F32 => false,
            RuntimeType::F64 => false,
            RuntimeType::UnsizedFloat => false,
            RuntimeType::Boolean => true,
            RuntimeType::Unit => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }

    /// true means the specialization succeeded
    pub fn specialize_int_size(&mut self) -> bool {
        match *self {
            RuntimeType::Number => *self = RuntimeType::I32,
            RuntimeType::UnsizedInteger => *self = RuntimeType::I32,
            RuntimeType::UnsizedFloat => *self = RuntimeType::F32,
            _ => return false,
        }

        true
    }

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
            match a {
                _ if a == b => Merged(a),

                RuntimeType::Number => {
                    if b.is_numeric() {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::UnsizedInteger => {
                    if b.is_integer() {
                        if b == RuntimeType::Number {
                            Merged(a)
                        } else {
                            Merged(b)
                        }
                    } else {
                        NotMerged { a, b }
                    }
                }

                RuntimeType::I8 | RuntimeType::I16 | RuntimeType::I32 | RuntimeType::I64
                    if b == RuntimeType::Number || b == RuntimeType::UnsizedInteger =>
                {
                    Merged(a)
                }
                RuntimeType::I8 | RuntimeType::I16 | RuntimeType::I32 | RuntimeType::I64 => {
                    NotMerged { a, b }
                }

                RuntimeType::UnsizedFloat => {
                    if b.is_float() {
                        if b == RuntimeType::Number {
                            Merged(a)
                        } else {
                            Merged(b)
                        }
                    } else {
                        NotMerged { a, b }
                    }
                }

                RuntimeType::F32 | RuntimeType::F64
                    if b == RuntimeType::Number || b == RuntimeType::UnsizedFloat =>
                {
                    Merged(a)
                }
                RuntimeType::F32 | RuntimeType::F64 => NotMerged { a, b },

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
            }
        })
    }
}
