#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Void,
    Byte,   // uint8_t
    Int,    // int32_t
    Long,   // int64_t
    Size,   // size_t
    Float,  // double
    Bool,   // bool
    Char,   // chr32_t
}

impl PrimitiveType {
    pub fn size_in_bytes(&self) -> usize {
        match self {
            PrimitiveType::Void => 0,
            PrimitiveType::Byte => 1,
            PrimitiveType::Int => 4,
            PrimitiveType::Long => 8,
            PrimitiveType::Size => std::mem::size_of::<usize>(),
            PrimitiveType::Float => 8,
            PrimitiveType::Bool => 1,
            PrimitiveType::Char => 4,
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            PrimitiveType::Int | PrimitiveType::Long | PrimitiveType::Float | PrimitiveType::Char
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            PrimitiveType::Byte
                | PrimitiveType::Int
                | PrimitiveType::Long
                | PrimitiveType::Size
                | PrimitiveType::Char
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveType::Float)
    }
}
