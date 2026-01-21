use crate::core::types::composite::{ArrayType, StructType, FunctionType};
use crate::core::types::generic::GenericType;
use crate::core::types::pointer::PointerType;
use crate::core::types::primitive::PrimitiveType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    Struct(StructType),
    Array(ArrayType),
    Pointer(PointerType),
    Generic(GenericType),
    Function(FunctionType),
    String, // first clss str type
}

impl Type {
    pub fn size_in_bytes(&self) -> Option<usize> {
        match self {
            Type::Primitive(p) => Some(p.size_in_bytes()),
            Type::Struct(s) => s.size,
            Type::Array(a) => Some(a.element.size_in_bytes()? * a.size),
            Type::Pointer(_) => Some(std::mem::size_of::<usize>()), // ptr size
            Type::Generic(_) => None, // unknown until monomorphization
            Type::Function(_) => None, // functions dont have a size
            Type::String => Some(std::mem::size_of::<usize>() * 3), // ptr 2 data + length + cpcty
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Primitive(p) => p.size_in_bytes(),
            Type::Struct(s) => s.align.unwrap_or(1),
            Type::Array(a) => a.element.align(),
            Type::Pointer(_) => std::mem::size_of::<usize>(),
            Type::Generic(_) => 1, // unknwn
            Type::Function(_) => 1,
            Type::String => std::mem::size_of::<usize>(),
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
}
