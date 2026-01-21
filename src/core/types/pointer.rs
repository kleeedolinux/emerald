use crate::core::types::ty::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PointerType {
    pub pointee: Box<Type>,
    pub nullable: bool,
}

impl PointerType {
    pub fn new(pointee: Type, nullable: bool) -> Self {
        Self {
            pointee: Box::new(pointee),
            nullable,
        }
    }

    pub fn ref_(pointee: Type) -> Self {
        Self::new(pointee, false)
    }

    pub fn ref_nullable(pointee: Type) -> Self {
        Self::new(pointee, true)
    }
}
