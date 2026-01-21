
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(ArrayType),
    Pointer(PointerType),
    Named(NamedType),
    Generic(GenericType),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Void,
    Byte,
    Int,
    Long,
    Size,
    Float,
    Bool,
    Char,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub element: Box<Type>,
    pub size: Option<usize>, // none 4 unsized arrays
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    pub pointee: Box<Type>,
    pub nullable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    pub name: String,
    pub generics: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_type: Box<Type>,
}

impl Type {
    pub fn int() -> Self {
        Type::Primitive(PrimitiveType::Int)
    }

    pub fn float() -> Self {
        Type::Primitive(PrimitiveType::Float)
    }

    pub fn bool() -> Self {
        Type::Primitive(PrimitiveType::Bool)
    }

    pub fn void() -> Self {
        Type::Primitive(PrimitiveType::Void)
    }

    pub fn ref_(pointee: Type) -> Self {
        Type::Pointer(PointerType {
            pointee: Box::new(pointee),
            nullable: false,
        })
    }

    pub fn ref_nullable(pointee: Type) -> Self {
        Type::Pointer(PointerType {
            pointee: Box::new(pointee),
            nullable: true,
        })
    }
}
