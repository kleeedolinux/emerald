use crate::core::ast::types::Type as AstType;
use crate::core::types::ty::Type;
use crate::core::types::primitive::PrimitiveType;
use crate::core::types::pointer::PointerType;
use crate::core::types::composite::{ArrayType, StructType, FunctionType};
use crate::core::types::generic::GenericType;

pub fn resolve_ast_type(ast_type: &AstType) -> Type {
    match ast_type {
        AstType::Primitive(p) => Type::Primitive(match p {
            crate::core::ast::types::PrimitiveType::Void => PrimitiveType::Void,
            crate::core::ast::types::PrimitiveType::Byte => PrimitiveType::Byte,
            crate::core::ast::types::PrimitiveType::Int => PrimitiveType::Int,
            crate::core::ast::types::PrimitiveType::Long => PrimitiveType::Long,
            crate::core::ast::types::PrimitiveType::Size => PrimitiveType::Size,
            crate::core::ast::types::PrimitiveType::Float => PrimitiveType::Float,
            crate::core::ast::types::PrimitiveType::Bool => PrimitiveType::Bool,
            crate::core::ast::types::PrimitiveType::Char => PrimitiveType::Char,
        }),
        AstType::Array(a) => Type::Array(ArrayType {
            element: Box::new(resolve_ast_type(&a.element)),
            size: a.size.unwrap_or(0),
        }),
        AstType::Pointer(p) => Type::Pointer(PointerType {
            pointee: Box::new(resolve_ast_type(&p.pointee)),
            nullable: p.nullable,
        }),
        AstType::Named(n) => {
            if n.name == "string" {
                Type::String
            } else {
                Type::Struct(StructType {
                    name: n.name.clone(),
                    fields: Vec::new(),
                    size: None,
                    align: None,
                })
            }
        }
        AstType::Generic(g) => Type::Generic(GenericType {
            name: g.name.clone(),
            constraints: Vec::new(),
        }),
        AstType::Function(f) => Type::Function(FunctionType {
            params: f.params.iter().map(resolve_ast_type).collect(),
            return_type: Box::new(resolve_ast_type(&f.return_type)),
        }),
    }
}
