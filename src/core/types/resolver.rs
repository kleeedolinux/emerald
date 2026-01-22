use crate::core::ast::types::Type as AstType;
use crate::core::types::ty::Type;
use crate::core::types::primitive::PrimitiveType;
use crate::core::types::pointer::PointerType;
use crate::core::types::composite::{ArrayType, StructType, FunctionType};
use crate::core::types::generic::GenericType;
use std::collections::HashSet;

pub fn resolve_ast_type(ast_type: &AstType) -> Type {
    resolve_ast_type_with_context(ast_type, &HashSet::new())
}

pub fn resolve_ast_type_with_context(ast_type: &AstType, generic_params: &HashSet<String>) -> Type {
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
            element: Box::new(resolve_ast_type_with_context(&a.element, generic_params)),
            size: a.size.unwrap_or(0),
        }),
        AstType::Pointer(p) => Type::Pointer(PointerType {
            pointee: Box::new(resolve_ast_type_with_context(&p.pointee, generic_params)),
            nullable: p.nullable,
        }),
        AstType::Named(n) => {
            if n.name == "string" {
                Type::String
            } else if generic_params.contains(&n.name) {
                // this is a generic type param
                Type::Generic(GenericType {
                    name: n.name.clone(),
                    constraints: Vec::new(),
                })
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
            params: f.params.iter().map(|p| resolve_ast_type_with_context(p, generic_params)).collect(),
            return_type: Box::new(resolve_ast_type_with_context(&f.return_type, generic_params)),
        }),
    }
}
