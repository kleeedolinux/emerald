use crate::core::ast::expr::*;
use crate::core::ast::item::*;
use crate::core::ast::stmt::*;
use crate::core::ast::types::*;
use crate::core::ast::Ast;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::lexer::token::{Token, TokenKind};
use crate::frontend::parser::precedence::Precedence;
use codespan::{FileId, Span};

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    file_id: FileId,
    reporter: &'a mut Reporter,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, file_id: FileId, reporter: &'a mut Reporter) -> Self {
        Self {
            tokens,
            current: 0,
            file_id,
            reporter,
        }
    }

    pub fn parse(&mut self) -> Ast {
        let mut items = Vec::new();
        let start_span = self.peek().span;

        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(_) => {
                    self.synchronize();
                }
            }
        }

        let span = if items.is_empty() {
            start_span
        } else {
            Span::new(start_span.start(), self.previous().span.end())
        };

        Ast { items, span }
    }

    fn parse_item(&mut self) -> Result<Item, ()> {
        match self.peek().kind {
            TokenKind::Def => self.parse_function().map(Item::Function),
            TokenKind::Struct => self.parse_struct().map(Item::Struct),
            TokenKind::Trait => self.parse_trait().map(Item::Trait),
            TokenKind::Implement => self.parse_trait_impl().map(Item::TraitImpl),
            TokenKind::Module => self.parse_module().map(Item::Module),
            TokenKind::Foreign => self.parse_foreign().map(Item::Foreign),
            TokenKind::Require => self.parse_require().map(Item::Require),
            TokenKind::Use => self.parse_use().map(Item::Use),
            TokenKind::Declare => self.parse_declare(),
            _ => {
                // try 2 parse as glbl var
                if let Ok(global) = self.parse_global() {
                    Ok(Item::Global(global))
                } else {
                    self.error("Expected item");
                    Err(())
                }
            }
        }
    }

    fn parse_function(&mut self) -> Result<Function, ()> {
        let start_span = self.advance().span; // def
        let name = self.expect_identifier_or_keyword()?;
        let generics = self.parse_generics()?;
        let (params, _variadic) = self.parse_params()?;
        let return_type = if self.check(&TokenKind::Returns) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let uses = if self.check(&TokenKind::Uses) {
            self.advance();
            self.parse_uses()?
        } else {
            Vec::new()
        };
        let body = if self.check(&TokenKind::LeftBrace) {
            Some(self.parse_block_stmts()?)
        } else if self.check(&TokenKind::Equal) {
            // one liner fn
            self.advance();
            let expr = self.parse_expression()?;
            Some(vec![Stmt::Return(ReturnStmt {
                value: Some(expr),
                span: self.previous().span,
            })])
        } else if !self.is_at_end() && !self.check(&TokenKind::End) {
            // fn body w/ statements until end
            Some(self.parse_stmts_until_end()?)
        } else {
            // Empty body - if we're at End, consume it
            if self.check(&TokenKind::End) {
                self.advance();
            }
            None
        };

        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Function {
            name,
            generics,
            params,
            return_type,
            body,
            uses,
            span,
        })
    }

    fn parse_trait_params(&mut self) -> Result<Vec<Param>, ()> {
        if self.check(&TokenKind::LeftParen) {
            self.advance(); // (
            let mut params = Vec::new();
            if !self.check(&TokenKind::RightParen) {
                loop {
                    let name = self.expect_identifier_or_keyword()?;
                    // in traits, first param (self) can omit type
                    if params.is_empty() && name == "self" && !self.check(&TokenKind::Colon) {
                        // self without type - use void as placeholder, will be checked in trait checker
                        params.push(Param {
                            name,
                            type_: Type::Primitive(crate::core::ast::types::PrimitiveType::Void),
                            span: self.previous().span,
                        });
                        if !self.check(&TokenKind::Comma) {
                            break;
                        }
                        self.advance(); // ,
                        continue;
                    }
                    if !self.check(&TokenKind::Colon) {
                        self.error("Parameter must have explicit type annotation");
                        return Err(());
                    }
                    self.advance(); // :
                    let type_ = self.parse_type()?;
                    let span = self.previous().span;
                    params.push(Param {
                        name,
                        type_,
                        span,
                    });
                    if !self.check(&TokenKind::Comma) {
                        break;
                    }
                    self.advance(); // ,
                }
            }
            self.expect(&TokenKind::RightParen)?;
            Ok(params)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_params(&mut self) -> Result<(Vec<Param>, bool), ()> {
        if self.check(&TokenKind::LeftParen) {
            // parse with parentheses
            self.advance(); // (
            let mut params = Vec::new();
            let mut variadic = false;
            if !self.check(&TokenKind::RightParen) {
                loop {
                    // Check for ellipsis (variadic parameter)
                    if self.check(&TokenKind::Ellipsis) {
                        self.advance(); // consume ...
                        // Ellipsis should be the last thing before closing paren
                        if !self.check(&TokenKind::RightParen) {
                            self.error("Ellipsis must be the last parameter");
                            return Err(());
                        }
                        variadic = true;
                        break; // Exit loop, ellipsis handled
                    }
                    
                    let name = self.expect_identifier_or_keyword()?;
                    // require explicit type annotation for all parameters
                    if !self.check(&TokenKind::Colon) {
                        self.error("Parameter must have explicit type annotation");
                        return Err(());
                    }
                    self.advance(); // :
                    let type_ = self.parse_type()?;
                    let span = self.previous().span;
                    params.push(Param {
                        name,
                        type_,
                        span,
                    });

                    if !self.check(&TokenKind::Comma) {
                        break;
                    }
                    self.advance(); // ,
                }
            }
            self.expect(&TokenKind::RightParen)?;
            Ok((params, variadic))
        } else {
            // try to parse params without parentheses
            // First check if we're definitely not in params (see returns, uses, {, end, or statement keywords)
            if self.check(&TokenKind::Returns)
                || self.check(&TokenKind::Uses)
                || self.check(&TokenKind::LeftBrace)
                || self.check(&TokenKind::End)
                || self.check(&TokenKind::If)
                || self.check(&TokenKind::While)
                || self.check(&TokenKind::For)
                || self.check(&TokenKind::Return)
                || self.check(&TokenKind::Mut)
                || self.check(&TokenKind::Comptime)
                || self.is_at_end()
            {
                return Ok((Vec::new(), false));
            }
            
            // Check if next token is identifier followed by colon
            let looks_like_param = matches!(self.peek().kind, TokenKind::Identifier(_))
                && self.current + 1 < self.tokens.len()
                && matches!(self.tokens[self.current + 1].kind, TokenKind::Colon);
            
            if !looks_like_param {
                // If we see = or other statement starters, it's not a param
                if self.check(&TokenKind::Equal) {
                    return Ok((Vec::new(), false));
                }
                // If we see an identifier but not followed by colon, it's not a param
                if matches!(self.peek().kind, TokenKind::Identifier(_)) {
                    return Ok((Vec::new(), false));
                }
                // If we see a literal, expression starter, or other non-parameter token, it's not a param
                // Just return empty params instead of erroring
                return Ok((Vec::new(), false));
            }
            
            // parse parameters until we hit returns, uses, {, =, or end
            let mut params = Vec::new();
            loop {
                // check if we've hit a terminator
                if self.check(&TokenKind::Returns)
                    || self.check(&TokenKind::Uses)
                    || self.check(&TokenKind::LeftBrace)
                    || self.check(&TokenKind::Equal)
                    || self.check(&TokenKind::End)
                    || self.is_at_end()
                {
                    break;
                }
                
                // Check if next token is identifier followed by colon (a parameter)
                if !matches!(self.peek().kind, TokenKind::Identifier(_)) {
                    break;
                }
                // Peek ahead to see if it's followed by colon
                if self.current + 1 >= self.tokens.len() 
                    || !matches!(self.tokens[self.current + 1].kind, TokenKind::Colon) {
                    break;
                }
                
                // Peek further: if after "identifier : type" we see "=", it's a statement
                // We need to parse ahead to check this
                let mut peek_pos = self.current;
                let mut is_param = true;
                
                // Skip identifier and colon
                if peek_pos + 1 < self.tokens.len() 
                    && matches!(self.tokens[peek_pos].kind, TokenKind::Identifier(_))
                    && matches!(self.tokens[peek_pos + 1].kind, TokenKind::Colon) {
                    peek_pos += 2;
                    // Try to find what comes after the type
                    // Types can be: primitive, identifier, ref type, etc.
                    // We'll look for =, ,, returns, uses, {, end
                    while peek_pos < self.tokens.len() {
                        match &self.tokens[peek_pos].kind {
                            TokenKind::Equal => {
                                // After type we see =, this is a statement, not a parameter
                                is_param = false;
                                break;
                            }
                            TokenKind::Comma | TokenKind::Returns | TokenKind::Uses 
                            | TokenKind::LeftBrace | TokenKind::End => {
                                // These indicate end of parameter or parameter list
                                break;
                            }
                            _ => {
                                peek_pos += 1;
                            }
                        }
                    }
                }
                
                if !is_param {
                    // This looks like a statement, not a parameter
                    break;
                }
                
                let name = self.expect_identifier_or_keyword()?;
                // require explicit type annotation for all parameters
                if !self.check(&TokenKind::Colon) {
                    self.error("Parameter must have explicit type annotation");
                    return Err(());
                }
                self.advance(); // :
                let type_ = self.parse_type()?;
                
                // Double-check: if we see = now, this was actually a statement
                if self.check(&TokenKind::Equal) {
                    // We've consumed too much, but that's okay - the caller will handle it
                    break;
                }
                
                let span = self.previous().span;
                params.push(Param {
                    name,
                    type_,
                    span,
                });

                // check if there's another parameter (comma) or if we're done
                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance(); // ,
            }
            Ok((params, false))
        }
    }

    fn parse_generics(&mut self) -> Result<Vec<GenericParam>, ()> {
        if !self.check(&TokenKind::LeftBracket) {
            return Ok(Vec::new());
        }
        self.advance(); // [

        let mut generics = Vec::new();
        loop {
            if self.check(&TokenKind::RightBracket) {
                break;
            }
            self.expect(&TokenKind::Identifier("Type".to_string()))?; // type kywrd
            let name = self.expect_identifier_or_keyword()?;
            let constraint = if self.check(&TokenKind::Identifier("for".to_string())) {
                self.advance();
                Some(self.expect_identifier()?)
            } else {
                None
            };
            let span = self.previous().span;
            generics.push(GenericParam {
                name,
                constraint,
                span,
            });

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance(); // 
        }
        self.expect(&TokenKind::RightBracket)?;
        Ok(generics)
    }

    fn parse_struct(&mut self) -> Result<Struct, ()> {
        let start_span = self.advance().span; // struct
        let name = self.expect_identifier_or_keyword()?;
        let generics = self.parse_generics()?;
        let mut fields = Vec::new();

        while !self.check(&TokenKind::End) && !self.is_at_end() {
            let field_name = self.expect_identifier_or_keyword()?;
            self.expect(&TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            let span = self.previous().span;
            fields.push(Field {
                name: field_name,
                type_,
                span,
            });
        }

        self.expect(&TokenKind::End)?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Struct {
            name,
            generics,
            fields,
            span,
        })
    }

    fn parse_trait(&mut self) -> Result<Trait, ()> {
        let start_span = self.advance().span; // trait
        let name = self.expect_identifier_or_keyword()?;
        let generics = self.parse_generics()?;
        let mut methods = Vec::new();

        // parse methods until we hit end
        loop {
            if self.check(&TokenKind::End) || self.is_at_end() {
                break;
            }
            if self.check(&TokenKind::Def) {
                let method = self.parse_trait_method()?;
                methods.push(method);
            } else {
                // unxpctd token break and let expect handle the err
                break;
            }
        }

        self.expect(&TokenKind::End)?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Trait {
            name,
            generics,
            methods,
            span,
        })
    }

    fn parse_trait_method(&mut self) -> Result<TraitMethod, ()> {
        self.advance(); // def
        let name = self.expect_identifier_or_keyword()?;
        let params = self.parse_trait_params()?;
        let return_type = if self.check(&TokenKind::Returns) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let span = self.previous().span;
        Ok(TraitMethod {
            name,
            params,
            return_type,
            span,
        })
    }

    fn parse_trait_impl(&mut self) -> Result<TraitImpl, ()> {
        let start_span = self.advance().span; // implement
        let trait_name = self.expect_identifier()?;
        self.expect(&TokenKind::For)?;
        let type_name = self.expect_identifier()?;
        let generics = self.parse_generics()?;
        let mut methods = Vec::new();

        while !self.check(&TokenKind::End) && !self.is_at_end() {
            if self.check(&TokenKind::Def) {
                let method = self.parse_function()?;
                methods.push(method);
            } else {
                self.advance();
            }
        }

        self.expect(&TokenKind::End)?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(TraitImpl {
            trait_name,
            type_name,
            generics,
            methods,
            span,
        })
    }

    fn parse_module(&mut self) -> Result<Module, ()> {
        let start_span = self.advance().span; // mdl
        let name = self.expect_identifier_or_keyword()?;
        let mut items = Vec::new();

        while !self.check(&TokenKind::End) && !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(_) => {
                    self.synchronize();
                }
            }
        }

        self.expect(&TokenKind::End)?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Module { name, items, span })
    }

    fn parse_foreign(&mut self) -> Result<Foreign, ()> {
        let start_span = self.advance().span; // foreign
        let abi = if matches!(self.peek().kind, TokenKind::StringLiteral(_)) {
            if let TokenKind::StringLiteral(s) = self.advance().kind.clone() {
                s
            } else {
                "C".to_string()
            }
        } else {
            "C".to_string()
        };
        let name = self.expect_identifier_or_keyword()?;
        let mut functions = Vec::new();

        while !self.check(&TokenKind::End) && !self.is_at_end() {
            if self.check(&TokenKind::Def) {
                let func = self.parse_foreign_function()?;
                functions.push(func);
            } else {
                self.advance();
            }
        }

        self.expect(&TokenKind::End)?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Foreign {
            abi,
            name,
            functions,
            span,
        })
    }

    fn parse_foreign_function(&mut self) -> Result<ForeignFunction, ()> {
        self.advance(); // def
        let name = self.expect_identifier_or_keyword()?;
        let (params, variadic) = self.parse_params()?;
        let return_type = if self.check(&TokenKind::Returns) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let abi = if self.check(&TokenKind::Identifier("with".to_string())) {
            self.advance();
            self.expect(&TokenKind::Identifier("abi".to_string()))?;
            self.expect(&TokenKind::Equal)?;
            if let TokenKind::StringLiteral(s) = self.advance().kind.clone() {
                Some(s)
            } else {
                None
            }
        } else {
            None
        };
        let span = self.previous().span;
        Ok(ForeignFunction {
            name,
            params,
            return_type,
            abi,
            variadic,
            span,
        })
    }

    fn parse_require(&mut self) -> Result<Require, ()> {
        let start_span = self.advance().span; // require
        let path = if let TokenKind::StringLiteral(s) = self.advance().kind.clone() {
            s
        } else {
            self.error("Expected string literal after require");
            return Err(());
        };
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Require { path, span })
    }

    fn parse_use(&mut self) -> Result<Use, ()> {
        let start_span = self.advance().span; // use
        let mut path = Vec::new();
        loop {
            let name = self.expect_identifier_or_keyword()?;
            path.push(name);
            if self.check(&TokenKind::Dot) {
                self.advance(); // .
            } else if self.check(&TokenKind::ColonColon) {
                self.advance(); // ::
            } else {
                break;
            }
        }
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Use { path, span })
    }

    fn parse_declare(&mut self) -> Result<Item, ()> {
        let start_span = self.advance().span; // declare
        if self.check(&TokenKind::Struct) {
            self.advance();
            let name = self.expect_identifier_or_keyword()?;
            let generics = self.parse_generics()?;
            let span = Span::new(start_span.start(), self.previous().span.end());
            Ok(Item::ForwardDecl(ForwardDecl {
                name,
                generics,
                span,
            }))
        } else {
            self.error("Expected struct after declare");
            Err(())
        }
    }

    fn parse_global(&mut self) -> Result<Global, ()> {
        let start_span = self.peek().span;
        let mutable = self.check(&TokenKind::Mut);
        if mutable {
            self.advance();
        }
        let name = self.expect_identifier_or_keyword()?;
        self.expect(&TokenKind::Colon)?;
        let type_ = self.parse_type()?;
        let value = if self.check(&TokenKind::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Global {
            name,
            mutable,
            type_,
            value,
            span,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ()> {
        let base_type = match self.peek().kind {
            TokenKind::Void => {
                self.advance();
                Type::Primitive(PrimitiveType::Void)
            }
            TokenKind::Byte => {
                self.advance();
                Type::Primitive(PrimitiveType::Byte)
            }
            TokenKind::Int => {
                self.advance();
                Type::Primitive(PrimitiveType::Int)
            }
            TokenKind::Long => {
                self.advance();
                Type::Primitive(PrimitiveType::Long)
            }
            TokenKind::Size => {
                self.advance();
                Type::Primitive(PrimitiveType::Size)
            }
            TokenKind::Float => {
                self.advance();
                Type::Primitive(PrimitiveType::Float)
            }
            TokenKind::Bool => {
                self.advance();
                Type::Primitive(PrimitiveType::Bool)
            }
            TokenKind::Char => {
                self.advance();
                Type::Primitive(PrimitiveType::Char)
            }
            TokenKind::String => {
                self.advance();
                Type::Named(NamedType { name: "string".to_string(), generics: Vec::new() })
            }
            TokenKind::Ref => {
                self.advance();
                let pointee = self.parse_type()?;
                // chk if array type follows: ref int[10]
                if self.check(&TokenKind::LeftBracket) {
                    self.advance(); // [
                    let size = if matches!(self.peek().kind, TokenKind::IntLiteral(_)) {
                        if let TokenKind::IntLiteral(n) = self.advance().kind.clone() {
                            Some(n as usize)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    self.expect(&TokenKind::RightBracket)?;
                    Type::Array(ArrayType {
                        element: Box::new(Type::ref_(pointee)),
                        size,
                    })
                } else {
                    Type::ref_(pointee)
                }
            }
            TokenKind::RefNullable => {
                self.advance();
                let pointee = self.parse_type()?;
                // chk if array type follows: ref? int[10]
                if self.check(&TokenKind::LeftBracket) {
                    self.advance(); // [
                    let size = if matches!(self.peek().kind, TokenKind::IntLiteral(_)) {
                        if let TokenKind::IntLiteral(n) = self.advance().kind.clone() {
                            Some(n as usize)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    self.expect(&TokenKind::RightBracket)?;
                    Type::Array(ArrayType {
                        element: Box::new(Type::ref_nullable(pointee)),
                        size,
                    })
                } else {
                    Type::ref_nullable(pointee)
                }
            }
            TokenKind::LeftBracket => {
                self.error("Unexpected [ in type position");
                return Err(());
            }
            TokenKind::Identifier(_) => {
                let name = if let TokenKind::Identifier(n) = self.advance().kind.clone() {
                    n
                } else {
                    return Err(());
                };
                // chk if this is an array type: MyType[5] or generic type List[int]
                if self.check(&TokenKind::LeftBracket) {
                    self.advance(); // [
                    // chk if its an array size (int literal) or generic params (types)
                    if matches!(self.peek().kind, TokenKind::IntLiteral(_)) {
                        // array type: MyType[10]
                        let size = if let TokenKind::IntLiteral(n) = self.advance().kind.clone() {
                            Some(n as usize)
                        } else {
                            None
                        };
                        self.expect(&TokenKind::RightBracket)?;
                        let element_type = if name == "string" {
                            Type::Named(NamedType { name: "string".to_string(), generics: Vec::new() })
                        } else {
                            Type::Named(NamedType { name, generics: Vec::new() })
                        };
                        Type::Array(ArrayType {
                            element: Box::new(element_type),
                            size,
                        })
                    } else {
                        // generic type: List[int, bool]
                        let mut types = Vec::new();
                        loop {
                            if self.check(&TokenKind::RightBracket) {
                                break;
                            }
                            types.push(self.parse_type()?);
                            if !self.check(&TokenKind::Comma) {
                                break;
                            }
                            self.advance(); // 
                        }
                        self.expect(&TokenKind::RightBracket)?;
                        // chk if array type follows: List[int][10]
                        if self.check(&TokenKind::LeftBracket) {
                            self.advance(); // [
                            let size = if matches!(self.peek().kind, TokenKind::IntLiteral(_)) {
                                if let TokenKind::IntLiteral(n) = self.advance().kind.clone() {
                                    Some(n as usize)
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            self.expect(&TokenKind::RightBracket)?;
                            Type::Array(ArrayType {
                                element: Box::new(Type::Named(NamedType { name, generics: types })),
                                size,
                            })
                        } else {
                            Type::Named(NamedType { name, generics: types })
                        }
                    }
                } else {
                    Type::Named(NamedType { name, generics: Vec::new() })
                }
            }
            _ => {
                self.error("Expected type");
                return Err(());
            }
        };
        
        // chk if array type follows the base type: int[10], string[5], etc
        let final_type = if self.check(&TokenKind::LeftBracket) {
            self.advance(); // [
            let size = if matches!(self.peek().kind, TokenKind::IntLiteral(_)) {
                if let TokenKind::IntLiteral(n) = self.advance().kind.clone() {
                    Some(n as usize)
                } else {
                    None
                }
            } else {
                None
            };
            self.expect(&TokenKind::RightBracket)?;
            Type::Array(ArrayType {
                element: Box::new(base_type),
                size,
            })
        } else {
            base_type
        };
        
        Ok(final_type)
    }

    fn parse_block_stmts(&mut self) -> Result<Vec<Stmt>, ()> {
        self.expect(&TokenKind::LeftBrace)?;
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(&TokenKind::RightBrace)?;
        Ok(stmts)
    }

    fn parse_stmts_until_end(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::End) && !self.is_at_end() {
            eprintln!("[DEBUG PARSER] About to parse stmt, current token: {:?}, pos: {}", self.peek().kind, self.current);
            match self.parse_stmt() {
                Ok(stmt) => {
                    eprintln!("[DEBUG PARSER] Successfully parsed stmt: {:?}", std::mem::discriminant(&stmt));
                    stmts.push(stmt);
                    eprintln!("[DEBUG PARSER] After parsing, current token: {:?}, pos: {}", self.peek().kind, self.current);
                }
                Err(_) => {
                    eprintln!("[DEBUG PARSER] Error parsing stmt, synchronizing");
                    self.synchronize();
                }
            }
        }
        eprintln!("[DEBUG PARSER] Parsed {} statements total", stmts.len());
        self.expect(&TokenKind::End)?;
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ()> {
        match self.peek().kind {
            TokenKind::Return => self.parse_return().map(Stmt::Return),
            TokenKind::If => self.parse_if_stmt().map(Stmt::If),
            TokenKind::While => self.parse_while().map(Stmt::While),
            TokenKind::For => self.parse_for().map(Stmt::For),
            TokenKind::Break => {
                let span = self.advance().span;
                Ok(Stmt::Break(BreakStmt { span }))
            }
            TokenKind::Continue => {
                let span = self.advance().span;
                Ok(Stmt::Continue(ContinueStmt { span }))
            }
            TokenKind::Mut | TokenKind::Identifier(_) => {
                // culd be let sttmnt or expression
                if self.check(&TokenKind::Mut) || self.check_ahead_identifier_colon() {
                    self.parse_let().map(Stmt::Let)
                } else {
                    self.parse_expression()
                        .map(|e| Stmt::Expr(ExprStmt { expr: e, span: self.previous().span }))
                }
            }
            _ => self
                .parse_expression()
                .map(|e| Stmt::Expr(ExprStmt { expr: e, span: self.previous().span })),
        }
    }

    fn check_ahead_identifier_colon(&self) -> bool {
        if let Some(token) = self.tokens.get(self.current) {
            if matches!(token.kind, TokenKind::Identifier(_)) {
                if let Some(next) = self.tokens.get(self.current + 1) {
                    return matches!(next.kind, TokenKind::Colon);
                }
            }
        }
        false
    }

    fn parse_let(&mut self) -> Result<LetStmt, ()> {
        let comptime = self.check(&TokenKind::Comptime);
        if comptime {
            self.advance();
        }
        let mutable = self.check(&TokenKind::Mut);
        if mutable {
            self.advance();
        }
        let name = self.expect_identifier_or_keyword()?;
        let type_annotation = if self.check(&TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let value = if self.check(&TokenKind::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        let span = self.previous().span;
        Ok(LetStmt {
            name,
            mutable,
            comptime,
            type_annotation,
            value,
            span,
        })
    }

    fn parse_return(&mut self) -> Result<ReturnStmt, ()> {
        let start_span = self.advance().span; // ret
        let value = if !self.check(&TokenKind::Semicolon) && !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(ReturnStmt { value, span })
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ()> {
        let start_span = self.advance().span; // if
        let condition = self.parse_expression()?;
        let then_branch = if self.check(&TokenKind::LeftBrace) {
            self.parse_block_stmts()?
        } else {
            // parse statements until else or end
            let mut stmts = Vec::new();
            while !self.check(&TokenKind::Else) && !self.check(&TokenKind::End) && !self.is_at_end() {
                stmts.push(self.parse_stmt()?);
            }
            stmts
        };
        let else_branch = if self.check(&TokenKind::Else) {
            self.advance();
            if self.check(&TokenKind::LeftBrace) {
                Some(self.parse_block_stmts()?)
            } else {
                // parse statements unt end
                let mut stmts = Vec::new();
                while !self.check(&TokenKind::End) && !self.is_at_end() {
                    stmts.push(self.parse_stmt()?);
                }
                Some(stmts)
            }
        } else {
            None
        };
        // consume the end keyword 4 the if statement
        if !self.check(&TokenKind::LeftBrace) {
            self.expect(&TokenKind::End)?;
        }
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }

    fn parse_while(&mut self) -> Result<WhileStmt, ()> {
        let start_span = self.advance().span; // whl
        let condition = self.parse_expression()?;
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block_stmts()?
        } else {
            self.parse_stmts_until_end()?
        };
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(WhileStmt {
            condition,
            body,
            span,
        })
    }

    fn parse_for(&mut self) -> Result<ForStmt, ()> {
        let start_span = self.advance().span; // 4
        self.expect(&TokenKind::LeftParen)?;
        let init = if !self.check(&TokenKind::Semicolon) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };
        self.expect(&TokenKind::Semicolon)?;
        let condition = if !self.check(&TokenKind::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(&TokenKind::Semicolon)?;
        let increment = if !self.check(&TokenKind::RightParen) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(&TokenKind::RightParen)?;
        let body = self.parse_block_stmts()?;
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(ForStmt {
            init,
            condition,
            increment,
            body,
            span,
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, ()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expr, ()> {
        let mut expr = self.parse_prefix()?;
        
        if (precedence == Precedence::Call || precedence == Precedence::Assignment)
            && !self.is_at_end()
            && !self.check(&TokenKind::Semicolon)
            && !self.check(&TokenKind::End)
            && !self.check(&TokenKind::Else)
            && !self.check(&TokenKind::Comma)
            && !self.check(&TokenKind::LeftParen)
            && !self.check(&TokenKind::RightParen)
            && !self.check(&TokenKind::Eof)
        {
            let is_callable = matches!(
                expr,
                Expr::Variable(_) 
                | Expr::FieldAccess(_) 
                | Expr::MethodCall(_)
                | Expr::Call(_)
                | Expr::Index(_)
            );
            
            if is_callable && self.can_parse_call_without_parens() {
                return self.parse_call_without_parens(expr);
            }
        }

        loop {
            if matches!(self.peek().kind, TokenKind::Identifier(_)) {
                if let Some(next) = self.tokens.get(self.current + 1) {
                    if matches!(next.kind, TokenKind::Equal) {
                        break;
                    }
                }
            }
            let next_prec = self.get_precedence();
            if precedence > next_prec {
                break;
            }
            if self.check(&TokenKind::Semicolon)
                || self.check(&TokenKind::End)
                || self.check(&TokenKind::Else)
                || self.check(&TokenKind::Comma)
                || self.check(&TokenKind::RightParen)
                || self.check(&TokenKind::Eof) {
                break;
            }
            // save current position before calling parse_infix
            let pos_before = self.current;
            expr = self.parse_infix(expr, precedence)?;
            // if parse_infix didn't advance, break to prevent infinite loop
            if self.current == pos_before {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ()> {
        let kind = self.peek().kind.clone();
        match kind {
            TokenKind::Minus | TokenKind::Not => {
                let op = match self.advance().kind {
                    TokenKind::Minus => UnaryOp::Neg,
                    TokenKind::Not => UnaryOp::Not,
                    _ => return Err(()),
                };
                let expr = self.parse_precedence(Precedence::Unary)?;
                let span = self.previous().span;
                Ok(Expr::Unary(UnaryExpr {
                    op,
                    expr: Box::new(expr),
                    span,
                }))
            }
            TokenKind::IntLiteral(n) => {
                let span = self.advance().span;
                Ok(Expr::Literal(LiteralExpr {
                    kind: LiteralKind::Int(n),
                    span,
                }))
            }
            TokenKind::FloatLiteral(n) => {
                let span = self.advance().span;
                Ok(Expr::Literal(LiteralExpr {
                    kind: LiteralKind::Float(n),
                    span,
                }))
            }
            TokenKind::BoolLiteral(b) => {
                let span = self.advance().span;
                Ok(Expr::Literal(LiteralExpr {
                    kind: LiteralKind::Bool(b),
                    span,
                }))
            }
            TokenKind::CharLiteral(c) => {
                let span = self.advance().span;
                Ok(Expr::Literal(LiteralExpr {
                    kind: LiteralKind::Char(c),
                    span,
                }))
            }
            TokenKind::StringLiteral(s) => {
                let span = self.advance().span;
                Ok(Expr::Literal(LiteralExpr {
                    kind: LiteralKind::String(s),
                    span,
                }))
            }
            TokenKind::Null => {
                self.advance();
                Ok(Expr::Null)
            }
            TokenKind::Identifier(_) => {
                let name = if let TokenKind::Identifier(n) = self.advance().kind.clone() {
                    n
                } else {
                    return Err(());
                };
                let start_span = self.previous().span;
                // chk 4 module access: Utils::helper
                if self.check(&TokenKind::ColonColon) {
                    self.advance(); // ::
                    let member = self.expect_identifier_or_keyword()?;
                    let span = Span::new(start_span.start(), self.previous().span.end());
                    Ok(Expr::ModuleAccess(ModuleAccessExpr {
                        module: name,
                        member,
                        span,
                    }))
                } else {
                    let span = start_span;
                    Ok(Expr::Variable(VariableExpr { name, span }))
                }
            }
            TokenKind::Size | TokenKind::Int | TokenKind::Float | TokenKind::Bool 
            | TokenKind::Char | TokenKind::String | TokenKind::Void | TokenKind::Byte 
            | TokenKind::Long => {
                // Type keywords can be used as identifiers in expressions
                let name = match self.advance().kind {
                    TokenKind::Size => "size".to_string(),
                    TokenKind::Int => "int".to_string(),
                    TokenKind::Float => "float".to_string(),
                    TokenKind::Bool => "bool".to_string(),
                    TokenKind::Char => "char".to_string(),
                    TokenKind::String => "string".to_string(),
                    TokenKind::Void => "void".to_string(),
                    TokenKind::Byte => "byte".to_string(),
                    TokenKind::Long => "long".to_string(),
                    _ => return Err(()),
                };
                let span = self.previous().span;
                Ok(Expr::Variable(VariableExpr { name, span }))
            }
            TokenKind::LeftParen => {
                self.advance(); // (
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::RightParen)?;
                Ok(expr)
            }
            TokenKind::LeftBracket => {
                // array literal: [expr1, expr2, ...]
                let start_span = self.advance().span; // [
                let mut elements = Vec::new();
                if !self.check(&TokenKind::RightBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.check(&TokenKind::Comma) {
                            break;
                        }
                        self.advance(); // ,
                    }
                }
                self.expect(&TokenKind::RightBracket)?;
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::ArrayLiteral(ArrayLiteralExpr {
                    elements,
                    span,
                }))
            }
            TokenKind::LeftBrace => {
                let start_span = self.advance().span; // {
                // chk if this is struct literal: Circle { radius: 5.0 }
                // struct literals have field: value pairs
                if matches!(self.peek().kind, TokenKind::Identifier(_)) {
                    // might be struct literal - try parsing fields
                    let mut fields = Vec::new();
                    let mut is_struct_literal = false;
                    loop {
                        if self.check(&TokenKind::RightBrace) {
                            break;
                        }
                        if let Ok(field_name) = self.expect_identifier_or_keyword() {
                            if self.check(&TokenKind::Colon) {
                                // field: value - struct literal
                                is_struct_literal = true;
                                self.advance(); // :
                                let value = self.parse_expression()?;
                                fields.push((field_name, value));
                                if !self.check(&TokenKind::Comma) {
                                    break;
                                }
                                self.advance(); // ,
                            } else {
                                // not struct literal - rewind and parse as block
                                // 4 now just break and parse as block
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    self.expect(&TokenKind::RightBrace)?;
                    if is_struct_literal {
                        // need struct name - will be set by caller context
                        // 4 now create w/ empty name, will be filled in parse_infix
                        let span = Span::new(start_span.start(), self.previous().span.end());
                        Ok(Expr::StructLiteral(StructLiteralExpr {
                            struct_name: String::new(), // will be filled by context
                            fields,
                            span,
                        }))
                    } else {
                        // block expression
                        let stmts = Vec::new();
                        let span = Span::new(start_span.start(), self.previous().span.end());
                        Ok(Expr::Block(BlockExpr { stmts, expr: None, span }))
                    }
                } else {
                    // block expression
                    let mut stmts = Vec::new();
                    while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                        stmts.push(self.parse_stmt()?);
                    }
                    let expr = if !stmts.is_empty() && self.check(&TokenKind::RightBrace) {
                        None
                    } else {
                        None
                    };
                    self.expect(&TokenKind::RightBrace)?;
                    let span = Span::new(start_span.start(), self.previous().span.end());
                    Ok(Expr::Block(BlockExpr { stmts, expr, span }))
                }
            }
            TokenKind::If => {
                let start_span = self.advance().span; // if
                let condition = self.parse_expression()?;
                let then_branch = Box::new(self.parse_expression()?);
                let else_branch = if self.check(&TokenKind::Else) {
                    self.advance();
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::If(IfExpr {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                    span,
                }))
            }
            TokenKind::At => {
                let start_span = self.advance().span; // at
                // parse expr after @ - use Unary precedence to avoid call-without-parens check
                // then manually handle postfix ops
                let mut expr = self.parse_precedence(Precedence::Unary)?;
                // allow postfix ops like field access, indexing (but stop at comma/paren)
                // Check for terminators first, then check precedence
                while !self.is_at_end()
                    && !self.check(&TokenKind::Comma) 
                    && !self.check(&TokenKind::RightParen) 
                    && !self.check(&TokenKind::Semicolon) 
                    && !self.check(&TokenKind::End)
                    && Precedence::Call <= self.get_precedence() {
                    expr = self.parse_infix(expr, Precedence::Call)?;
                }
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::At(AtExpr {
                    expr: Box::new(expr),
                    span,
                }))
            }
            TokenKind::Comptime => {
                let start_span = self.advance().span; // cmptm
                let expr = self.parse_expression()?;
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::Comptime(ComptimeExpr {
                    expr: Box::new(expr),
                    span,
                }))
            }
            TokenKind::Do => {
                let start_span = self.advance().span; // do
                let mut params = Vec::new();
                if self.check(&TokenKind::Pipe) {
                    self.advance(); // |
                    while !self.check(&TokenKind::Pipe) && !self.is_at_end() {
                        // Check for terminators before trying to parse
                        if self.check(&TokenKind::End) || self.check(&TokenKind::RightBrace) {
                            break;
                        }
                        let name = self.expect_identifier_or_keyword()?;
                        // Check if there's a type annotation (identifier : type)
                        if self.check(&TokenKind::Colon) {
                            self.advance(); // :
                            let _type_ = self.parse_type()?; // parse but don't use for now
                        }
                        params.push(name);
                        if !self.check(&TokenKind::Comma) && !self.check(&TokenKind::Pipe) {
                            break;
                        }
                        if self.check(&TokenKind::Comma) {
                            self.advance(); // ,
                        }
                    }
                    if self.check(&TokenKind::Pipe) {
                        self.advance(); // |
                    }
                }
                let mut stmts = Vec::new();
                while !self.check(&TokenKind::End) && !self.is_at_end() {
                    stmts.push(self.parse_stmt()?);
                }
                self.expect(&TokenKind::End)?;
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::Closure(ClosureExpr {
                    params,
                    body: stmts,
                    span,
                }))
            }
            _ => {
                self.error("Expected expression");
                Err(())
            }
        }
    }

    fn parse_infix(&mut self, left: Expr, current_precedence: Precedence) -> Result<Expr, ()> {
        match self.peek().kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::EqualEqual
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or => {
                // get precedence bfr advncng so we use the operators precedence not the nxt tokens
                let precedence = self.get_precedence();
                let op = match self.advance().kind {
                    TokenKind::Plus => BinaryOp::Add,
                    TokenKind::Minus => BinaryOp::Sub,
                    TokenKind::Star => BinaryOp::Mul,
                    TokenKind::Slash => BinaryOp::Div,
                    TokenKind::Percent => BinaryOp::Mod,
                    TokenKind::EqualEqual => BinaryOp::Eq,
                    TokenKind::NotEqual => BinaryOp::Ne,
                    TokenKind::Less => BinaryOp::Lt,
                    TokenKind::LessEqual => BinaryOp::Le,
                    TokenKind::Greater => BinaryOp::Gt,
                    TokenKind::GreaterEqual => BinaryOp::Ge,
                    TokenKind::And => BinaryOp::And,
                    TokenKind::Or => BinaryOp::Or,
                    _ => return Err(()),
                };
                let right = self.parse_precedence(precedence.next())?;
                let span = Span::new(left.span().start(), right.span().end());
                Ok(Expr::Binary(BinaryExpr {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span,
                }))
            }
            TokenKind::Equal => {
                let start = left.span();
                self.advance();
                let value = self.parse_precedence(Precedence::Assignment)?;
                let span = Span::new(start.start(), value.span().end());
                Ok(Expr::Assignment(AssignmentExpr {
                    target: Box::new(left),
                    value: Box::new(value),
                    span,
                }))
            }
            TokenKind::LeftParen => {
                let start = left.span();
                self.advance(); // (
                let mut args = Vec::new();
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        args.push(self.parse_argument_expression()?);
                        if !self.check(&TokenKind::Comma) {
                            break;
                        }
                        self.advance(); // 
                    }
                }
                self.expect(&TokenKind::RightParen)?;
                let span = Span::new(start.start(), self.previous().span.end());
                Ok(Expr::Call(CallExpr {
                    callee: Box::new(left),
                    args,
                    generic_args: None,
                    span,
                }))
            }
            TokenKind::ColonColon => {
                // module access: Utils::helper
                let start = left.span();
                self.advance(); // ::
                let member = self.expect_identifier_or_keyword()?;
                let span = Span::new(start.start(), self.previous().span.end());
                Ok(Expr::ModuleAccess(ModuleAccessExpr {
                    module: match &left {
                        Expr::Variable(v) => v.name.clone(),
                        Expr::ModuleAccess(m) => format!("{}::{}", m.module, m.member),
                        _ => {
                            self.error("Expected module name before ::");
                            return Err(());
                        }
                    },
                    member,
                    span,
                }))
            }
            TokenKind::Dot => {
                let start = left.span();
                self.advance(); // 
                // hndl exists? as a spcl field
                // also handle tokenkind::exists if lexer tokenizes it as one token
                let field = if self.check(&TokenKind::Exists) {
                    self.advance(); // exists?
                    "exists?".to_string()
                } else {
                    let field_name = self.expect_identifier()?;
                    // chk if fllwd by ?
                    if self.check(&TokenKind::Question) {
                        self.advance(); // ?
                        if field_name == "exists" {
                            "exists?".to_string()
                        } else {
                            field_name
                        }
                    } else {
                        field_name
                    }
                };
                // exists? and value are always field accesses (for pointers), not method calls
                if field == "exists?" || field == "value" {
                    let span = Span::new(start.start(), self.previous().span.end());
                    Ok(Expr::FieldAccess(FieldAccessExpr {
                        object: Box::new(left),
                        field,
                        span,
                    }))
                } else if self.check(&TokenKind::LeftParen) {
                    // method call with parentheses
                    self.advance(); // (
                    let mut args = Vec::new();
                    if !self.check(&TokenKind::RightParen) {
                        loop {
                            args.push(self.parse_argument_expression()?);
                            if !self.check(&TokenKind::Comma) {
                                break;
                            }
                            self.advance(); // ,
                        }
                    }
                    self.expect(&TokenKind::RightParen)?;
                    let span = Span::new(start.start(), self.previous().span.end());
                    Ok(Expr::MethodCall(MethodCallExpr {
                        receiver: Box::new(left),
                        method: field,
                        args,
                        span,
                    }))
                } else {
                    // field access supports ptrvl 4 ptr dereferencing
                    // also spprts trgtxsts? 4 nllbl ptr chk
                    let span = Span::new(start.start(), self.previous().span.end());
                    Ok(Expr::FieldAccess(FieldAccessExpr {
                        object: Box::new(left),
                        field,
                        span,
                    }))
                }
            }
            TokenKind::Exists => {
                // exists? operator 4 nllbl pntrs
                let start = left.span();
                self.advance(); // exists?
                let span = Span::new(start.start(), self.previous().span.end());
                Ok(Expr::Exists(ExistsExpr {
                    expr: Box::new(left),
                    span,
                }))
            }
            TokenKind::LeftBracket => {
                // array indexing: arr[0]
                let start = left.span();
                self.advance(); // [
                let index = self.parse_expression()?;
                self.expect(&TokenKind::RightBracket)?;
                let span = Span::new(start.start(), self.previous().span.end());
                Ok(Expr::Index(IndexExpr {
                    array: Box::new(left),
                    index: Box::new(index),
                    span,
                }))
            }
            _ => {
                // chk if we can parse fn call w/o parens
                // works at Call precedence or Assignment precedence
                // and for any callable expression (Variable, FieldAccess, MethodCall, etc.)
                if (current_precedence == Precedence::Call || current_precedence == Precedence::Assignment) 
                    && !self.is_at_end() 
                {
                    // Check if the expression is callable (not just Variable)
                    let is_callable = matches!(
                        left,
                        Expr::Variable(_) 
                        | Expr::FieldAccess(_) 
                        | Expr::MethodCall(_)
                        | Expr::Call(_)
                        | Expr::Index(_)
                    );
                    
                    if is_callable && self.can_parse_call_without_parens() {
                        // Check if next token is a valid argument starter
                        // If it is, parse as a call without parentheses
                        return self.parse_call_without_parens(left);
                    }
                }
                Ok(left)
            }
        }
    }

    fn can_parse_call_without_parens(&self) -> bool {
        if self.is_at_end() {
            return false;
        }
        match self.peek().kind {
            // can't be an operator
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash
            | TokenKind::Percent | TokenKind::EqualEqual | TokenKind::NotEqual
            | TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater
            | TokenKind::GreaterEqual | TokenKind::And | TokenKind::Or
            | TokenKind::Equal | TokenKind::LeftParen | TokenKind::LeftBracket
            | TokenKind::Dot | TokenKind::Exists | TokenKind::Semicolon
            | TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace
            | TokenKind::Comma | TokenKind::Colon | TokenKind::End | TokenKind::Eof
            | TokenKind::Returns | TokenKind::Uses
            // can't be statement keywords
            | TokenKind::Return | TokenKind::If | TokenKind::Else | TokenKind::While
            | TokenKind::For | TokenKind::Break | TokenKind::Continue
            | TokenKind::Def | TokenKind::Struct | TokenKind::Trait | TokenKind::Implement
            | TokenKind::Module | TokenKind::Foreign | TokenKind::Require | TokenKind::Use
            | TokenKind::Declare => false,
            // can be: identifier, literal, do (closure), or other expression starters
            _ => true,
        }
    }

    fn parse_call_without_parens(&mut self, callee: Expr) -> Result<Expr, ()> {
        let start = callee.span();
        let mut args = Vec::new();
        
        // parse first arg (required for calls w/o parens)
        // Parse expression that stops at comma, end, semicolon, etc.
        let first_arg = self.parse_argument_expression()?;
        args.push(first_arg);
        
        // parse more args if comma separated
        while self.check(&TokenKind::Comma) {
            self.advance(); // consume the comma
            // After advancing past the comma, we should be at the next expression
            // parse_argument_expression will validate and parse it
            let arg = self.parse_argument_expression()?;
            args.push(arg);
        }
        
        let span = Span::new(start.start(), self.previous().span.end());
        Ok(Expr::Call(CallExpr {
            callee: Box::new(callee),
            args,
            generic_args: None,
            span,
        }))
    }
    
    // Parse an expression that can be used as a function argument
    // Stops at comma, end, semicolon, or other statement terminators
    fn parse_argument_expression(&mut self) -> Result<Expr, ()> {
        // Check for invalid starters
        if self.check(&TokenKind::Comma) || self.check(&TokenKind::End) 
            || self.check(&TokenKind::Semicolon) || self.check(&TokenKind::RightParen)
            || self.is_at_end() {
            self.error("Expected expression");
            return Err(());
        }
        
        // Parse expression with assignment precedence (lowest), which will parse
        // the full expression until we hit a comma or other terminator
        let expr = self.parse_precedence(Precedence::Assignment)?;
        
        Ok(expr)
    }

    fn get_precedence(&self) -> Precedence {
        match self.peek().kind {
            TokenKind::Equal => Precedence::Assignment,
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::EqualEqual | TokenKind::NotEqual => Precedence::Equality,
            TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual => {
                Precedence::Comparison
            }
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::LeftBrace | TokenKind::Dot | TokenKind::ColonColon => Precedence::Call,
            _ => Precedence::None,
        }
    }

    fn parse_uses(&mut self) -> Result<Vec<String>, ()> {
        let mut uses = Vec::new();
        loop {
            // check for terminators before trying to parse identifier
            if self.check(&TokenKind::End)
                || self.check(&TokenKind::LeftBrace)
                || self.check(&TokenKind::Equal)
                || self.is_at_end()
            {
                break;
            }
            let name = self.expect_identifier_or_keyword()?;
            uses.push(name);
            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance(); // ,
        }
        Ok(uses)
    }

    // helper methods
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<(), ()> {
        if self.check(kind) {
            self.advance();
            Ok(())
        } else {
            self.error(&format!("Expected {:?}", kind));
            Err(())
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ()> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            TokenKind::Char => {
                self.advance();
                Ok("char".to_string())
            }
            _ => {
                self.error("Expected identifier");
                Err(())
            }
        }
    }

    fn expect_identifier_or_keyword(&mut self) -> Result<String, ()> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            TokenKind::Size => {
                self.advance();
                Ok("size".to_string())
            }
            TokenKind::Int => {
                self.advance();
                Ok("int".to_string())
            }
            TokenKind::Float => {
                self.advance();
                Ok("float".to_string())
            }
            TokenKind::Bool => {
                self.advance();
                Ok("bool".to_string())
            }
            TokenKind::Char => {
                self.advance();
                Ok("char".to_string())
            }
            TokenKind::String => {
                self.advance();
                Ok("string".to_string())
            }
            TokenKind::Void => {
                self.advance();
                Ok("void".to_string())
            }
            _ => {
                self.error("Expected identifier");
                Err(())
            }
        }
    }

    fn error(&mut self, message: &str) {
        let span = self.peek().span;
        let diagnostic = Diagnostic::error(
            DiagnosticKind::SyntaxError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if matches!(self.previous().kind, TokenKind::Semicolon) {
                return;
            }
            match self.peek().kind {
                TokenKind::Def
                | TokenKind::Struct
                | TokenKind::Trait
                | TokenKind::Implement
                | TokenKind::Module
                | TokenKind::Foreign
                | TokenKind::Return
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For => return,
                _ => {}
            }
            self.advance();
        }
    }

    
}
