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
        let name = self.expect_identifier()?;
        let generics = self.parse_generics()?;
        let params = self.parse_params()?;
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

    fn parse_params(&mut self) -> Result<Vec<Param>, ()> {
        if !self.check(&TokenKind::LeftParen) {
            return Ok(Vec::new());
        }
        self.advance(); // (

        let mut params = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                let name = self.expect_identifier()?;
                let type_ = if self.check(&TokenKind::Colon) {
                    self.advance(); // :
                    self.parse_type()?
                } else {
                    // 4 trait methods type can be omttd
                    // use void as placeholder will be resolved drng smntc anlyss
                    Type::Primitive(crate::core::ast::types::PrimitiveType::Void)
                };
                let span = self.previous().span;
                params.push(Param {
                    name,
                    type_,
                    span,
                });

                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance(); // 
            }
        }
        self.expect(&TokenKind::RightParen)?;
        Ok(params)
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
            let name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
        let generics = self.parse_generics()?;
        let mut fields = Vec::new();

        while !self.check(&TokenKind::End) && !self.is_at_end() {
            let field_name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
        let params = self.parse_params()?;
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
        self.expect(&TokenKind::Identifier("for".to_string()))?;
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
        let name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
        let params = self.parse_params()?;
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
            let name = self.expect_identifier()?;
            path.push(name);
            if !self.check(&TokenKind::Dot) {
                break;
            }
            self.advance(); // 
        }
        let span = Span::new(start_span.start(), self.previous().span.end());
        Ok(Use { path, span })
    }

    fn parse_declare(&mut self) -> Result<Item, ()> {
        let start_span = self.advance().span; // declare
        if self.check(&TokenKind::Struct) {
            self.advance();
            let name = self.expect_identifier()?;
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
        let name = self.expect_identifier()?;
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
        match self.peek().kind {
            TokenKind::Void => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Void))
            }
            TokenKind::Byte => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Byte))
            }
            TokenKind::Int => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Int))
            }
            TokenKind::Long => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Long))
            }
            TokenKind::Size => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Size))
            }
            TokenKind::Float => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Float))
            }
            TokenKind::Bool => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Bool))
            }
            TokenKind::Char => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Char))
            }
            TokenKind::String => {
                self.advance();
                Ok(Type::Named(NamedType { name: "string".to_string(), generics: Vec::new() }))
            }
            TokenKind::Ref => {
                self.advance();
                let pointee = self.parse_type()?;
                Ok(Type::ref_(pointee))
            }
            TokenKind::RefNullable => {
                self.advance();
                let pointee = self.parse_type()?;
                Ok(Type::ref_nullable(pointee))
            }
            TokenKind::Identifier(_) => {
                let name = if let TokenKind::Identifier(n) = self.advance().kind.clone() {
                    n
                } else {
                    return Err(());
                };
                let generics = if self.check(&TokenKind::LeftBracket) {
                    self.advance(); // [
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
                    types
                } else {
                    Vec::new()
                };
                Ok(Type::Named(NamedType { name, generics }))
            }
            _ => {
                self.error("Expected type");
                Err(())
            }
        }
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
            stmts.push(self.parse_stmt()?);
        }
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
        let _comptime = self.check(&TokenKind::Comptime);
        if _comptime {
            self.advance();
        }
        let mutable = self.check(&TokenKind::Mut);
        if mutable {
            self.advance();
        }
        let name = self.expect_identifier()?;
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

        while precedence <= self.get_precedence() && !self.check(&TokenKind::Semicolon) {
            expr = self.parse_infix(expr)?;
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
                let span = self.previous().span;
                Ok(Expr::Variable(VariableExpr { name, span }))
            }
            TokenKind::LeftParen => {
                self.advance(); // (
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::RightParen)?;
                Ok(expr)
            }
            TokenKind::LeftBrace => {
                let start_span = self.advance().span; // {
                let mut stmts = Vec::new();
                while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                    stmts.push(self.parse_stmt()?);
                }
                let expr = if !stmts.is_empty() && self.check(&TokenKind::RightBrace) {
                    // last statement might be an expression
                    None
                } else {
                    None
                };
                self.expect(&TokenKind::RightBrace)?;
                let span = Span::new(start_span.start(), self.previous().span.end());
                Ok(Expr::Block(BlockExpr { stmts, expr, span }))
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
                let expr = self.parse_precedence(Precedence::Unary)?;
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
                        let name = self.expect_identifier()?;
                        params.push(name);
                        if !self.check(&TokenKind::Comma) && !self.check(&TokenKind::Pipe) {
                            break;
                        }
                        if self.check(&TokenKind::Comma) {
                            self.advance(); // 
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

    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ()> {
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
                self.advance(); // =
                let value = self.parse_precedence(Precedence::Assignment.next())?;
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
                        args.push(self.parse_expression()?);
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
                    span,
                }))
            }
            TokenKind::LeftBracket => {
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
                // chk if its a method call
                if self.check(&TokenKind::LeftParen) {
                    self.advance(); // (
                    let mut args = Vec::new();
                    if !self.check(&TokenKind::RightParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if !self.check(&TokenKind::Comma) {
                                break;
                            }
                            self.advance(); // 
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
            _ => Ok(left),
        }
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
            TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }

    fn parse_uses(&mut self) -> Result<Vec<String>, ()> {
        let mut uses = Vec::new();
        loop {
            let name = self.expect_identifier()?;
            uses.push(name);
            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance(); // 
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
        if let TokenKind::Identifier(name) = self.peek().kind.clone() {
            self.advance();
            Ok(name)
        } else {
            self.error("Expected identifier");
            Err(())
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
