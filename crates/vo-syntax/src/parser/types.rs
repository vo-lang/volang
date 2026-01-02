//! Type parsing.

use super::{Parser, ParseResult};
use crate::ast::*;
use crate::token::TokenKind;
use vo_common::span::Span;

impl<'a> Parser<'a> {
    /// Parses a type expression.
    pub fn parse_type(&mut self) -> ParseResult<TypeExpr> {
        let start = self.current.span.start;
        
        let kind = match self.current.kind {
            TokenKind::Ident => {
                let ident = self.parse_ident()?;
                // Check for qualified type: pkg.Type
                if self.eat(TokenKind::Dot) {
                    let sel = self.parse_ident()?;
                    TypeExprKind::Selector(Box::new(SelectorTypeExpr { 
                        pkg: ident, 
                        sel 
                    }))
                } else {
                    TypeExprKind::Ident(ident)
                }
            }
            TokenKind::LBracket => {
                self.advance();
                if self.at(TokenKind::RBracket) {
                    // Slice type: []T
                    self.advance();
                    let elem = self.parse_type()?;
                    TypeExprKind::Slice(Box::new(elem))
                } else {
                    // Array type: [N]T
                    let len = self.parse_expr()?;
                    self.expect(TokenKind::RBracket)?;
                    let elem = self.parse_type()?;
                    TypeExprKind::Array(Box::new(ArrayType { len, elem }))
                }
            }
            TokenKind::Map => {
                self.advance();
                self.expect(TokenKind::LBracket)?;
                let key = self.parse_type()?;
                self.expect(TokenKind::RBracket)?;
                let value = self.parse_type()?;
                TypeExprKind::Map(Box::new(MapType { key, value }))
            }
            TokenKind::Chan => {
                self.advance();
                // Check for send-only: chan<-
                let dir = if self.eat(TokenKind::Arrow) {
                    ChanDir::Send
                } else {
                    ChanDir::Both
                };
                let elem = self.parse_type()?;
                TypeExprKind::Chan(Box::new(ChanType { dir, elem }))
            }
            TokenKind::Arrow => {
                // Receive-only channel: <-chan
                self.advance();
                self.expect(TokenKind::Chan)?;
                let elem = self.parse_type()?;
                TypeExprKind::Chan(Box::new(ChanType { dir: ChanDir::Recv, elem }))
            }
            TokenKind::Func => {
                self.advance();
                let func_type = self.parse_func_type_sig()?;
                TypeExprKind::Func(Box::new(func_type))
            }
            TokenKind::Struct => {
                self.advance();
                let struct_type = self.parse_struct_body()?;
                TypeExprKind::Struct(Box::new(struct_type))
            }
            TokenKind::Star => {
                self.advance();
                let inner = self.parse_type()?;
                TypeExprKind::Pointer(Box::new(inner))
            }
            TokenKind::Interface => {
                self.advance();
                self.expect(TokenKind::LBrace)?;
                let elems = self.parse_interface_elems()?;
                self.expect(TokenKind::RBrace)?;
                TypeExprKind::Interface(Box::new(InterfaceType { elems }))
            }
            _ => {
                self.error_expected("type");
                return Err(());
            }
        };
        
        Ok(self.make_type_expr(kind, Span::new(start, self.current.span.start)))
    }

    fn parse_func_type_sig(&mut self) -> ParseResult<FuncType> {
        self.expect(TokenKind::LParen)?;
        let params = self.parse_func_type_params()?;
        self.expect(TokenKind::RParen)?;
        
        // Parse result types
        let results = if self.at(TokenKind::LParen) {
            self.advance();
            let results = self.parse_func_type_params()?;
            self.expect(TokenKind::RParen)?;
            results
        } else if self.is_type_start() {
            let start = self.current.span.start;
            let ty = self.parse_type()?;
            let span = Span::new(start, self.current.span.start);
            vec![Param { names: Vec::new(), ty, span }]
        } else {
            Vec::new()
        };
        
        Ok(FuncType { params, results })
    }
    
    /// Parses function type parameters, which may have optional names.
    /// 
    /// Strategy (aligned with goscript):
    /// 1. Collect first param group as types (could be names or types)
    /// 2. Try to parse another type after the list
    /// 3. If successful: first list was names, continue parsing named params
    /// 4. If not: first list was types (anonymous params)
    fn parse_func_type_params(&mut self) -> ParseResult<Vec<Param>> {
        if self.at(TokenKind::RParen) {
            return Ok(Vec::new());
        }
        
        // Phase 1: Collect first parameter declaration (types that might be names)
        let first_group = self.collect_type_list()?;
        
        // Phase 2: Try to parse a type after the list
        if let Some(ty) = self.try_parse_type() {
            // Success: first_group was identifier names, ty is their type
            self.parse_named_params(first_group, ty)
        } else {
            // No type follows: first_group was anonymous type params
            Ok(self.types_to_anonymous_params(first_group))
        }
    }
    
    /// Collects a comma-separated list of types (stops at ')' or when non-type follows).
    fn collect_type_list(&mut self) -> ParseResult<Vec<TypeExpr>> {
        let mut types = Vec::new();
        loop {
            types.push(self.parse_type()?);
            if !self.eat(TokenKind::Comma) || self.at(TokenKind::RParen) {
                break;
            }
        }
        Ok(types)
    }
    
    /// Tries to parse a type without consuming tokens on failure.
    fn try_parse_type(&mut self) -> Option<TypeExpr> {
        if self.is_type_start() || self.at(TokenKind::Ellipsis) {
            self.eat(TokenKind::Ellipsis); // handle variadic
            self.parse_type().ok()
        } else {
            None
        }
    }
    
    /// Converts type expressions to identifier names (for named parameter parsing).
    fn types_to_idents(&self, types: Vec<TypeExpr>) -> Vec<Ident> {
        types.into_iter().filter_map(|t| {
            if let TypeExprKind::Ident(ident) = t.kind {
                Some(ident)
            } else {
                None // Non-ident types are invalid as names, skip
            }
        }).collect()
    }
    
    /// Converts type expressions to anonymous Params.
    fn types_to_anonymous_params(&self, types: Vec<TypeExpr>) -> Vec<Param> {
        types.into_iter().map(|ty| {
            let span = ty.span;
            Param { names: Vec::new(), ty, span }
        }).collect()
    }
    
    /// Parses remaining named parameters after first group.
    fn parse_named_params(&mut self, first_names: Vec<TypeExpr>, first_type: TypeExpr) -> ParseResult<Vec<Param>> {
        let mut params = Vec::new();
        
        // First param: names from first_names, type from first_type
        let names = self.types_to_idents(first_names);
        let span = if let Some(first) = names.first() {
            Span::new(first.span.start, first_type.span.end)
        } else {
            first_type.span
        };
        params.push(Param { names, ty: first_type, span });
        
        // Continue parsing remaining named params
        while self.eat(TokenKind::Comma) && !self.at(TokenKind::RParen) {
            let start = self.current.span.start;
            let names = self.parse_ident_list()?;
            self.eat(TokenKind::Ellipsis); // handle variadic
            let ty = self.parse_type()?;
            let span = Span::new(start, self.current.span.start);
            params.push(Param { names, ty, span });
        }
        
        Ok(params)
    }
    
    fn parse_struct_body(&mut self) -> ParseResult<StructType> {
        self.expect(TokenKind::LBrace)?;
        
        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            fields.push(self.parse_field()?);
            self.expect_semi();
        }
        
        self.expect(TokenKind::RBrace)?;
        Ok(StructType { fields })
    }

    fn parse_field(&mut self) -> ParseResult<Field> {
        let start = self.current.span.start;
        
        // Could be: names Type tag? | Type tag? (embedded)
        if self.at(TokenKind::Ident) && (self.peek_is(TokenKind::Comma) || self.peek_is(TokenKind::Ident) || self.is_type_start_peek()) {
            // Named field(s)
            let names = self.parse_ident_list()?;
            let ty = self.parse_type()?;
            let tag = if self.at(TokenKind::StringLit) || self.at(TokenKind::RawStringLit) {
                Some(self.parse_string_lit()?)
            } else {
                None
            };
            Ok(Field {
                names,
                ty,
                tag,
                span: Span::new(start, self.current.span.start),
            })
        } else {
            // Embedded field (just a type)
            let ty = self.parse_type()?;
            let tag = if self.at(TokenKind::StringLit) || self.at(TokenKind::RawStringLit) {
                Some(self.parse_string_lit()?)
            } else {
                None
            };
            Ok(Field {
                names: Vec::new(),
                ty,
                tag,
                span: Span::new(start, self.current.span.start),
            })
        }
    }

    fn is_type_start(&self) -> bool {
        matches!(
            self.current.kind,
            TokenKind::Ident
                | TokenKind::LBracket
                | TokenKind::Map
                | TokenKind::Chan
                | TokenKind::Arrow
                | TokenKind::Func
                | TokenKind::Struct
                | TokenKind::Star
                | TokenKind::Interface
        )
    }

    fn is_type_start_peek(&self) -> bool {
        matches!(
            self.peek.kind,
            TokenKind::Ident
                | TokenKind::LBracket
                | TokenKind::Map
                | TokenKind::Chan
                | TokenKind::Arrow
                | TokenKind::Func
                | TokenKind::Struct
                | TokenKind::Star
                | TokenKind::Interface
        )
    }
}
