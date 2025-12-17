//! Declaration parsing.

use super::{Parser, ParseResult};
use crate::ast::*;
use crate::token::TokenKind;
use gox_common::span::Span;

impl<'a> Parser<'a> {
    /// Parses a top-level declaration.
    pub fn parse_top_level_decl(&mut self) -> ParseResult<Decl> {
        match self.current.kind {
            TokenKind::Var => self.parse_var_decl().map(Decl::Var),
            TokenKind::Const => self.parse_const_decl().map(Decl::Const),
            TokenKind::Type => self.parse_type_decl().map(Decl::Type),
            TokenKind::Func => self.parse_func_decl().map(Decl::Func),
            TokenKind::Interface => self.parse_interface_decl().map(Decl::Interface),
            _ => {
                self.error_expected("declaration");
                Err(())
            }
        }
    }

    /// Parses a var declaration.
    pub fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Var)?;
        
        let specs = if self.eat(TokenKind::LParen) {
            let mut specs = Vec::new();
            while !self.at(TokenKind::RParen) && !self.at_eof() {
                specs.push(self.parse_var_spec()?);
                self.expect_semi();
            }
            self.expect(TokenKind::RParen)?;
            specs
        } else {
            vec![self.parse_var_spec()?]
        };
        
        self.expect_semi();
        Ok(VarDecl {
            specs,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_var_spec(&mut self) -> ParseResult<VarSpec> {
        let start = self.current.span.start;
        let names = self.parse_ident_list()?;
        
        // Type is optional if there's an initializer
        let ty = if !self.at(TokenKind::Eq) && !self.at(TokenKind::Semicolon) && !self.at(TokenKind::RParen) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let values = if self.eat(TokenKind::Eq) {
            self.parse_expr_list()?
        } else {
            Vec::new()
        };
        
        Ok(VarSpec {
            names,
            ty,
            values,
            span: Span::new(start, self.current.span.start),
        })
    }

    /// Parses a const declaration.
    pub fn parse_const_decl(&mut self) -> ParseResult<ConstDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Const)?;
        
        let specs = if self.eat(TokenKind::LParen) {
            let mut specs = Vec::new();
            while !self.at(TokenKind::RParen) && !self.at_eof() {
                specs.push(self.parse_const_spec()?);
                self.expect_semi();
            }
            self.expect(TokenKind::RParen)?;
            specs
        } else {
            vec![self.parse_const_spec()?]
        };
        
        self.expect_semi();
        Ok(ConstDecl {
            specs,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_const_spec(&mut self) -> ParseResult<ConstSpec> {
        let start = self.current.span.start;
        let names = self.parse_ident_list()?;
        
        // Type is optional
        let ty = if !self.at(TokenKind::Eq) && !self.at(TokenKind::Semicolon) && !self.at(TokenKind::RParen) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        // Values may be omitted for iota continuation
        let values = if self.eat(TokenKind::Eq) {
            self.parse_expr_list()?
        } else {
            Vec::new()
        };
        
        Ok(ConstSpec {
            names,
            ty,
            values,
            span: Span::new(start, self.current.span.start),
        })
    }

    /// Parses a type declaration.
    pub fn parse_type_decl(&mut self) -> ParseResult<TypeDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Type)?;
        let name = self.parse_ident()?;
        let ty = self.parse_type()?;
        self.expect_semi();
        Ok(TypeDecl {
            name,
            ty,
            span: Span::new(start, self.current.span.start),
        })
    }

    /// Parses a function declaration.
    /// A function without body (ending with `;`) is a native function.
    pub fn parse_func_decl(&mut self) -> ParseResult<FuncDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Func)?;
        
        // Check for receiver
        let receiver = if self.at(TokenKind::LParen) {
            Some(self.parse_receiver()?)
        } else {
            None
        };
        
        let name = self.parse_ident()?;
        let sig = self.parse_func_sig()?;
        
        // Body is optional - no body means native/external implementation
        let body = if self.at(TokenKind::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect_semi();
            None
        };
        
        Ok(FuncDecl {
            receiver,
            name,
            sig,
            body,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_receiver(&mut self) -> ParseResult<Receiver> {
        let start = self.current.span.start;
        self.expect(TokenKind::LParen)?;
        let name = self.parse_ident()?;
        let ty = self.parse_ident()?;
        self.expect(TokenKind::RParen)?;
        Ok(Receiver {
            name,
            ty,
            span: Span::new(start, self.current.span.start),
        })
    }

    pub(crate) fn parse_func_sig(&mut self) -> ParseResult<FuncSig> {
        let start = self.current.span.start;
        self.expect(TokenKind::LParen)?;
        let (params, variadic) = self.parse_param_list()?;
        self.expect(TokenKind::RParen)?;
        let results = self.parse_result_type()?;
        Ok(FuncSig {
            params,
            results,
            variadic,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_param_list(&mut self) -> ParseResult<(Vec<Param>, bool)> {
        let mut params = Vec::new();
        let mut variadic = false;
        
        if self.at(TokenKind::RParen) {
            return Ok((params, variadic));
        }
        
        loop {
            let param_start = self.current.span.start;
            
            // Check for variadic: ...Type
            if self.eat(TokenKind::Ellipsis) {
                variadic = true;
                let ty = self.parse_type()?;
                params.push(Param {
                    names: Vec::new(),
                    ty,
                    span: Span::new(param_start, self.current.span.start),
                });
                break;
            }
            
            // Parse names
            let names = self.parse_ident_list()?;
            
            // Check for variadic after names: name ...Type
            if self.eat(TokenKind::Ellipsis) {
                variadic = true;
                let ty = self.parse_type()?;
                params.push(Param {
                    names,
                    ty,
                    span: Span::new(param_start, self.current.span.start),
                });
                break;
            }
            
            let ty = self.parse_type()?;
            params.push(Param {
                names,
                ty,
                span: Span::new(param_start, self.current.span.start),
            });
            
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        
        Ok((params, variadic))
    }

    fn parse_result_type(&mut self) -> ParseResult<Vec<ResultParam>> {
        // Multiple results in parentheses
        if self.at(TokenKind::LParen) {
            self.advance();
            let mut results = Vec::new();
            while !self.at(TokenKind::RParen) && !self.at_eof() {
                let start = self.current.span.start;
                
                // Try to parse as named result: name type
                // or unnamed result: type
                if self.at(TokenKind::Ident) {
                    let first = self.parse_ident()?;
                    
                    // Check if next token is a type (meaning first was a name)
                    if self.at_type_start() {
                        let ty = self.parse_type()?;
                        results.push(ResultParam {
                            name: Some(first),
                            ty,
                            span: Span::new(start, self.current.span.start),
                        });
                    } else {
                        // First was actually the type (identifier type like `int`)
                        let ty = TypeExpr {
                            kind: TypeExprKind::Ident(first),
                            span: first.span,
                        };
                        results.push(ResultParam {
                            name: None,
                            ty,
                            span: Span::new(start, self.current.span.start),
                        });
                    }
                } else {
                    // Non-identifier type (like *int, []int, etc.)
                    let ty = self.parse_type()?;
                    results.push(ResultParam {
                        name: None,
                        ty,
                        span: Span::new(start, self.current.span.start),
                    });
                }
                
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            Ok(results)
        } else if self.at(TokenKind::LBrace) || self.at(TokenKind::Semicolon) || self.at_eof() {
            // No result type
            Ok(Vec::new())
        } else {
            // Single result type (always unnamed)
            let start = self.current.span.start;
            let ty = self.parse_type()?;
            Ok(vec![ResultParam {
                name: None,
                ty,
                span: Span::new(start, self.current.span.start),
            }])
        }
    }
    
    /// Checks if current token could start a type expression.
    fn at_type_start(&self) -> bool {
        matches!(self.current.kind, 
            TokenKind::Ident | TokenKind::LBracket | 
            TokenKind::Map | TokenKind::Chan | TokenKind::Func |
            TokenKind::Struct | TokenKind::Interface
        )
    }

    /// Parses an interface declaration.
    pub fn parse_interface_decl(&mut self) -> ParseResult<InterfaceDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Interface)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::LBrace)?;
        let elems = self.parse_interface_elems()?;
        self.expect(TokenKind::RBrace)?;
        self.expect_semi();
        Ok(InterfaceDecl {
            name,
            elems,
            span: Span::new(start, self.current.span.start),
        })
    }

    pub(crate) fn parse_interface_elems(&mut self) -> ParseResult<Vec<InterfaceElem>> {
        let mut elems = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            let elem_start = self.current.span.start;
            let name = self.parse_ident()?;
            
            if self.at(TokenKind::LParen) {
                // Method specification
                let sig = self.parse_func_sig()?;
                elems.push(InterfaceElem::Method(MethodSpec {
                    name,
                    sig: sig.clone(),
                    span: Span::new(elem_start, self.current.span.start),
                }));
            } else {
                // Embedded interface
                elems.push(InterfaceElem::Embedded(name));
            }
            self.expect_semi();
        }
        Ok(elems)
    }
}
