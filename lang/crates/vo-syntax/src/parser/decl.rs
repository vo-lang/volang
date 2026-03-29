#![allow(clippy::result_unit_err)]
//! Declaration parsing.

use super::{ParseResult, Parser};
use crate::ast::*;
use crate::token::TokenKind;
use vo_common::span::Span;

impl<'a> Parser<'a> {
    /// Parses a top-level declaration.
    pub fn parse_top_level_decl(&mut self) -> ParseResult<Decl> {
        match self.current.kind {
            TokenKind::Var => self.parse_var_decl().map(Decl::Var),
            TokenKind::Const => self.parse_const_decl().map(Decl::Const),
            TokenKind::Type => self.parse_type_decl().map(Decl::Type),
            TokenKind::Func => self.parse_func_decl().map(Decl::Func),
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
        let ty = if !self.at(TokenKind::Eq)
            && !self.at(TokenKind::Semicolon)
            && !self.at(TokenKind::RParen)
        {
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
        let ty = if !self.at(TokenKind::Eq)
            && !self.at(TokenKind::Semicolon)
            && !self.at(TokenKind::RParen)
        {
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
    /// Supports both `type X Y` (new type) and `type X = Y` (type alias).
    pub fn parse_type_decl(&mut self) -> ParseResult<TypeDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Type)?;
        let name = self.parse_ident()?;
        let is_alias = self.eat(TokenKind::Eq);
        let ty = self.parse_type()?;
        self.expect_semi();
        Ok(TypeDecl {
            name,
            is_alias,
            ty,
            span: Span::new(start, self.current.span.start),
        })
    }

    /// Parses a function declaration.
    /// A function without body (ending with `;`) is an extern function.
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

        // Body is optional - no body means extern function (implemented outside Vo)
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

    /// Parses a method receiver.
    /// Supports both named and anonymous receivers:
    /// - `(r T)` or `(r *T)` - named receiver
    /// - `(T)` or `(*T)` - anonymous receiver (Go allows this when receiver is unused)
    fn parse_receiver(&mut self) -> ParseResult<Receiver> {
        let start = self.current.span.start;
        self.expect(TokenKind::LParen)?;

        // Check for anonymous pointer receiver: (*T)
        if self.at(TokenKind::Star) {
            self.advance();
            let ty = self.parse_ident()?;
            self.expect(TokenKind::RParen)?;
            return Ok(Receiver {
                name: None,
                ty,
                is_pointer: true,
                span: Span::new(start, self.current.span.start),
            });
        }

        // Parse the first identifier
        let first_ident = self.parse_ident()?;

        // Determine if this is named or anonymous receiver by looking at next token:
        // - `)` means anonymous: (T)
        // - `*` means named pointer: (r *T)
        // - ident means named: (r T)
        if self.at(TokenKind::RParen) {
            // Anonymous receiver: (T)
            self.advance();
            Ok(Receiver {
                name: None,
                ty: first_ident,
                is_pointer: false,
                span: Span::new(start, self.current.span.start),
            })
        } else if self.at(TokenKind::Star) {
            // Named pointer receiver: (r *T)
            self.advance();
            let ty = self.parse_ident()?;
            self.expect(TokenKind::RParen)?;
            Ok(Receiver {
                name: Some(first_ident),
                ty,
                is_pointer: true,
                span: Span::new(start, self.current.span.start),
            })
        } else {
            // Named receiver: (r T)
            let ty = self.parse_ident()?;
            self.expect(TokenKind::RParen)?;
            Ok(Receiver {
                name: Some(first_ident),
                ty,
                is_pointer: false,
                span: Span::new(start, self.current.span.start),
            })
        }
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

    /// Parses function parameters. Reuses parse_func_type_params and detects variadic.
    fn parse_param_list(&mut self) -> ParseResult<(Vec<Param>, bool)> {
        if self.at(TokenKind::RParen) {
            return Ok((Vec::new(), false));
        }

        // Handle leading variadic: ...Type
        if self.eat(TokenKind::Ellipsis) {
            let start = self.current.span.start;
            let ty = self.parse_type()?;
            let param = Param {
                names: Vec::new(),
                ty,
                span: Span::new(start, self.current.span.start),
            };
            return Ok((vec![param], true));
        }

        // Reuse the strategy from parse_func_type_params
        let first_group = self.collect_type_list()?;

        // Check for variadic after first group
        if self.eat(TokenKind::Ellipsis) {
            let ty = self.parse_type()?;
            let names = self.types_to_idents(first_group);
            let span = ty.span;
            return Ok((vec![Param { names, ty, span }], true));
        }

        // Try to parse type after the list
        if let Some(ty) = self.try_parse_type() {
            // first_group was names, reuse parse_named_params
            self.parse_named_params(first_group, ty)
        } else {
            // first_group was anonymous types
            Ok((self.types_to_anonymous_params(first_group), false))
        }
    }

    /// Parses result types for function signatures.
    /// Supports Go-style named returns: (a, b int, c bool) or (err error)
    /// Also supports unnamed returns: (int, bool) or int
    ///
    /// Strategy (aligned with parse_func_type_params):
    /// 1. Collect first group as types (could be names or types)
    /// 2. Try to parse a type after the group
    /// 3. If successful: first group was names, continue parsing named results
    /// 4. If not: first group was types (anonymous results)
    fn parse_result_type(&mut self) -> ParseResult<Vec<ResultParam>> {
        // Multiple results in parentheses
        if self.at(TokenKind::LParen) {
            self.advance();

            if self.at(TokenKind::RParen) {
                self.advance();
                return Ok(Vec::new());
            }

            // Phase 1: Collect first group (types that might be names)
            let first_group = self.collect_result_group()?;

            // Phase 2: Try to parse a type after the group
            if let Some(ty) = self.try_parse_type() {
                // Success: first_group was identifier names, ty is their type
                let results = self.parse_named_results(first_group, ty)?;
                self.expect(TokenKind::RParen)?;
                Ok(results)
            } else {
                // No type follows: first_group was anonymous type results
                let results = self.types_to_anonymous_results(first_group);
                self.expect(TokenKind::RParen)?;
                Ok(results)
            }
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

    /// Collects a comma-separated group of types (stops at ')' or when a type follows idents).
    /// For result parsing: collects until we can determine if these are names or types.
    fn collect_result_group(&mut self) -> ParseResult<Vec<TypeExpr>> {
        let mut types = Vec::new();
        loop {
            types.push(self.parse_type()?);
            if !self.eat(TokenKind::Comma) || self.at(TokenKind::RParen) {
                break;
            }
        }
        Ok(types)
    }

    /// Converts type expressions to named ResultParams with the given type.
    /// Continues parsing more named result groups.
    fn parse_named_results(
        &mut self,
        first_names: Vec<TypeExpr>,
        first_type: TypeExpr,
    ) -> ParseResult<Vec<ResultParam>> {
        let mut results = Vec::new();

        // Convert first group to named results
        for type_expr in first_names {
            if let TypeExprKind::Ident(ident) = type_expr.kind {
                let span = Span::new(ident.span.start, first_type.span.end);
                results.push(ResultParam {
                    name: Some(ident),
                    ty: first_type.clone(),
                    span,
                });
            } else {
                // Non-ident in name position is invalid, but continue parsing
                let span = type_expr.span;
                results.push(ResultParam {
                    name: None,
                    ty: type_expr,
                    span,
                });
            }
        }

        // Continue parsing more named result groups
        while self.eat(TokenKind::Comma) && !self.at(TokenKind::RParen) {
            let start = self.current.span.start;

            // Collect names
            let names = self.parse_ident_list()?;

            // Parse their type
            let ty = self.parse_type()?;

            for name in names {
                results.push(ResultParam {
                    name: Some(name),
                    ty: ty.clone(),
                    span: Span::new(start, self.current.span.start),
                });
            }
        }

        Ok(results)
    }

    /// Converts type expressions to anonymous ResultParams.
    fn types_to_anonymous_results(&self, types: Vec<TypeExpr>) -> Vec<ResultParam> {
        types
            .into_iter()
            .map(|ty| {
                let span = ty.span;
                ResultParam {
                    name: None,
                    ty,
                    span,
                }
            })
            .collect()
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
            } else if self.at(TokenKind::Dot) {
                // Qualified embedded interface (pkg.Type)
                self.advance(); // consume '.'
                let type_name = self.parse_ident()?;
                elems.push(InterfaceElem::EmbeddedQualified {
                    pkg: name,
                    name: type_name,
                    span: Span::new(elem_start, self.current.span.start),
                });
            } else {
                // Simple embedded interface
                elems.push(InterfaceElem::Embedded(name));
            }
            self.expect_semi();
        }
        Ok(elems)
    }
}
