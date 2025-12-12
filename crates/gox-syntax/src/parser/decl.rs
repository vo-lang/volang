//! Declaration parsing for GoX.
//!
//! Handles top-level declarations: var, const, type, func, interface, implements.

use super::Parser;
use crate::ast::*;
use crate::token::TokenKind;
use gox_common::GoxResult;

impl<'a> Parser<'a> {
    // ═══════════════════════════════════════════════════════════════════════
    // Top-Level Declaration Dispatch
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse a top-level declaration.
    pub(super) fn parse_top_decl(&mut self) -> GoxResult<TopDecl> {
        match &self.current.kind {
            TokenKind::Var => Ok(TopDecl::Var(self.parse_var_decl()?)),
            TokenKind::Const => Ok(TopDecl::Const(self.parse_const_decl()?)),
            TokenKind::Type => Ok(TopDecl::Type(self.parse_type_decl()?)),
            TokenKind::Interface => Ok(TopDecl::Interface(self.parse_interface_decl()?)),
            TokenKind::Implements => Ok(TopDecl::Implements(self.parse_implements_decl()?)),
            TokenKind::Func => Ok(TopDecl::Func(self.parse_func_decl()?)),
            _ => Err(self
                .error_msg("expected declaration (var, const, type, func, interface, implements)")),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Variable Declaration (§5.1)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `var name Type? (= Expr)?;`
    pub(super) fn parse_var_decl(&mut self) -> GoxResult<VarDecl> {
        let start = self.expect(&TokenKind::Var)?;
        let mut specs = vec![self.parse_var_spec()?];

        // Handle multiple specs with comma
        while self.eat(&TokenKind::Comma) {
            specs.push(self.parse_var_spec()?);
        }

        self.expect_semi()?;

        let end = specs.last().unwrap().span;
        Ok(VarDecl {
            specs,
            span: start.to(&end),
        })
    }

    fn parse_var_spec(&mut self) -> GoxResult<VarSpec> {
        let name = self.parse_ident()?;
        let start = name.span;

        // Optional type
        let ty = if !self.cur_is(&TokenKind::Assign)
            && !self.cur_is(&TokenKind::Comma)
            && !self.cur_is(&TokenKind::Semi)
        {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Optional value
        let value = if self.eat(&TokenKind::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = value
            .as_ref()
            .map(|v| v.span())
            .or(ty.as_ref().map(|t| t.span()))
            .unwrap_or(start);

        Ok(VarSpec {
            name,
            ty,
            value,
            span: start.to(&end),
        })
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Constant Declaration (§5.2)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `const name Type? = Expr;`
    pub(super) fn parse_const_decl(&mut self) -> GoxResult<ConstDecl> {
        let start = self.expect(&TokenKind::Const)?;
        let mut specs = vec![self.parse_const_spec()?];

        while self.eat(&TokenKind::Comma) {
            specs.push(self.parse_const_spec()?);
        }

        self.expect_semi()?;

        let end = specs.last().unwrap().span;
        Ok(ConstDecl {
            specs,
            span: start.to(&end),
        })
    }

    fn parse_const_spec(&mut self) -> GoxResult<ConstSpec> {
        let name = self.parse_ident()?;
        let start = name.span;

        // Optional type
        let ty = if !self.cur_is(&TokenKind::Assign) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Required value
        self.expect(&TokenKind::Assign)?;
        let value = self.parse_expr()?;

        Ok(ConstSpec {
            span: start.to(&value.span()),
            name,
            ty,
            value,
        })
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Type Declaration (§5.4)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `type Name Type;`
    pub(super) fn parse_type_decl(&mut self) -> GoxResult<TypeDecl> {
        let start = self.expect(&TokenKind::Type)?;
        let name = self.parse_ident()?;
        let ty = self.parse_type()?;
        self.expect_semi()?;

        Ok(TypeDecl {
            span: start.to(&ty.span()),
            name,
            ty,
        })
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Interface Declaration (§7.1)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `interface Name { ... };`
    pub(super) fn parse_interface_decl(&mut self) -> GoxResult<InterfaceDecl> {
        let start = self.expect(&TokenKind::Interface)?;
        let name = self.parse_ident()?;
        self.expect(&TokenKind::LBrace)?;

        let mut elements = Vec::new();
        while !self.cur_is(&TokenKind::RBrace) && !self.at_eof() {
            elements.push(self.parse_interface_elem()?);
        }

        let end = self.expect(&TokenKind::RBrace)?;
        self.expect_semi()?;

        Ok(InterfaceDecl {
            name,
            elements,
            span: start.to(&end),
        })
    }

    fn parse_interface_elem(&mut self) -> GoxResult<InterfaceElem> {
        let name = self.parse_ident()?;

        if self.cur_is(&TokenKind::LParen) {
            // Method: `Name(params) Result?;`
            let method_start = name.span;
            self.next_token(); // eat (

            let params = self.parse_param_list()?;
            self.expect(&TokenKind::RParen)?;

            let result = self.parse_optional_result()?;
            self.expect_semi()?;

            let end = result.as_ref().map(|r| r.span()).unwrap_or(method_start);
            Ok(InterfaceElem::Method(MethodSpec {
                name,
                params,
                result,
                span: method_start.to(&end),
            }))
        } else {
            // Embedded interface: `Name;`
            self.expect_semi()?;
            Ok(InterfaceElem::Embedded(name))
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Implements Declaration (§7.5)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `implements TypeName : Interface, Interface, ...;`
    pub(super) fn parse_implements_decl(&mut self) -> GoxResult<ImplementsDecl> {
        let start = self.expect(&TokenKind::Implements)?;
        let type_name = self.parse_ident()?;
        self.expect(&TokenKind::Colon)?;

        let mut interfaces = vec![self.parse_ident()?];
        while self.eat(&TokenKind::Comma) {
            interfaces.push(self.parse_ident()?);
        }

        let end = interfaces.last().unwrap().span;
        self.expect_semi()?;

        Ok(ImplementsDecl {
            type_name,
            interfaces,
            span: start.to(&end),
        })
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Function Declaration (§7.4)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse: `func (receiver)? Name(params) Result? Block`
    pub(super) fn parse_func_decl(&mut self) -> GoxResult<FuncDecl> {
        let start = self.expect(&TokenKind::Func)?;

        // Optional receiver
        let receiver = if self.cur_is(&TokenKind::LParen) {
            Some(self.parse_receiver()?)
        } else {
            None
        };

        let name = self.parse_ident()?;

        // Parameters
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;

        // Optional result
        let result = self.parse_optional_result()?;

        // Body
        let body = self.parse_block()?;

        Ok(FuncDecl {
            span: start.to(&body.span),
            receiver,
            name,
            params,
            result,
            body,
        })
    }

    fn parse_receiver(&mut self) -> GoxResult<Receiver> {
        let start = self.expect(&TokenKind::LParen)?;
        let name = self.parse_ident()?;
        let ty = self.parse_ident()?; // Must be a named type
        let end = self.expect(&TokenKind::RParen)?;

        Ok(Receiver {
            name,
            ty,
            span: start.to(&end),
        })
    }

    fn parse_param_list(&mut self) -> GoxResult<Vec<Param>> {
        let mut params = Vec::new();

        if self.cur_is(&TokenKind::RParen) {
            return Ok(params);
        }

        params.push(self.parse_param()?);
        while self.eat(&TokenKind::Comma) {
            params.push(self.parse_param()?);
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> GoxResult<Param> {
        let name = self.parse_ident()?;
        let ty = self.parse_type()?;
        Ok(Param {
            span: name.span.to(&ty.span()),
            name,
            ty,
        })
    }

    fn parse_optional_result(&mut self) -> GoxResult<Option<ResultType>> {
        // Check for no result (block or semicolon follows)
        if self.cur_is(&TokenKind::LBrace) || self.cur_is(&TokenKind::Semi) {
            return Ok(None);
        }

        // Check for tuple result: (Type, Type, ...)
        if self.cur_is(&TokenKind::LParen) {
            let start = self.current.span;
            self.next_token();

            let mut types = vec![self.parse_type()?];
            while self.eat(&TokenKind::Comma) {
                types.push(self.parse_type()?);
            }

            let end = self.expect(&TokenKind::RParen)?;
            return Ok(Some(ResultType::Tuple(types, start.to(&end))));
        }

        // Single type result
        let ty = self.parse_type()?;
        Ok(Some(ResultType::Single(ty)))
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Type Parsing (§6)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse a type.
    pub(super) fn parse_type(&mut self) -> GoxResult<Type> {
        match &self.current.kind {
            TokenKind::Ident(_) => {
                let id = self.parse_ident()?;
                Ok(Type::Named(id))
            }
            TokenKind::LBracket => self.parse_array_or_slice_type(),
            TokenKind::Map => self.parse_map_type(),
            TokenKind::Func => self.parse_func_type(),
            TokenKind::Struct => self.parse_struct_type(),
            _ => Err(self.error_msg("expected type")),
        }
    }

    fn parse_array_or_slice_type(&mut self) -> GoxResult<Type> {
        let start = self.expect(&TokenKind::LBracket)?;

        if self.cur_is(&TokenKind::RBracket) {
            // Slice: []T
            self.next_token();
            let elem = self.parse_type()?;
            Ok(Type::Slice(Box::new(SliceType {
                span: start.to(&elem.span()),
                elem,
            })))
        } else {
            // Array: [N]T
            let len = match &self.current.kind {
                TokenKind::Int(n) => {
                    let n = *n;
                    self.next_token();
                    n
                }
                _ => return Err(self.error_msg("expected array length")),
            };
            self.expect(&TokenKind::RBracket)?;
            let elem = self.parse_type()?;
            Ok(Type::Array(Box::new(ArrayType {
                len,
                span: start.to(&elem.span()),
                elem,
            })))
        }
    }

    fn parse_map_type(&mut self) -> GoxResult<Type> {
        let start = self.expect(&TokenKind::Map)?;
        self.expect(&TokenKind::LBracket)?;
        let key = self.parse_type()?;
        self.expect(&TokenKind::RBracket)?;
        let value = self.parse_type()?;

        Ok(Type::Map(Box::new(MapType {
            key,
            span: start.to(&value.span()),
            value,
        })))
    }

    fn parse_func_type(&mut self) -> GoxResult<Type> {
        let start = self.expect(&TokenKind::Func)?;
        self.expect(&TokenKind::LParen)?;

        let mut params = Vec::new();
        if !self.cur_is(&TokenKind::RParen) {
            params.push(self.parse_type()?);
            while self.eat(&TokenKind::Comma) {
                params.push(self.parse_type()?);
            }
        }

        let rparen = self.expect(&TokenKind::RParen)?;
        let result = self.parse_optional_result()?;

        let end = result.as_ref().map(|r| r.span()).unwrap_or(rparen);

        Ok(Type::Func(Box::new(FuncType {
            params,
            result: result.map(Box::new),
            span: start.to(&end),
        })))
    }

    fn parse_struct_type(&mut self) -> GoxResult<Type> {
        let start = self.expect(&TokenKind::Struct)?;
        self.expect(&TokenKind::LBrace)?;

        let mut fields = Vec::new();
        while !self.cur_is(&TokenKind::RBrace) && !self.at_eof() {
            fields.push(self.parse_field_decl()?);
        }

        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Type::Struct(Box::new(StructType {
            fields,
            span: start.to(&end),
        })))
    }

    fn parse_field_decl(&mut self) -> GoxResult<FieldDecl> {
        let name = self.parse_ident()?;
        let ty = self.parse_type()?;

        // Optional tag
        let tag = if let TokenKind::String(s) = &self.current.kind {
            let s = s.clone();
            self.next_token();
            Some(s)
        } else {
            None
        };

        self.expect_semi()?;

        let end = tag.as_ref().map(|_| self.current.span).unwrap_or(ty.span());

        Ok(FieldDecl {
            span: name.span.to(&end),
            name,
            ty,
            tag,
        })
    }
}
