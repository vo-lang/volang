//! Detra Parser - parses tokens into AST.

use crate::ast::*;
use crate::lexer::{Token, SpannedToken};

pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut types = Vec::new();
        let mut state = None;
        let mut actions = Vec::new();
        let mut rules = Vec::new();
        let mut views = Vec::new();
        let mut commands = Vec::new();

        while !self.is_eof() {
            match self.peek() {
                Token::Type => types.push(self.parse_type_decl()?),
                Token::State => {
                    if state.is_some() {
                        return Err("Multiple state declarations".to_string());
                    }
                    state = Some(self.parse_state_decl()?);
                }
                Token::Action => actions.push(self.parse_action_decl()?),
                Token::Rule => rules.push(self.parse_rule_decl()?),
                Token::View => views.push(self.parse_view_decl()?),
                Token::Command => commands.push(self.parse_command_decl()?),
                _ => return Err(format!("Unexpected token: {:?}", self.peek())),
            }
        }

        let state = state.ok_or("Missing state declaration")?;

        if !views.iter().any(|v| v.name == "Main") {
            return Err("Missing Main view".to_string());
        }

        Ok(Program {
            types,
            state,
            actions,
            rules,
            views,
            commands,
        })
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos].token
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos].token;
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        tok
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.peek() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.peek()))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.peek().clone() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(format!("Expected identifier, got {:?}", self.peek())),
        }
    }

    fn parse_type_decl(&mut self) -> Result<TypeDecl, String> {
        self.expect(Token::Type)?;
        let name = self.expect_ident()?;
        self.expect(Token::Struct)?;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while self.peek() != &Token::RBrace {
            let field_name = self.expect_ident()?;
            let ty = self.parse_type()?;
            fields.push(FieldDecl { name: field_name, ty });
        }
        self.expect(Token::RBrace)?;

        Ok(TypeDecl { name, fields })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.peek().clone() {
            Token::Bool => { self.advance(); Ok(Type::Bool) }
            Token::Int => { self.advance(); Ok(Type::Int) }
            Token::Float => { self.advance(); Ok(Type::Float) }
            Token::String => { self.advance(); Ok(Type::String) }
            Token::LBracket => {
                self.advance();
                self.expect(Token::RBracket)?;
                let elem = self.parse_type()?;
                Ok(Type::Array(Box::new(elem)))
            }
            Token::Map => {
                self.advance();
                self.expect(Token::LBracket)?;
                let key = self.parse_type()?;
                self.expect(Token::RBracket)?;
                let val = self.parse_type()?;
                Ok(Type::Map(Box::new(key), Box::new(val)))
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Type::Named(name))
            }
            _ => Err(format!("Expected type, got {:?}", self.peek())),
        }
    }

    fn parse_state_decl(&mut self) -> Result<StateDecl, String> {
        self.expect(Token::State)?;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while self.peek() != &Token::RBrace {
            let modifier = match self.peek() {
                Token::Const => { self.advance(); FieldModifier::Const }
                Token::External => { self.advance(); FieldModifier::External }
                _ => FieldModifier::None,
            };
            let field_name = self.expect_ident()?;
            let ty = self.parse_type()?;
            let default = if self.peek() == &Token::Assign {
                self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            fields.push(StateField {
                modifier,
                name: field_name,
                ty,
                default,
            });
        }
        self.expect(Token::RBrace)?;

        Ok(StateDecl { name, fields })
    }

    fn parse_action_decl(&mut self) -> Result<ActionDecl, String> {
        self.expect(Token::Action)?;
        let name = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;

        let mut body = Vec::new();
        while self.peek() != &Token::RBrace {
            body.push(self.parse_action_stmt()?);
        }
        self.expect(Token::RBrace)?;

        Ok(ActionDecl { name, params, body })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        if self.peek() == &Token::RParen {
            return Ok(params);
        }

        loop {
            let name = self.expect_ident()?;
            let ty = self.parse_type()?;
            let default = if self.peek() == &Token::Assign {
                self.advance();
                Some(self.parse_literal()?)
            } else {
                None
            };
            params.push(Param { name, ty, default });

            if self.peek() != &Token::Comma {
                break;
            }
            self.advance();
        }
        Ok(params)
    }

    fn parse_action_stmt(&mut self) -> Result<ActionStmt, String> {
        match self.peek().clone() {
            Token::Require => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(ActionStmt::Require(expr))
            }
            Token::Set => {
                self.advance();
                let path = self.parse_path()?;
                self.expect(Token::Assign)?;
                let value = self.parse_expr()?;
                Ok(ActionStmt::Set { path, value })
            }
            Token::Emit => {
                self.advance();
                let command = self.expect_ident()?;
                self.expect(Token::LParen)?;
                let args = self.parse_args()?;
                self.expect(Token::RParen)?;
                Ok(ActionStmt::Emit { command, args })
            }
            _ => Err(format!("Expected action statement, got {:?}", self.peek())),
        }
    }

    fn parse_rule_decl(&mut self) -> Result<RuleDecl, String> {
        self.expect(Token::Rule)?;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut body = Vec::new();
        while self.peek() != &Token::RBrace {
            body.push(self.parse_rule_stmt()?);
        }
        self.expect(Token::RBrace)?;

        Ok(RuleDecl { name, body })
    }

    fn parse_rule_stmt(&mut self) -> Result<RuleStmt, String> {
        match self.peek().clone() {
            Token::Derive => {
                self.advance();
                let path = self.parse_path()?;
                self.expect(Token::Assign)?;
                let value = self.parse_expr()?;
                Ok(RuleStmt::Derive { path, value })
            }
            Token::Check => {
                self.advance();
                let expr = self.parse_expr()?;
                let message = if self.peek() == &Token::Colon {
                    self.advance();
                    match self.peek().clone() {
                        Token::StringLit(s) => {
                            self.advance();
                            Some(s)
                        }
                        _ => return Err("Expected string after ':'".to_string()),
                    }
                } else {
                    None
                };
                Ok(RuleStmt::Check { expr, message })
            }
            _ => Err(format!("Expected rule statement, got {:?}", self.peek())),
        }
    }

    fn parse_view_decl(&mut self) -> Result<ViewDecl, String> {
        self.expect(Token::View)?;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        let body = self.parse_node()?;
        self.expect(Token::RBrace)?;

        Ok(ViewDecl { name, body })
    }

    fn parse_command_decl(&mut self) -> Result<CommandDecl, String> {
        self.expect(Token::Command)?;
        let name = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        Ok(CommandDecl { name, params })
    }

    fn parse_node(&mut self) -> Result<Node, String> {
        let kind = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let props = self.parse_props()?;
        self.expect(Token::RParen)?;

        let children = if self.peek() == &Token::LBrace {
            self.advance();
            let mut children = Vec::new();
            while self.peek() != &Token::RBrace {
                children.push(self.parse_view_child()?);
            }
            self.advance();
            children
        } else {
            Vec::new()
        };

        Ok(Node { kind, props, children })
    }

    fn parse_props(&mut self) -> Result<Vec<Prop>, String> {
        let mut props = Vec::new();
        if self.peek() == &Token::RParen {
            return Ok(props);
        }

        loop {
            let name = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let value = self.parse_prop_value()?;
            props.push(Prop { name, value });

            if self.peek() != &Token::Comma {
                break;
            }
            self.advance();
        }
        Ok(props)
    }

    fn parse_prop_value(&mut self) -> Result<PropValue, String> {
        if let Token::Ident(name) = self.peek().clone() {
            let next_pos = self.pos + 1;
            if next_pos < self.tokens.len() {
                let next = &self.tokens[next_pos].token;
                if matches!(next, Token::LParen | Token::Comma | Token::RParen) 
                    && name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) 
                {
                    return Ok(PropValue::ActionRef(self.parse_action_ref()?));
                }
            }
        }
        Ok(PropValue::Expr(self.parse_expr()?))
    }

    fn parse_action_ref(&mut self) -> Result<ActionRef, String> {
        let name = self.expect_ident()?;
        let args = if self.peek() == &Token::LParen {
            self.advance();
            let args = self.parse_args()?;
            self.expect(Token::RParen)?;
            args
        } else {
            Vec::new()
        };
        Ok(ActionRef { name, args })
    }

    fn parse_args(&mut self) -> Result<Vec<Arg>, String> {
        let mut args = Vec::new();
        if matches!(self.peek(), Token::RParen) {
            return Ok(args);
        }

        loop {
            let name = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let value = self.parse_expr()?;
            args.push(Arg { name, value });

            if self.peek() != &Token::Comma {
                break;
            }
            self.advance();
        }
        Ok(args)
    }

    fn parse_view_child(&mut self) -> Result<ViewChild, String> {
        match self.peek() {
            Token::If => Ok(ViewChild::If(self.parse_view_if()?)),
            Token::For => Ok(ViewChild::For(self.parse_comprehension()?)),
            _ => Ok(ViewChild::Node(self.parse_node()?)),
        }
    }

    fn parse_view_if(&mut self) -> Result<ViewIf, String> {
        self.expect(Token::If)?;
        let cond = self.parse_expr()?;
        self.expect(Token::LBrace)?;

        let mut then_body = Vec::new();
        while self.peek() != &Token::RBrace {
            then_body.push(self.parse_view_child()?);
        }
        self.expect(Token::RBrace)?;

        let else_body = if self.peek() == &Token::Else {
            self.advance();
            self.expect(Token::LBrace)?;
            let mut body = Vec::new();
            while self.peek() != &Token::RBrace {
                body.push(self.parse_view_child()?);
            }
            self.expect(Token::RBrace)?;
            Some(body)
        } else {
            None
        };

        Ok(ViewIf { cond, then_body, else_body })
    }

    fn parse_comprehension(&mut self) -> Result<Comprehension, String> {
        self.expect(Token::For)?;
        let binding = self.parse_binding()?;
        self.expect(Token::In)?;
        let source = self.parse_expr()?;

        let mut filters = Vec::new();
        let mut sorts = Vec::new();

        loop {
            match self.peek() {
                Token::If => {
                    self.advance();
                    filters.push(self.parse_expr()?);
                }
                Token::Sort => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    let desc = match self.peek() {
                        Token::Desc => { self.advance(); true }
                        Token::Asc => { self.advance(); false }
                        _ => false,
                    };
                    sorts.push(SortClause { expr, desc });
                }
                _ => break,
            }
        }

        self.expect(Token::LBrace)?;
        let body = self.parse_node()?;
        self.expect(Token::RBrace)?;

        Ok(Comprehension { binding, source, filters, sorts, body })
    }

    fn parse_binding(&mut self) -> Result<Binding, String> {
        let first = self.expect_ident()?;
        if self.peek() == &Token::Comma {
            self.advance();
            let second = self.expect_ident()?;
            Ok(Binding::Pair(first, second))
        } else {
            Ok(Binding::Single(first))
        }
    }

    fn parse_path(&mut self) -> Result<Path, String> {
        let mut segments = Vec::new();
        let first = match self.peek() {
            Token::Ident(name) => { let n = name.clone(); self.advance(); n }
            Token::State => { self.advance(); "state".to_string() }
            _ => return Err(format!("Expected identifier or 'state', got {:?}", self.peek())),
        };
        segments.push(first);
        while self.peek() == &Token::Dot {
            self.advance();
            segments.push(self.expect_ident()?);
        }
        Ok(Path { segments })
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and_expr()?;
        while self.peek() == &Token::Or {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::Binary {
                op: BinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_eq_expr()?;
        while self.peek() == &Token::And {
            self.advance();
            let right = self.parse_eq_expr()?;
            left = Expr::Binary {
                op: BinOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_eq_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_cmp_expr()?;
        loop {
            let op = match self.peek() {
                Token::Eq => BinOp::Eq,
                Token::Ne => BinOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_cmp_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_cmp_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_add_expr()?;
        loop {
            let op = match self.peek() {
                Token::Lt => BinOp::Lt,
                Token::Le => BinOp::Le,
                Token::Gt => BinOp::Gt,
                Token::Ge => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_add_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_add_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_mul_expr()?;
        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_mul_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_mul_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary_expr()?;
        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Token::Not => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary { op: UnaryOp::Not, expr: Box::new(expr) })
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(expr) })
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            match self.peek() {
                Token::Dot => {
                    self.advance();
                    let field = self.expect_ident()?;
                    expr = Expr::Field { expr: Box::new(expr), field };
                }
                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(Token::RBracket)?;
                    expr = Expr::Index { expr: Box::new(expr), index: Box::new(index) };
                }
                Token::LParen => {
                    if let Expr::Path(path) = &expr {
                        if path.segments.len() == 1 {
                            let func = path.segments[0].clone();
                            self.advance();
                            let mut args = Vec::new();
                            if self.peek() != &Token::RParen {
                                loop {
                                    args.push(self.parse_expr()?);
                                    if self.peek() != &Token::Comma {
                                        break;
                                    }
                                    self.advance();
                                }
                            }
                            self.expect(Token::RParen)?;
                            expr = Expr::Call { func, args };
                            continue;
                        }
                    }
                    break;
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, String> {
        match self.peek().clone() {
            Token::True => { self.advance(); Ok(Expr::Literal(Literal::Bool(true))) }
            Token::False => { self.advance(); Ok(Expr::Literal(Literal::Bool(false))) }
            Token::IntLit(n) => { self.advance(); Ok(Expr::Literal(Literal::Int(n))) }
            Token::FloatLit(f) => { self.advance(); Ok(Expr::Literal(Literal::Float(f))) }
            Token::StringLit(s) => { self.advance(); Ok(Expr::Literal(Literal::String(s))) }
            Token::EventVar(name) => { self.advance(); Ok(Expr::EventVar(name)) }
            Token::Ident(_) | Token::State => {
                let path = self.parse_path()?;
                Ok(Expr::Path(path))
            }
            Token::String | Token::Int | Token::Float | Token::Bool => {
                let func_name = match self.peek() {
                    Token::String => "string",
                    Token::Int => "int",
                    Token::Float => "float",
                    Token::Bool => "bool",
                    _ => unreachable!(),
                };
                self.advance();
                if self.peek() == &Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    if self.peek() != &Token::RParen {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.peek() != &Token::Comma {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(Token::RParen)?;
                    Ok(Expr::Call { func: func_name.to_string(), args })
                } else {
                    Err(format!("Expected '(' after type conversion function"))
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::LBracket => {
                self.advance();
                let mut elems = Vec::new();
                if self.peek() != &Token::RBracket {
                    loop {
                        elems.push(self.parse_expr()?);
                        if self.peek() != &Token::Comma {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(Token::RBracket)?;
                Ok(Expr::ArrayLit(elems))
            }
            _ => Err(format!("Expected expression, got {:?}", self.peek())),
        }
    }

    fn parse_literal(&mut self) -> Result<Literal, String> {
        match self.peek().clone() {
            Token::True => { self.advance(); Ok(Literal::Bool(true)) }
            Token::False => { self.advance(); Ok(Literal::Bool(false)) }
            Token::IntLit(n) => { self.advance(); Ok(Literal::Int(n)) }
            Token::FloatLit(f) => { self.advance(); Ok(Literal::Float(f)) }
            Token::StringLit(s) => { self.advance(); Ok(Literal::String(s)) }
            _ => Err(format!("Expected literal, got {:?}", self.peek())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(src: &str) -> Result<Program, String> {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_minimal_program() {
        let src = r#"
            state App {
                count int = 0
            }
            view Main {
                Text(text: "hello")
            }
        "#;
        let prog = parse(src).unwrap();
        assert_eq!(prog.state.name, "App");
        assert_eq!(prog.views.len(), 1);
    }
}
