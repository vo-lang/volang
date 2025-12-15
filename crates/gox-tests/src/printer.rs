//! AST pretty-printer for test output.

use gox_syntax::ast::*;
use gox_common::symbol::SymbolInterner;
use std::fmt::Write;

/// Pretty-prints AST nodes for test comparison.
pub struct AstPrinter<'a> {
    interner: &'a SymbolInterner,
    output: String,
    indent: usize,
}

impl<'a> AstPrinter<'a> {
    pub fn new(interner: &'a SymbolInterner) -> Self {
        Self {
            interner,
            output: String::new(),
            indent: 0,
        }
    }

    pub fn print_file(&mut self, file: &File) -> String {
        self.output.clear();
        self.write_file(file);
        std::mem::take(&mut self.output)
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn write_file(&mut self, file: &File) {
        self.writeln("File {");
        self.indent += 1;

        // Package
        if let Some(pkg) = &file.package {
            self.write_indent();
            let name = self.resolve_symbol(pkg.symbol);
            writeln!(self.output, "package: \"{}\",", name).unwrap();
        } else {
            self.writeln("package: None,");
        }

        // Imports
        if !file.imports.is_empty() {
            self.writeln("imports: [");
            self.indent += 1;
            for import in &file.imports {
                let path = self.resolve_symbol(import.path.raw);
                self.write_indent();
                writeln!(self.output, "{},", path).unwrap();
            }
            self.indent -= 1;
            self.writeln("],");
        }

        // Declarations
        if !file.decls.is_empty() {
            self.writeln("decls: [");
            self.indent += 1;
            for decl in &file.decls {
                self.write_decl(decl);
            }
            self.indent -= 1;
            self.writeln("],");
        }

        self.indent -= 1;
        self.writeln("}");
    }

    fn write_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(v) => self.write_var_decl(v),
            Decl::Const(c) => self.write_const_decl(c),
            Decl::Type(t) => self.write_type_decl(t),
            Decl::Func(f) => self.write_func_decl(f),
            Decl::Interface(i) => self.write_interface_decl(i),
        }
    }

    fn write_var_decl(&mut self, v: &VarDecl) {
        self.writeln("Var {");
        self.indent += 1;
        self.writeln("specs: [");
        self.indent += 1;
        for spec in &v.specs {
            self.write_var_spec(spec);
        }
        self.indent -= 1;
        self.writeln("],");
        self.indent -= 1;
        self.writeln("},");
    }

    fn write_var_spec(&mut self, spec: &VarSpec) {
        self.writeln("VarSpec {");
        self.indent += 1;
        
        // Names
        self.write_indent();
        write!(self.output, "names: [").unwrap();
        for (i, name) in spec.names.iter().enumerate() {
            if i > 0 { write!(self.output, ", ").unwrap(); }
            write!(self.output, "\"{}\"", self.resolve_symbol(name.symbol)).unwrap();
        }
        writeln!(self.output, "],").unwrap();

        // Type
        if let Some(ty) = &spec.ty {
            self.write_indent();
            write!(self.output, "type: ").unwrap();
            self.write_type_inline(ty);
            writeln!(self.output, ",").unwrap();
        }

        // Values
        if !spec.values.is_empty() {
            self.writeln("values: [");
            self.indent += 1;
            for val in &spec.values {
                self.write_expr(val);
            }
            self.indent -= 1;
            self.writeln("],");
        }

        self.indent -= 1;
        self.writeln("},");
    }

    fn write_const_decl(&mut self, c: &ConstDecl) {
        self.writeln("Const {");
        self.indent += 1;
        self.writeln("specs: [");
        self.indent += 1;
        for spec in &c.specs {
            self.write_const_spec(spec);
        }
        self.indent -= 1;
        self.writeln("],");
        self.indent -= 1;
        self.writeln("},");
    }

    fn write_const_spec(&mut self, spec: &ConstSpec) {
        self.writeln("ConstSpec {");
        self.indent += 1;
        
        self.write_indent();
        write!(self.output, "names: [").unwrap();
        for (i, name) in spec.names.iter().enumerate() {
            if i > 0 { write!(self.output, ", ").unwrap(); }
            write!(self.output, "\"{}\"", self.resolve_symbol(name.symbol)).unwrap();
        }
        writeln!(self.output, "],").unwrap();

        if let Some(ty) = &spec.ty {
            self.write_indent();
            write!(self.output, "type: ").unwrap();
            self.write_type_inline(ty);
            writeln!(self.output, ",").unwrap();
        }

        if !spec.values.is_empty() {
            self.writeln("values: [");
            self.indent += 1;
            for val in &spec.values {
                self.write_expr(val);
            }
            self.indent -= 1;
            self.writeln("],");
        }

        self.indent -= 1;
        self.writeln("},");
    }

    fn write_type_decl(&mut self, t: &TypeDecl) {
        self.write_indent();
        let name = self.resolve_symbol(t.name.symbol);
        write!(self.output, "Type {{ name: \"{}\", type: ", name).unwrap();
        self.write_type_inline(&t.ty);
        writeln!(self.output, " }},").unwrap();
    }

    fn write_func_decl(&mut self, f: &FuncDecl) {
        self.writeln("Func {");
        self.indent += 1;

        // Receiver
        if let Some(recv) = &f.receiver {
            self.write_indent();
            let recv_name = self.resolve_symbol(recv.name.symbol);
            let recv_ty = self.resolve_symbol(recv.ty.symbol);
            writeln!(self.output, "receiver: ({} {}),", recv_name, recv_ty).unwrap();
        }

        // Name
        self.write_indent();
        let name = self.resolve_symbol(f.name.symbol);
        writeln!(self.output, "name: \"{}\",", name).unwrap();

        // Signature
        self.write_func_sig(&f.sig);

        // Body
        if let Some(body) = &f.body {
            self.writeln("body: {");
            self.indent += 1;
            for stmt in &body.stmts {
                self.write_stmt(stmt);
            }
            self.indent -= 1;
            self.writeln("},");
        }

        self.indent -= 1;
        self.writeln("},");
    }

    fn write_func_sig(&mut self, sig: &FuncSig) {
        // Params
        if !sig.params.is_empty() {
            self.writeln("params: [");
            self.indent += 1;
            for param in &sig.params {
                self.write_indent();
                write!(self.output, "(").unwrap();
                for (i, name) in param.names.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    write!(self.output, "{}", self.resolve_symbol(name.symbol)).unwrap();
                }
                write!(self.output, " ").unwrap();
                self.write_type_inline(&param.ty);
                writeln!(self.output, "),").unwrap();
            }
            self.indent -= 1;
            self.writeln("],");
        }

        // Results
        if !sig.results.is_empty() {
            self.write_indent();
            write!(self.output, "results: [").unwrap();
            for (i, result) in sig.results.iter().enumerate() {
                if i > 0 { write!(self.output, ", ").unwrap(); }
                if let Some(ref name) = result.name {
                    write!(self.output, "{} ", self.resolve_symbol(name.symbol)).unwrap();
                }
                self.write_type_inline(&result.ty);
            }
            writeln!(self.output, "],").unwrap();
        }

        if sig.variadic {
            self.writeln("variadic: true,");
        }
    }

    fn write_interface_decl(&mut self, i: &InterfaceDecl) {
        self.writeln("Interface {");
        self.indent += 1;
        
        let name = self.resolve_symbol(i.name.symbol);
        self.write_indent();
        writeln!(self.output, "name: \"{}\",", name).unwrap();

        if !i.elems.is_empty() {
            self.writeln("methods: [");
            self.indent += 1;
            for elem in &i.elems {
                match elem {
                    InterfaceElem::Method(m) => {
                        self.write_indent();
                        let mname = self.resolve_symbol(m.name.symbol);
                        writeln!(self.output, "{},", mname).unwrap();
                    }
                    InterfaceElem::Embedded(e) => {
                        self.write_indent();
                        let ename = self.resolve_symbol(e.symbol);
                        writeln!(self.output, "embed: {},", ename).unwrap();
                    }
                }
            }
            self.indent -= 1;
            self.writeln("],");
        }

        self.indent -= 1;
        self.writeln("},");
    }

    fn write_type_inline(&mut self, ty: &TypeExpr) {
        match &ty.kind {
            TypeExprKind::Ident(id) => {
                write!(self.output, "{}", self.resolve_symbol(id.symbol)).unwrap();
            }
            TypeExprKind::Array(arr) => {
                write!(self.output, "[").unwrap();
                self.write_expr_inline(&arr.len);
                write!(self.output, "]").unwrap();
                self.write_type_inline(&arr.elem);
            }
            TypeExprKind::Slice(elem) => {
                write!(self.output, "[]").unwrap();
                self.write_type_inline(elem);
            }
            TypeExprKind::Map(m) => {
                write!(self.output, "map[").unwrap();
                self.write_type_inline(&m.key);
                write!(self.output, "]").unwrap();
                self.write_type_inline(&m.value);
            }
            TypeExprKind::Chan(c) => {
                match c.dir {
                    ChanDir::Both => write!(self.output, "chan ").unwrap(),
                    ChanDir::Send => write!(self.output, "chan<- ").unwrap(),
                    ChanDir::Recv => write!(self.output, "<-chan ").unwrap(),
                }
                self.write_type_inline(&c.elem);
            }
            TypeExprKind::Func(f) => {
                write!(self.output, "func(").unwrap();
                for (i, p) in f.params.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_type_inline(p);
                }
                write!(self.output, ")").unwrap();
                if !f.results.is_empty() {
                    write!(self.output, " ").unwrap();
                    if f.results.len() > 1 {
                        write!(self.output, "(").unwrap();
                    }
                    for (i, r) in f.results.iter().enumerate() {
                        if i > 0 { write!(self.output, ", ").unwrap(); }
                        self.write_type_inline(r);
                    }
                    if f.results.len() > 1 {
                        write!(self.output, ")").unwrap();
                    }
                }
            }
            TypeExprKind::Struct(s) => {
                write!(self.output, "struct {{ ").unwrap();
                for field in &s.fields {
                    for (i, name) in field.names.iter().enumerate() {
                        if i > 0 { write!(self.output, ", ").unwrap(); }
                        write!(self.output, "{}", self.resolve_symbol(name.symbol)).unwrap();
                    }
                    if !field.names.is_empty() {
                        write!(self.output, " ").unwrap();
                    }
                    self.write_type_inline(&field.ty);
                    write!(self.output, "; ").unwrap();
                }
                write!(self.output, "}}").unwrap();
            }
            TypeExprKind::Obx(s) => {
                write!(self.output, "object {{ ").unwrap();
                for field in &s.fields {
                    for (i, name) in field.names.iter().enumerate() {
                        if i > 0 { write!(self.output, ", ").unwrap(); }
                        write!(self.output, "{}", self.resolve_symbol(name.symbol)).unwrap();
                    }
                    if !field.names.is_empty() {
                        write!(self.output, " ").unwrap();
                    }
                    self.write_type_inline(&field.ty);
                    write!(self.output, "; ").unwrap();
                }
                write!(self.output, "}}").unwrap();
            }
            TypeExprKind::Interface(i) => {
                write!(self.output, "interface {{ ").unwrap();
                for elem in &i.elems {
                    match elem {
                        InterfaceElem::Method(m) => {
                            write!(self.output, "{}(); ", self.resolve_symbol(m.name.symbol)).unwrap();
                        }
                        InterfaceElem::Embedded(e) => {
                            write!(self.output, "{}; ", self.resolve_symbol(e.symbol)).unwrap();
                        }
                    }
                }
                write!(self.output, "}}").unwrap();
            }
        }
    }

    fn write_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Empty => self.writeln("Empty,"),
            StmtKind::Block(b) => {
                self.writeln("Block {");
                self.indent += 1;
                for s in &b.stmts {
                    self.write_stmt(s);
                }
                self.indent -= 1;
                self.writeln("},");
            }
            StmtKind::Var(v) => self.write_var_decl(v),
            StmtKind::Const(c) => self.write_const_decl(c),
            StmtKind::ShortVar(sv) => {
                self.write_indent();
                write!(self.output, "ShortVar {{ names: [").unwrap();
                for (i, name) in sv.names.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    write!(self.output, "\"{}\"", self.resolve_symbol(name.symbol)).unwrap();
                }
                write!(self.output, "], values: [").unwrap();
                for (i, val) in sv.values.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_expr_inline(val);
                }
                writeln!(self.output, "] }},").unwrap();
            }
            StmtKind::Expr(e) => {
                self.write_indent();
                write!(self.output, "Expr(").unwrap();
                self.write_expr_inline(e);
                writeln!(self.output, "),").unwrap();
            }
            StmtKind::Assign(a) => {
                self.write_indent();
                write!(self.output, "Assign {{ lhs: [").unwrap();
                for (i, l) in a.lhs.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_expr_inline(l);
                }
                write!(self.output, "], op: {:?}, rhs: [", a.op).unwrap();
                for (i, r) in a.rhs.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_expr_inline(r);
                }
                writeln!(self.output, "] }},").unwrap();
            }
            StmtKind::IncDec(id) => {
                self.write_indent();
                write!(self.output, "IncDec {{ expr: ").unwrap();
                self.write_expr_inline(&id.expr);
                writeln!(self.output, ", inc: {} }},", id.is_inc).unwrap();
            }
            StmtKind::Return(r) => {
                self.write_indent();
                write!(self.output, "Return [").unwrap();
                for (i, v) in r.values.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_expr_inline(v);
                }
                writeln!(self.output, "],").unwrap();
            }
            StmtKind::If(i) => {
                self.writeln("If {");
                self.indent += 1;
                if let Some(init) = &i.init {
                    self.writeln("init:");
                    self.indent += 1;
                    self.write_stmt(init);
                    self.indent -= 1;
                }
                self.write_indent();
                write!(self.output, "cond: ").unwrap();
                self.write_expr_inline(&i.cond);
                writeln!(self.output, ",").unwrap();
                self.writeln("then: {");
                self.indent += 1;
                for s in &i.then.stmts {
                    self.write_stmt(s);
                }
                self.indent -= 1;
                self.writeln("},");
                if let Some(else_) = &i.else_ {
                    self.writeln("else:");
                    self.indent += 1;
                    self.write_stmt(else_);
                    self.indent -= 1;
                }
                self.indent -= 1;
                self.writeln("},");
            }
            StmtKind::For(f) => {
                self.writeln("For {");
                self.indent += 1;
                match &f.clause {
                    ForClause::Cond(c) => {
                        if let Some(cond) = c {
                            self.write_indent();
                            write!(self.output, "cond: ").unwrap();
                            self.write_expr_inline(cond);
                            writeln!(self.output, ",").unwrap();
                        }
                    }
                    ForClause::Three { init, cond, post } => {
                        if let Some(init) = init {
                            self.writeln("init:");
                            self.indent += 1;
                            self.write_stmt(init);
                            self.indent -= 1;
                        }
                        if let Some(cond) = cond {
                            self.write_indent();
                            write!(self.output, "cond: ").unwrap();
                            self.write_expr_inline(cond);
                            writeln!(self.output, ",").unwrap();
                        }
                        if let Some(post) = post {
                            self.writeln("post:");
                            self.indent += 1;
                            self.write_stmt(post);
                            self.indent -= 1;
                        }
                    }
                    ForClause::Range { key, value, define, expr } => {
                        self.write_indent();
                        write!(self.output, "range: ").unwrap();
                        if let Some(k) = key {
                            write!(self.output, "{}", self.resolve_symbol(k.symbol)).unwrap();
                        }
                        if let Some(v) = value {
                            write!(self.output, ", {}", self.resolve_symbol(v.symbol)).unwrap();
                        }
                        write!(self.output, " {} ", if *define { ":=" } else { "=" }).unwrap();
                        self.write_expr_inline(expr);
                        writeln!(self.output, ",").unwrap();
                    }
                }
                self.writeln("body: {");
                self.indent += 1;
                for s in &f.body.stmts {
                    self.write_stmt(s);
                }
                self.indent -= 1;
                self.writeln("},");
                self.indent -= 1;
                self.writeln("},");
            }
            StmtKind::Switch(s) => {
                self.writeln("Switch {");
                self.indent += 1;
                if let Some(tag) = &s.tag {
                    self.write_indent();
                    write!(self.output, "tag: ").unwrap();
                    self.write_expr_inline(tag);
                    writeln!(self.output, ",").unwrap();
                }
                self.writeln("cases: [");
                self.indent += 1;
                for case in &s.cases {
                    self.write_indent();
                    write!(self.output, "case [").unwrap();
                    for (i, e) in case.exprs.iter().enumerate() {
                        if i > 0 { write!(self.output, ", ").unwrap(); }
                        self.write_expr_inline(e);
                    }
                    writeln!(self.output, "]: {{").unwrap();
                    self.indent += 1;
                    for stmt in &case.body {
                        self.write_stmt(stmt);
                    }
                    self.indent -= 1;
                    self.writeln("},");
                }
                self.indent -= 1;
                self.writeln("],");
                self.indent -= 1;
                self.writeln("},");
            }
            StmtKind::TypeSwitch(_) => self.writeln("TypeSwitch { ... },"),
            StmtKind::Select(_) => self.writeln("Select { ... },"),
            StmtKind::Go(g) => {
                self.write_indent();
                write!(self.output, "Go(").unwrap();
                self.write_expr_inline(&g.call);
                writeln!(self.output, "),").unwrap();
            }
            StmtKind::Defer(d) => {
                self.write_indent();
                write!(self.output, "Defer(").unwrap();
                self.write_expr_inline(&d.call);
                writeln!(self.output, "),").unwrap();
            }
            StmtKind::Send(s) => {
                self.write_indent();
                write!(self.output, "Send {{ chan: ").unwrap();
                self.write_expr_inline(&s.chan);
                write!(self.output, ", value: ").unwrap();
                self.write_expr_inline(&s.value);
                writeln!(self.output, " }},").unwrap();
            }
            StmtKind::Break(b) => {
                self.write_indent();
                if let Some(label) = &b.label {
                    writeln!(self.output, "Break({}),", self.resolve_symbol(label.symbol)).unwrap();
                } else {
                    writeln!(self.output, "Break,").unwrap();
                }
            }
            StmtKind::Continue(c) => {
                self.write_indent();
                if let Some(label) = &c.label {
                    writeln!(self.output, "Continue({}),", self.resolve_symbol(label.symbol)).unwrap();
                } else {
                    writeln!(self.output, "Continue,").unwrap();
                }
            }
            StmtKind::Goto(g) => {
                self.write_indent();
                writeln!(self.output, "Goto({}),", self.resolve_symbol(g.label.symbol)).unwrap();
            }
            StmtKind::Fallthrough => self.writeln("Fallthrough,"),
            StmtKind::Labeled(l) => {
                self.write_indent();
                writeln!(self.output, "Label({}):", self.resolve_symbol(l.label.symbol)).unwrap();
                self.indent += 1;
                self.write_stmt(&l.stmt);
                self.indent -= 1;
            }
            StmtKind::Type(t) => {
                self.write_indent();
                writeln!(self.output, "Type {{ name: \"{}\", ty: ... }},", self.resolve_symbol(t.name.symbol)).unwrap();
            }
        }
    }

    fn write_expr(&mut self, expr: &Expr) {
        self.write_indent();
        self.write_expr_inline(expr);
        writeln!(self.output, ",").unwrap();
    }

    fn write_expr_inline(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(id) => {
                write!(self.output, "{}", self.resolve_symbol(id.symbol)).unwrap();
            }
            ExprKind::IntLit(lit) => {
                write!(self.output, "{}", self.resolve_symbol(lit.raw)).unwrap();
            }
            ExprKind::FloatLit(lit) => {
                write!(self.output, "{}", self.resolve_symbol(lit.raw)).unwrap();
            }
            ExprKind::RuneLit(lit) => {
                write!(self.output, "{}", self.resolve_symbol(lit.raw)).unwrap();
            }
            ExprKind::StringLit(lit) => {
                write!(self.output, "{}", self.resolve_symbol(lit.raw)).unwrap();
            }
            ExprKind::Binary(b) => {
                write!(self.output, "(").unwrap();
                self.write_expr_inline(&b.left);
                write!(self.output, " {:?} ", b.op).unwrap();
                self.write_expr_inline(&b.right);
                write!(self.output, ")").unwrap();
            }
            ExprKind::Unary(u) => {
                write!(self.output, "{:?}(", u.op).unwrap();
                self.write_expr_inline(&u.operand);
                write!(self.output, ")").unwrap();
            }
            ExprKind::Call(c) => {
                self.write_expr_inline(&c.func);
                write!(self.output, "(").unwrap();
                for (i, arg) in c.args.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    self.write_expr_inline(arg);
                }
                if c.spread {
                    write!(self.output, "...").unwrap();
                }
                write!(self.output, ")").unwrap();
            }
            ExprKind::Index(idx) => {
                self.write_expr_inline(&idx.expr);
                write!(self.output, "[").unwrap();
                self.write_expr_inline(&idx.index);
                write!(self.output, "]").unwrap();
            }
            ExprKind::Slice(s) => {
                self.write_expr_inline(&s.expr);
                write!(self.output, "[").unwrap();
                if let Some(low) = &s.low {
                    self.write_expr_inline(low);
                }
                write!(self.output, ":").unwrap();
                if let Some(high) = &s.high {
                    self.write_expr_inline(high);
                }
                write!(self.output, "]").unwrap();
            }
            ExprKind::Selector(s) => {
                self.write_expr_inline(&s.expr);
                write!(self.output, ".{}", self.resolve_symbol(s.sel.symbol)).unwrap();
            }
            ExprKind::TypeAssert(ta) => {
                self.write_expr_inline(&ta.expr);
                write!(self.output, ".(").unwrap();
                if let Some(ty) = &ta.ty {
                    self.write_type_inline(ty);
                } else {
                    write!(self.output, "type").unwrap();
                }
                write!(self.output, ")").unwrap();
            }
            ExprKind::CompositeLit(cl) => {
                self.write_type_inline(&cl.ty);
                write!(self.output, "{{").unwrap();
                for (i, elem) in cl.elems.iter().enumerate() {
                    if i > 0 { write!(self.output, ", ").unwrap(); }
                    if let Some(key) = &elem.key {
                        match key {
                            CompositeLitKey::Ident(id) => {
                                write!(self.output, "{}: ", self.resolve_symbol(id.symbol)).unwrap();
                            }
                            CompositeLitKey::Expr(e) => {
                                self.write_expr_inline(e);
                                write!(self.output, ": ").unwrap();
                            }
                        }
                    }
                    self.write_expr_inline(&elem.value);
                }
                write!(self.output, "}}").unwrap();
            }
            ExprKind::FuncLit(_) => {
                write!(self.output, "func(...) {{ ... }}").unwrap();
            }
            ExprKind::Conversion(c) => {
                self.write_type_inline(&c.ty);
                write!(self.output, "(").unwrap();
                self.write_expr_inline(&c.expr);
                write!(self.output, ")").unwrap();
            }
            ExprKind::Receive(e) => {
                write!(self.output, "<-").unwrap();
                self.write_expr_inline(e);
            }
            ExprKind::Paren(e) => {
                write!(self.output, "(").unwrap();
                self.write_expr_inline(e);
                write!(self.output, ")").unwrap();
            }
            ExprKind::TypeAsExpr(t) => {
                self.write_type_inline(t);
            }
        }
    }

    fn resolve_symbol(&self, symbol: gox_common::symbol::Symbol) -> String {
        self.interner.resolve(symbol).unwrap_or("<unknown>").to_string()
    }
}
