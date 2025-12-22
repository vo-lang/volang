//! Selection representation for selector expressions.
//!
//! A Selection describes a selector expression x.f.


use crate::objects::{ObjKey, TCObjects, TypeKey};
use crate::typ;
use std::fmt::{self, Write};

/// SelectionKind describes the kind of a selector expression x.f.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectionKind {
    /// x.f is a struct field selector.
    FieldVal,
    /// x.f is a method selector.
    MethodVal,
    /// x.f is a method expression.
    MethodExpr,
}

/// A Selection describes a selector expression x.f.
#[derive(Clone, Debug)]
pub struct Selection {
    kind: SelectionKind,
    recv: Option<TypeKey>,
    obj: ObjKey,
    indices: Vec<usize>,
    indirect: bool,
    typ: Option<TypeKey>,
    id: String,
}

impl Selection {
    pub fn new(
        kind: SelectionKind,
        recv: Option<TypeKey>,
        obj: ObjKey,
        indices: Vec<usize>,
        indirect: bool,
        objs: &TCObjects,
    ) -> Selection {
        let id = objs.lobjs[obj].id(objs).to_string();
        Selection {
            kind,
            recv,
            obj,
            indices,
            indirect,
            typ: None,
            id,
        }
    }

    pub fn init_type(&mut self, objs: &mut TCObjects) {
        self.typ = Some(self.eval_type(objs));
    }

    pub fn kind(&self) -> &SelectionKind {
        &self.kind
    }

    pub fn recv(&self) -> Option<TypeKey> {
        self.recv
    }

    pub fn obj(&self) -> ObjKey {
        self.obj
    }

    pub fn typ(&self) -> TypeKey {
        self.typ.unwrap()
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    /// Returns the path from x to f in x.f.
    pub fn indices(&self) -> &[usize] {
        &self.indices
    }

    /// Reports whether any pointer indirection was required.
    pub fn indirect(&self) -> bool {
        self.indirect
    }

    /// Returns the field index (last element of indices).
    pub fn field_index(&self) -> Option<usize> {
        self.indices.last().copied()
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        f.write_str(match self.kind {
            SelectionKind::FieldVal => "field (",
            SelectionKind::MethodVal => "method (",
            SelectionKind::MethodExpr => "method expr (",
        })?;
        typ::fmt_type(self.recv(), f, objs)?;
        write!(f, ") {}", objs.lobjs[self.obj].name())?;
        match self.kind {
            SelectionKind::FieldVal => {
                f.write_char(' ')?;
                typ::fmt_type(Some(self.typ()), f, objs)?;
            }
            _ => typ::fmt_signature(self.typ(), f, objs)?,
        }
        Ok(())
    }

    fn eval_type(&self, objs: &mut TCObjects) -> TypeKey {
        let obj = &objs.lobjs[self.obj];
        match self.kind {
            SelectionKind::FieldVal => obj.typ().unwrap(),
            SelectionKind::MethodVal => {
                let t = &objs.types[obj.typ().unwrap()];
                let mut sig = *t.try_as_signature().unwrap();
                let recv_key = sig.recv().unwrap();
                let mut new_recv = objs.lobjs[recv_key].clone();
                new_recv.set_type(self.recv);
                sig.set_recv(Some(objs.lobjs.insert(new_recv)));
                objs.types.insert(typ::Type::Signature(sig))
            }
            SelectionKind::MethodExpr => {
                let t = &objs.types[obj.typ().unwrap()];
                let mut sig = *t.try_as_signature().unwrap();
                let recv_key = sig.recv().unwrap();
                let mut arg0 = objs.lobjs[recv_key].clone();
                arg0.set_type(self.recv);
                let arg0key = objs.lobjs.insert(arg0);
                sig.set_recv(None);
                let mut params = vec![arg0key];
                let tup = objs.types[sig.params()].try_as_tuple_mut().unwrap();
                params.append(&mut tup.vars().clone());
                let params_key = objs.types.insert(typ::Type::Tuple(typ::TupleDetail::new(params)));
                sig.set_params(params_key);
                objs.types.insert(typ::Type::Signature(sig))
            }
        }
    }
}
