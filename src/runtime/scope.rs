use std::{collections::HashMap, rc::Rc};

use crate::runtime::object::ObjectRef;

use super::object::{StructProps, TraitDef, TraitImpl, Type};

#[derive(Debug, Default)]
pub struct Scope {
    pub eval: Option<ObjectRef>,
    pub parent: Option<*mut Scope>,
    pub builtins: HashMap<String, ObjectRef>,
    pub store: HashMap<String, ObjectRef>,
    pub trait_defs: HashMap<String, TraitDef>,
    pub trait_impls: HashMap<Type, TraitImpl>,
    pub struct_defs: HashMap<String, StructProps>,
}

impl Scope {
    pub fn get_ref(&mut self) -> *mut Self {
        self as *mut Self
    }

    fn get_parent(&mut self) -> Option<&mut Scope> {
        if let Some(parent) = self.parent {
            return Some(unsafe { &mut *parent });
        }
        None
    }

    pub fn get(&mut self, key: &str) -> Option<ObjectRef> {
        if let Some(v) = self.store.get(key) {
            return Some(Rc::clone(v));
        } else if let Some(v) = self.builtins.get(key) {
            return Some(Rc::clone(v));
        } else if let Some(parent) = self.get_parent() {
            return parent.get(key);
        }
        None
    }

    pub fn add_trait_def(&mut self, name: &str, def: TraitDef) {
        self.trait_defs.insert(name.to_string(), def);
    }

    pub fn add_struct(&mut self, name: &str, props: StructProps) {
        self.struct_defs.insert(name.to_string(), props);
    }

    pub fn get_struct_def(&mut self, name: &str) -> Option<StructProps> {
        if let Some(s) = self.struct_defs.get(name) {
            return Some(s.clone());
        } else if let Some(parent) = self.get_parent() {
            return parent.get_struct_def(name);
        }
        None
    }

    pub fn add_trait(&mut self, _type: Type, new_trait: TraitImpl) {
        let existing = self.trait_impls.entry(_type).or_insert(HashMap::new());
        existing.extend(new_trait);
    }

    pub fn get_trait_impl(&mut self, _type: Type) -> Option<TraitImpl> {
        if let Some(v) = self.trait_impls.get(&_type) {
            return Some(v.clone());
        } else if let Some(parent) = self.get_parent() {
            return parent.get_trait_impl(_type);
        }
        None
    }

    pub fn set(&mut self, key: &str, value: ObjectRef) {
        self.store.insert(key.to_string(), value);
    }

    pub fn add_builtin(&mut self, key: &str, value: ObjectRef) {
        self.builtins.insert(key.to_string(), value);
    }

    pub fn locals(&self) -> Option<HashMap<String, ObjectRef>> {
        Some(self.store.clone())
    }

    pub fn root() -> Self {
        Self {
            ..Default::default()
        }
    }
}
