use std::collections::HashMap;

use crate::{
    ast::{ConstVal, NameAccess, NameRef},
    ir::{CallId, CallableShape, ValueType},
};

pub struct ConstScope {
    constants: HashMap<String, ConstVal>,
    callables: HashMap<String, (CallId, CallableShape)>,
    user_types: HashMap<String, usize>,
}

impl ConstScope {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            callables: HashMap::new(),
            user_types: HashMap::new(),
        }
    }

    pub fn add_const(&mut self, name: String, val: ConstVal) {
        self.constants.insert(name, val);
    }

    pub fn add_func(&mut self, name: String, id: CallId, parameter_types: Vec<ValueType>) {
        let to_insert = (id, CallableShape::new_func(parameter_types));
        self.callables.insert(name, to_insert);
    }

    pub fn add_proc(&mut self, name: String, id: CallId, parameter_types: Vec<ValueType>) {
        let to_insert = (id, CallableShape::new_proc(parameter_types));
        self.callables.insert(name, to_insert);
    }

    pub fn add_or_get_user_type(&mut self, name: String) -> usize {
        let would_be_next = self.user_types.len();
        *self.user_types.entry(name).or_insert(would_be_next)
    }

    pub(crate) fn callable_map(&self) -> &HashMap<String, (CallId, CallableShape)> {
        &self.callables
    }
}

impl NameAccess for ConstScope {
    fn lookup_name(&self, name: &str) -> Option<NameRef> {
        if let Some(val) = self.constants.get(name) {
            Some(NameRef::Const(val))
        } else {
            match self.callables.get(name) {
                Some((id, shape)) => {
                    if shape.is_func() {
                        Some(NameRef::Func(*id))
                    } else {
                        Some(NameRef::Proc(*id))
                    }
                }

                None => None,
            }
        }
    }
}
