pub type IntValue = i64;
pub type StrValue = Vec<u8>;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct JumpId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SwitchId(pub usize);

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VarId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct StrId(pub usize);

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CallId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Undefined,
    Integer,
    String,
    UserType(usize),
}

#[derive(Debug, Clone)]
pub struct CallableShape {
    parameter_types: Vec<ValueType>,
    has_return_value: bool,
}

impl CallableShape {
    pub fn new_func(parameter_types: Vec<ValueType>) -> Self {
        Self {
            parameter_types,
            has_return_value: true,
        }
    }

    pub fn new_proc(parameter_types: Vec<ValueType>) -> Self {
        Self {
            parameter_types,
            has_return_value: false,
        }
    }

    pub fn is_func(&self) -> bool {
        self.has_return_value
    }

    pub fn num_parameters(&self) -> usize {
        self.parameter_types.len()
    }

    pub fn parameter_types(&self) -> &[ValueType] {
        &self.parameter_types
    }
}

/** Identifies the object of a "case" statement with a "switch". */
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum CaseEnum {
    Val(IntValue),
    Default,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Ins {
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogicalAnd,
    LogicalOr,
    Inc,
    Dec,
    Neg,
    LogicalNot,
    Cmp,
    PushVar(VarId),
    PopVar(VarId),
    Dupe,
    Discard,
    PushInt(IntValue),
    Jmp(JumpId),
    Blt(JumpId),
    Ble(JumpId),
    Beq(JumpId),
    Bne(JumpId),
    Bge(JumpId),
    Bgt(JumpId),
    Call(CallId),
    Switch(SwitchId),
    Exit,

    Case(SwitchId, CaseEnum),
    Label(JumpId),
}

impl Ins {
    pub fn branch_target(&self) -> Option<JumpId> {
        match self {
            Ins::Blt(jump_id)
            | Ins::Ble(jump_id)
            | Ins::Beq(jump_id)
            | Ins::Bne(jump_id)
            | Ins::Bge(jump_id)
            | Ins::Bgt(jump_id) => Some(*jump_id),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Script {
    pub instructions: Vec<Ins>,
    pub strings: Vec<StrValue>,
}

impl Script {
    pub fn new(instructions: Vec<Ins>, strings: Vec<StrValue>) -> Self {
        Self {
            instructions,
            strings,
        }
    }
}
