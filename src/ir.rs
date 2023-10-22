pub type IntValue = i64;
pub type StrValue = Vec<u8>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct JumpId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SwitchId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VarId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct StrId(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CallId(pub usize);

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

    Case(SwitchId, CaseEnum),
    Label(JumpId),
}

#[derive(Debug)]
pub struct Script {
    pub ins_seq: Vec<Ins>,
    pub str_tab: Vec<StrValue>,
}

impl Script {
    pub fn new(ins_seq: Vec<Ins>, str_tab: Vec<StrValue>) -> Self {
        Self { ins_seq, str_tab }
    }
}
