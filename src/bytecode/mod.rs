pub mod opcodes {
    /// "NOP" (No Operation).
    /// The "NOP" opcode is also used for alignment padding.
    pub const OPCODE_NOP: u8 = 0x00;

    /// "EQU" (= operator).
    pub const OPCODE_EQU: u8 = 0x01;

    /// "ADDEQU" (+= operator).
    pub const OPCODE_ADDEQU: u8 = 0x02;

    /// "SUBEQU" (-= operator).
    pub const OPCODE_SUBEQU: u8 = 0x03;

    /// "MULEQU" (*= operator).
    pub const OPCODE_MULEQU: u8 = 0x04;

    /// "DIVEQU" (/= operator).
    pub const OPCODE_DIVEQU: u8 = 0x05;

    /// "MODEQU" (%= operator).
    pub const OPCODE_MODEQU: u8 = 0x06;

    /// "ADD" (+ operator).
    pub const OPCODE_ADD: u8 = 0x07;

    /// "SUB" (- operator).
    pub const OPCODE_SUB: u8 = 0x08;

    /// "MUL" (* operator).
    pub const OPCODE_MUL: u8 = 0x09;

    /// "DIV" (/ operator).
    pub const OPCODE_DIV: u8 = 0x0A;

    /// "MOD" (% operator).
    pub const OPCODE_MOD: u8 = 0x0B;

    /// "AND" (&& operator).
    pub const OPCODE_AND: u8 = 0x0C;

    /// "OR" (|| operator).
    pub const OPCODE_OR: u8 = 0x0D;

    /// "INC" (Increment).
    pub const OPCODE_INC: u8 = 0x0E;

    /// "DEC" (Decrement).
    pub const OPCODE_DEC: u8 = 0x0F;

    /// "NEG" (Negate: unary - operator).
    pub const OPCODE_NEG: u8 = 0x10;

    /// "NOT" (Logical not: ! operator).
    pub const OPCODE_NOT: u8 = 0x11;

    // "CMP" (Compare).
    pub const OPCODE_CMP: u8 = 0x12;

    pub const OPCODE_PUSHV: u8 = 0x13;
    pub const OPCODE_POPV: u8 = 0x14;
    pub const OPCODE_DUP: u8 = 0x15;
    pub const OPCODE_DISC: u8 = 0x16;
    pub const OPCODE_PUSH32: u8 = 0x17;
    pub const OPCODE_JMP: u8 = 0x18;
    pub const OPCODE_BLT: u8 = 0x19;
    pub const OPCODE_BLE: u8 = 0x1A;
    pub const OPCODE_BEQ: u8 = 0x1B;
    pub const OPCODE_BNE: u8 = 0x1C;
    pub const OPCODE_BGE: u8 = 0x1D;
    pub const OPCODE_BGT: u8 = 0x1E;

    /// "END" (End of script).
    pub const OPCODE_END: u8 = 0x20;
    pub const OPCODE_CALL: u8 = 0x21;
    pub const OPCODE_PUSH16: u8 = 0x22;
    pub const OPCODE_PUSH8: u8 = 0x23;
    pub const OPCODE_SWITCH: u8 = 0x24;

    /// "JPI" (Jump Indirect).
    /// This opcode isn't used in base game scripts, but its interpretation is implemented in the script engine.
    #[allow(unused)]
    pub const OPCODE_JPI: u8 = 0x1F;
}

mod decoder;
mod encoder;

pub use decoder::{decode_script, DecodeError};
pub use encoder::encode_script;

#[cfg(test)]
mod tests {
    use crate::ir::{CallId, Ins, JumpId, Script, VarId};

    use super::*;

    #[test]
    fn test_encode_decode_empty() {
        let script = Script {
            instructions: vec![],
            strings: vec![],
        };

        let data = encoder::encode_script(&script);
        let decoded = decoder::decode_script(&mut &data[..]).unwrap();

        assert_eq!(script.instructions, decoded.instructions);
    }

    #[test]
    fn test_encode_decode_many() {
        let scripts = [
            Script {
                instructions: vec![Ins::PushInt(0), Ins::Call(CallId(1)), Ins::Discard],
                strings: vec![],
            },
            Script {
                instructions: vec![
                    Ins::PushInt(0),
                    Ins::PushInt(10),
                    Ins::Assign,
                    Ins::Discard,
                    Ins::Label(JumpId(2)),
                    Ins::Call(CallId(2)),
                    Ins::PushVar(VarId(0)),
                    Ins::Dec,
                    Ins::Dupe,
                    Ins::PopVar(VarId(0)),
                    Ins::Discard,
                    Ins::PushVar(VarId(0)),
                    Ins::PushInt(0),
                    Ins::Cmp,
                    Ins::Bne(JumpId(0)),
                    Ins::PushInt(0),
                    Ins::Jmp(JumpId(1)),
                    Ins::Label(JumpId(0)),
                    Ins::PushInt(1),
                    Ins::Label(JumpId(1)),
                    Ins::Bne(JumpId(2)),
                ],
                strings: vec![],
            },
        ];

        for script in scripts {
            let data = encoder::encode_script(&script);
            let decoded = decoder::decode_script(&mut &data[..]).unwrap();

            assert_eq!(script.instructions, decoded.instructions);
        }
    }
}
