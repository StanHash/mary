use std::{collections::BTreeMap, iter::repeat};

use crate::ir::{CaseEnum, Ins, IntValue, Script, StrValue};

const OPCODE_NOP: u8 = 0x00;
const OPCODE_EQU: u8 = 0x01;
const OPCODE_ADDEQU: u8 = 0x02;
const OPCODE_SUBEQU: u8 = 0x03;
const OPCODE_MULEQU: u8 = 0x04;
const OPCODE_DIVEQU: u8 = 0x05;
const OPCODE_MODEQU: u8 = 0x06;
const OPCODE_ADD: u8 = 0x07;
const OPCODE_SUB: u8 = 0x08;
const OPCODE_MUL: u8 = 0x09;
const OPCODE_DIV: u8 = 0x0A;
const OPCODE_MOD: u8 = 0x0B;
const OPCODE_AND: u8 = 0x0C;
const OPCODE_OR: u8 = 0x0D;
const OPCODE_INC: u8 = 0x0E;
const OPCODE_DEC: u8 = 0x0F;
const OPCODE_NEG: u8 = 0x10;
const OPCODE_NOT: u8 = 0x11;
const OPCODE_CMP: u8 = 0x12;
const OPCODE_PUSHV: u8 = 0x13;
const OPCODE_POPV: u8 = 0x14;
const OPCODE_DUP: u8 = 0x15;
const OPCODE_DISC: u8 = 0x16;
const OPCODE_PUSH32: u8 = 0x17;
const OPCODE_JMP: u8 = 0x18;
const OPCODE_BLT: u8 = 0x19;
const OPCODE_BLE: u8 = 0x1A;
const OPCODE_BEQ: u8 = 0x1B;
const OPCODE_BNE: u8 = 0x1C;
const OPCODE_BGE: u8 = 0x1D;
const OPCODE_BGT: u8 = 0x1E;
// const OPCODE_JPI: u8 = 0x1F;
const OPCODE_END: u8 = 0x20;
const OPCODE_CALL: u8 = 0x21;
const OPCODE_PUSH16: u8 = 0x22;
const OPCODE_PUSH8: u8 = 0x23;
const OPCODE_SWITCH: u8 = 0x24;

trait EncoderHelper {
    fn push_u8(&mut self, val: u8);

    fn push_u16(&mut self, val: u16) {
        self.push_u8((val & 0x000000FF) as u8);
        self.push_u8(((val & 0x0000FF00) >> 8) as u8);
    }

    fn push_u32(&mut self, val: u32) {
        self.push_u8((val & 0x000000FF) as u8);
        self.push_u8(((val & 0x0000FF00) >> 8) as u8);
        self.push_u8(((val & 0x00FF0000) >> 16) as u8);
        self.push_u8(((val & 0xFF000000) >> 24) as u8);
    }
}

impl EncoderHelper for Vec<u8> {
    fn push_u8(&mut self, val: u8) {
        self.push(val);
    }
}

struct SlicePatch<'a>(&'a mut [u8]);

impl<'a> EncoderHelper for SlicePatch<'a> {
    fn push_u8(&mut self, val: u8) {
        use std::mem;
        self.0[0] = val;
        self.0 = &mut mem::take(&mut self.0)[1..];
    }
}

fn write_push(vec: &mut Vec<u8>, val: IntValue) {
    let val = val & 0xFFFFFFFF;

    // NOTE: those constants are a bit off
    // they could be 0x100 and 0x10000

    if val < 0x80 {
        vec.push(OPCODE_PUSH8);
        vec.push(val as u8);
    } else if val < 0x8000 {
        vec.push(OPCODE_PUSH16);
        vec.push_u16(val as u16);
    } else {
        vec.push(OPCODE_PUSH32);
        vec.push_u32(val as u32);
    }
}

struct JumpTables(Vec<Vec<(CaseEnum, usize)>>);

fn encode_code(vec: &mut Vec<u8>, ins_seq: &[Ins]) -> JumpTables {
    let mut label_map = BTreeMap::new();
    let mut jump_map = BTreeMap::new();
    let mut switch_map = BTreeMap::new();
    let mut switches = Vec::new();

    // placeholder for code size
    vec.push_u32(0);

    let begin = vec.len();

    for ins in ins_seq {
        match ins {
            Ins::Assign => vec.push(OPCODE_EQU),
            Ins::AssignAdd => vec.push(OPCODE_ADDEQU),
            Ins::AssignSub => vec.push(OPCODE_SUBEQU),
            Ins::AssignMul => vec.push(OPCODE_MULEQU),
            Ins::AssignDiv => vec.push(OPCODE_DIVEQU),
            Ins::AssignMod => vec.push(OPCODE_MODEQU),
            Ins::Add => vec.push(OPCODE_ADD),
            Ins::Sub => vec.push(OPCODE_SUB),
            Ins::Mul => vec.push(OPCODE_MUL),
            Ins::Div => vec.push(OPCODE_DIV),
            Ins::Mod => vec.push(OPCODE_MOD),
            Ins::LogicalAnd => vec.push(OPCODE_AND),
            Ins::LogicalOr => vec.push(OPCODE_OR),
            Ins::Inc => vec.push(OPCODE_INC),
            Ins::Dec => vec.push(OPCODE_DEC),
            Ins::Neg => vec.push(OPCODE_NEG),
            Ins::LogicalNot => vec.push(OPCODE_NOT),
            Ins::Cmp => vec.push(OPCODE_CMP),

            Ins::PushVar(vid) => {
                vec.push(OPCODE_PUSHV);
                vec.push_u32(vid.0 as u32);
            }

            Ins::PopVar(vid) => {
                vec.push(OPCODE_POPV);
                vec.push_u32(vid.0 as u32);
            }

            Ins::Dupe => vec.push(OPCODE_DUP),
            Ins::Discard => vec.push(OPCODE_DISC),

            Ins::PushInt(val) => write_push(vec, *val),

            Ins::Jmp(jid) => {
                vec.push(OPCODE_JMP);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Blt(jid) => {
                vec.push(OPCODE_BLT);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Ble(jid) => {
                vec.push(OPCODE_BLE);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Beq(jid) => {
                vec.push(OPCODE_BEQ);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Bne(jid) => {
                vec.push(OPCODE_BNE);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Bge(jid) => {
                vec.push(OPCODE_BGE);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Bgt(jid) => {
                vec.push(OPCODE_BGT);
                jump_map.insert(vec.len(), *jid);
                vec.push_u32(0);
            }

            Ins::Call(cid) => {
                vec.push(OPCODE_CALL);
                vec.push_u32(cid.0 as u32);
            }

            Ins::Switch(id) => {
                let id = switch_map.entry(*id).or_insert_with(|| {
                    switches.push(Vec::new());
                    switches.len() - 1
                });

                vec.push(OPCODE_SWITCH);
                vec.push_u32(*id as u32);
            }

            Ins::Case(id, en) => {
                let id = *switch_map.entry(*id).or_insert_with(|| {
                    switches.push(Vec::new());
                    switches.len() - 1
                });

                switches[id].push((*en, vec.len()));
            }

            Ins::Label(jid) => {
                label_map.insert(*jid, vec.len());
            }
        }
    }

    // apply jump offsets
    for (off, target) in jump_map {
        let target_off = label_map[&target];
        SlicePatch(&mut vec[off..]).push_u32(target_off as u32);
    }

    vec.push(OPCODE_END);

    let len_aligned = (vec.len() - begin + 3) & !3;

    while vec.len() - begin < len_aligned {
        vec.push(OPCODE_NOP);
    }

    SlicePatch(&mut vec[begin - 4..]).push_u32(len_aligned as u32);

    JumpTables(switches)
}

fn encode_jump(vec: &mut Vec<u8>, jump_tables: JumpTables) {
    let jump_tables = jump_tables.0;

    vec.push_u32(jump_tables.len() as u32);

    let table_begin = vec.len();
    vec.extend(repeat(0).take(4 * jump_tables.len()));

    let data_begin = vec.len();

    for (i, jt) in jump_tables.iter().enumerate() {
        let off = vec.len() - data_begin;
        SlicePatch(&mut vec[table_begin + 4 * i..]).push_u32((off / 4) as u32);

        // handle default offset

        let mut default_off = None;

        for (e, off) in jt {
            if let CaseEnum::Default = e {
                default_off = Some(*off);
                break;
            }
        }

        match default_off {
            Some(off) => {
                vec.push_u32((jt.len() - 1) as u32);
                vec.push_u32(off as u32);
            }

            None => {
                vec.push_u32(jt.len() as u32);
                vec.push_u32(0xFFFFFFFF);
            }
        }

        // values need to be sorted, we go through an intermediate vec

        let mut values = Vec::with_capacity(jt.len());

        for (e, off) in jt {
            if let CaseEnum::Val(val) = e {
                values.push((*val, *off));
            }
        }

        values.sort();

        for (val, off) in values {
            vec.push_u32(val as u32);
            vec.push_u32(off as u32);
        }
    }
}

fn encode_str(vec: &mut Vec<u8>, str_tab: &Vec<StrValue>) {
    vec.push_u32(str_tab.len() as u32);

    // alloc space for offset table
    let table_begin = vec.len();
    vec.extend(repeat(0).take(4 * str_tab.len()));

    let data_begin = vec.len();

    for (i, str) in str_tab.iter().enumerate() {
        let off = vec.len() - data_begin;
        SlicePatch(&mut vec[table_begin + 4 * i..]).push_u32(off as u32);

        vec.extend(str);
        vec.push(0); // strings are null-terminated
    }
}

pub fn encode_script(script: &Script) -> Vec<u8> {
    let mut vec = Vec::new();

    // "RIFF" magic
    vec.extend(b"RIFF");

    // placeholder for length
    vec.push_u32(0);

    // "SCR " magic
    vec.extend(b"SCR ");

    // first chunk is always CODE

    let chunk_hook = vec.len();
    vec.extend(b"CODE");
    vec.push_u32(0);

    let jump_tables = encode_code(&mut vec, &script.ins_seq);

    let chunk_len = vec.len() - chunk_hook - 8;
    SlicePatch(&mut vec[chunk_hook + 4..]).push_u32(chunk_len as u32);

    // second chunk is JUMP, if present

    if jump_tables.0.len() != 0 {
        let chunk_hook = vec.len();
        vec.extend(b"JUMP");
        vec.push_u32(0);

        encode_jump(&mut vec, jump_tables);

        let chunk_len = vec.len() - chunk_hook - 8;
        SlicePatch(&mut vec[chunk_hook + 4..]).push_u32(chunk_len as u32);
    }

    // last chunk is STR, if present

    if script.str_tab.len() != 0 {
        let chunk_hook = vec.len();
        vec.extend(b"STR ");
        vec.push_u32(0);

        encode_str(&mut vec, &script.str_tab);

        let chunk_len = vec.len() - chunk_hook - 8;
        SlicePatch(&mut vec[chunk_hook + 4..]).push_u32(chunk_len as u32);
    }

    // update length at start
    let len = vec.len();
    SlicePatch(&mut vec[4..]).push_u32(len as u32);

    vec
}
