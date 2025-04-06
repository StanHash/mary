use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    io,
    str::Utf8Error,
};

use thiserror::Error;

use crate::ir::{CallId, CaseEnum, Ins, IntValue, JumpId, Script, StrValue, SwitchId, VarId};

use super::opcodes::*;

#[derive(Error, Debug)]
pub enum DecodeError {
    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),

    #[error("Script Error: {0}")]
    ScriptError(&'static str),

    #[error("Corrupt chunk name")]
    BadChunkName(#[from] Utf8Error),

    #[error("Missing CODE chunk")]
    MissingCodeChunk,

    #[error("Misaligned jump table offset: {0:02X}")]
    MisalignedJumpTable(usize),

    #[error("CODE chunk size mismatch")]
    BadCodeChunk,

    #[error("Corrupt STR chunk")]
    BadStrChunk,
}

// This is pub(crate) as it is used in encoder tests also
// This could be moved elsewhere for that reason
#[allow(dead_code)]
pub(crate) trait DecodeHelper {
    fn read_u32(&mut self) -> io::Result<u32>;
    fn read_u16(&mut self) -> io::Result<u16>;
    fn read_u8(&mut self) -> io::Result<u8>;
    fn read_4byte(&mut self) -> io::Result<[u8; 4]>;
}

impl<R: io::Read> DecodeHelper for R {
    fn read_u32(&mut self) -> io::Result<u32> {
        let mut buf = [0u8; 4];
        self.read_exact(&mut buf)?;

        Ok((buf[0] as u32)
            | ((buf[1] as u32) << 8)
            | ((buf[2] as u32) << 16)
            | ((buf[3] as u32) << 24))
    }

    fn read_u16(&mut self) -> io::Result<u16> {
        let mut buf = [0u8; 2];
        self.read_exact(&mut buf)?;

        Ok((buf[0] as u16) | ((buf[1] as u16) << 8))
    }

    fn read_u8(&mut self) -> io::Result<u8> {
        let mut buf = [0u8; 1];
        self.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    fn read_4byte(&mut self) -> io::Result<[u8; 4]> {
        let mut buf = [0u8; 4];
        self.read_exact(&mut buf)?;
        Ok(buf)
    }
}

#[derive(Debug, Clone, Copy)]
struct JumpInfo {
    source_offset: usize,
    target_offset: usize,
    jump_id: JumpId,
}

impl JumpInfo {
    fn new(source_offset: usize, target_offset: usize, jump_id: JumpId) -> Self {
        Self {
            source_offset,
            target_offset,
            jump_id,
        }
    }
}

#[derive(Debug)]
struct JumpIdMap(Vec<JumpInfo>);

fn operand_size(opcode: u8) -> usize {
    match opcode {
        OPCODE_PUSHV => 4,  // push [A]
        OPCODE_POPV => 4,   // pop [A]
        OPCODE_PUSH32 => 4, // push imm32
        OPCODE_PUSH16 => 2, // push imm16
        OPCODE_PUSH8 => 1,  // push imm8
        OPCODE_JMP => 4,    // b label
        OPCODE_BLT => 4,    // blt label
        OPCODE_BLE => 4,    // ble label
        OPCODE_BEQ => 4,    // beq label
        OPCODE_BNE => 4,    // bne label
        OPCODE_BGE => 4,    // bge label
        OPCODE_BGT => 4,    // bgt label
        OPCODE_CALL => 4,   // call id
        OPCODE_SWITCH => 4, // switch id
        _ => 0,
    }
}

fn get_code_jumps(code_data: &[u8]) -> Result<JumpIdMap, DecodeError> {
    let mut offset = 0;

    let mut jump_id_counter = 0;

    /* second item is difference from jump location to target location (used for sorting) */
    let mut extended_jump_map: Vec<(JumpInfo, isize)> = Vec::new();

    while offset < code_data.len() {
        let opcode = code_data[offset];
        offset += 1;

        if let OPCODE_JMP | OPCODE_BEQ | OPCODE_BNE | OPCODE_BLE | OPCODE_BLT | OPCODE_BGE
        | OPCODE_BGT = opcode
        {
            let jump_id = JumpId(jump_id_counter);
            let target_offset = (&code_data[offset..offset + 4]).read_u32()? as usize;
            let disp = (target_offset as isize) - (offset as isize);
            let jump_info = JumpInfo::new(offset - 1, target_offset, jump_id);

            extended_jump_map.push((jump_info, disp));
            jump_id_counter += 1;
        }

        offset += operand_size(opcode);
    }

    extended_jump_map.sort_by(|a, b| {
        let (a_info, a_disp) = *a;
        let (b_info, b_disp) = *b;

        if a_info.target_offset == b_info.target_offset {
            if (a_disp < 0 && b_disp < 0) || (a_disp > 0 && b_disp > 0) {
                /* Both are same direction jumps, ascending order */
                a_disp.cmp(&b_disp)
            } else {
                /* Forwards before backwards, descending order */
                b_disp.cmp(&a_disp)
            }
        } else {
            /* different jump offset, by order of jumps */
            a_info.target_offset.cmp(&b_info.target_offset)
        }
    });

    let jump_map = extended_jump_map.iter().map(|(info, _)| *info).collect();

    Ok(JumpIdMap(jump_map))
}

#[derive(Default)]
struct CaseTable(Vec<(CaseEnum, usize)>);

struct CaseMap(Vec<(usize, SwitchId, CaseEnum)>);

#[derive(Default)]
struct DecodedJumpChunk {
    case_tables: Vec<CaseTable>,
}

fn decode_jump_chunk(data: &[u8]) -> Result<DecodedJumpChunk, DecodeError> {
    let read = &mut &data[..];
    let ent_count = read.read_u32()? as usize;

    let mut offs = vec![];

    for _ in 0..ent_count {
        let off = read.read_u32()? as usize;
        offs.push(off);

        if off % 4 != 0 {
            return Err(DecodeError::MisalignedJumpTable(off));
        }
    }

    let mut case_tables = vec![];

    for _ in 0..ent_count {
        let entries = read.read_u32()? as usize;
        let default = read.read_u32()? as usize;

        let mut case_table = CaseTable::default();

        if (default & 0x80000000) == 0 {
            case_table.0.push((CaseEnum::Default, default));
        }

        for _ in 0..entries {
            /* it's important here that we convert to i32 for JUMP sorting reasons
             * (game does signed compares, so any -1 need to be before any 0s)
             * raw cast to IntValue (i64) would result in -1 => 0xFFFFFFFF */

            let compare = read.read_u32()? as i32 as IntValue;
            let target = read.read_u32()? as usize;

            case_table.0.push((CaseEnum::Val(compare), target));
        }

        case_tables.push(case_table);
    }

    Ok(DecodedJumpChunk { case_tables })
}

fn build_case_map(jump_chunk: &DecodedJumpChunk) -> CaseMap {
    let mut case_map = Vec::new();

    for i in 0..jump_chunk.case_tables.len() {
        let switch_id = SwitchId(i);

        for &(case_enum, code_offset) in &jump_chunk.case_tables[i].0 {
            case_map.push((code_offset, switch_id, case_enum));
        }
    }

    /* this is a stable sort (this is important) */
    case_map.sort_by_key(|(code_offset, _, _)| *code_offset);

    CaseMap(case_map)
}

fn decode_code_chunk(code_data: &[u8]) -> Result<&[u8], DecodeError> {
    let head_size = (&code_data[0..]).read_u32()? as usize;

    if head_size != code_data.len() - 4 {
        return Err(DecodeError::BadCodeChunk);
    }

    let code_with_nops = &code_data[4..];

    let mut real_end = code_with_nops.len() - 1;

    while code_with_nops[real_end] != OPCODE_END {
        real_end -= 1;
    }

    /* we discard the last end to that when decompiling/reencoding we don't emit an extra exit */
    Ok(&code_with_nops[..real_end])
}

fn disassemble(
    code_data: &[u8],
    jump_targets: &JumpIdMap,
    case_map: &CaseMap,
) -> Result<Vec<Ins>, DecodeError> {
    let mut result = vec![];

    let mut offset = 0;

    let mut current_case_target_idx = 0;
    let mut current_jump_target_idx = 0;

    let mut jump_source_map = BTreeMap::<usize, JumpId>::new();

    for jump_info in &jump_targets.0 {
        jump_source_map.insert(jump_info.source_offset, jump_info.jump_id);
    }

    while offset < code_data.len() {
        /* before doing any decoding, let's insert any labels or cases here */

        while current_jump_target_idx < jump_targets.0.len() {
            let head = jump_targets.0[current_jump_target_idx];

            if head.target_offset > offset {
                break;
            }

            result.push(Ins::Label(head.jump_id));
            current_jump_target_idx += 1;
        }

        while current_case_target_idx < case_map.0.len() {
            let (target_offset, switch_id, case_id) = case_map.0[current_case_target_idx];

            if target_offset > offset {
                break;
            }

            result.push(Ins::Case(switch_id, case_id));
            current_case_target_idx += 1;
        }

        let pc = offset;

        let opcode = code_data[offset];
        offset += 1;

        let operand = match operand_size(opcode) {
            0 => 0,

            1 => {
                offset += 1;
                code_data[offset - 1] as usize
            }

            2 => {
                offset += 2;
                (&code_data[offset - 2..offset]).read_u16()? as usize
            }

            4 => {
                offset += 4;
                (&code_data[offset - 4..offset]).read_u32()? as usize
            }

            _ => unreachable!(),
        };

        match opcode {
            OPCODE_NOP => {}

            OPCODE_EQU => result.push(Ins::Assign),
            OPCODE_ADDEQU => result.push(Ins::AssignAdd),
            OPCODE_SUBEQU => result.push(Ins::AssignSub),
            OPCODE_MULEQU => result.push(Ins::AssignMul),
            OPCODE_DIVEQU => result.push(Ins::AssignDiv),
            OPCODE_MODEQU => result.push(Ins::AssignMod),

            OPCODE_ADD => result.push(Ins::Add),
            OPCODE_SUB => result.push(Ins::Sub),
            OPCODE_MUL => result.push(Ins::Mul),
            OPCODE_DIV => result.push(Ins::Div),
            OPCODE_MOD => result.push(Ins::Mod),

            OPCODE_AND => result.push(Ins::LogicalAnd),
            OPCODE_OR => result.push(Ins::LogicalOr),

            OPCODE_INC => result.push(Ins::Inc),
            OPCODE_DEC => result.push(Ins::Dec),

            OPCODE_NEG => result.push(Ins::Neg),
            OPCODE_NOT => result.push(Ins::LogicalNot),

            OPCODE_CMP => result.push(Ins::Cmp),

            OPCODE_PUSHV => result.push(Ins::PushVar(VarId(operand))),
            OPCODE_POPV => result.push(Ins::PopVar(VarId(operand))),

            OPCODE_DUP => result.push(Ins::Dupe),
            OPCODE_DISC => result.push(Ins::Discard),

            OPCODE_PUSH32 | OPCODE_PUSH16 | OPCODE_PUSH8 => {
                result.push(Ins::PushInt(operand as i32 as IntValue));
            }

            OPCODE_JMP => result.push(Ins::Jmp(jump_source_map[&pc])),
            OPCODE_BLT => result.push(Ins::Blt(jump_source_map[&pc])),
            OPCODE_BLE => result.push(Ins::Ble(jump_source_map[&pc])),
            OPCODE_BEQ => result.push(Ins::Beq(jump_source_map[&pc])),
            OPCODE_BNE => result.push(Ins::Bne(jump_source_map[&pc])),
            OPCODE_BGE => result.push(Ins::Bge(jump_source_map[&pc])),
            OPCODE_BGT => result.push(Ins::Bgt(jump_source_map[&pc])),

            OPCODE_END => result.push(Ins::Exit),

            OPCODE_CALL => result.push(Ins::Call(CallId(operand))),
            OPCODE_SWITCH => result.push(Ins::Switch(SwitchId(operand))),

            _ => {
                /* TODO: graceful error */
                panic!("Bad opcode")
            }
        }
    }

    while current_jump_target_idx < jump_targets.0.len() {
        let head = jump_targets.0[current_jump_target_idx];
        result.push(Ins::Label(head.jump_id));
        current_jump_target_idx += 1;
    }

    Ok(result)
}

fn read_str(data: &[u8]) -> &[u8] {
    let mut end_off = 0;

    while end_off < data.len() && data[end_off] != 0 {
        end_off += 1;
    }

    &data[0..end_off]
}

fn decode_string_chunk(strings_data: &[u8]) -> Result<Vec<StrValue>, DecodeError> {
    let string_count = (&strings_data[0..4]).read_u32()? as usize;

    let pool_offset = 4 + 4 * string_count;

    if pool_offset > strings_data.len() {
        return Err(DecodeError::BadStrChunk);
    }

    let string_offsets = &strings_data[4..pool_offset];
    let string_pool = &strings_data[pool_offset..];

    let mut string_table = vec![];

    for i in 0..string_count {
        let offset = (&(string_offsets[i * 4..])).read_u32()? as usize;

        if offset > strings_data.len() {
            return Err(DecodeError::BadStrChunk);
        }

        string_table.push(read_str(&string_pool[offset..]).to_vec());
    }

    Ok(string_table)
}

fn read_riff_chunks<R: io::Read>(
    read: &mut R,
    riff_size: usize,
) -> Result<HashMap<String, Vec<u8>>, DecodeError> {
    let mut chunks = HashMap::new();
    let mut offset = 12usize;

    while offset < riff_size {
        let chunk_name = read.read_4byte()?;
        let chunk_size = read.read_u32()? as usize;

        let mut chunk_data = vec![0u8; chunk_size];
        read.read(&mut chunk_data)?;

        let chunk_name = std::str::from_utf8(&chunk_name)?.to_string();
        chunks.insert(chunk_name, chunk_data);

        offset = offset + chunk_size + 8
    }

    Ok(chunks)
}

pub fn decode_script<R: io::Read>(read: &mut R) -> Result<Script, DecodeError> {
    /*
     * Step 1: Decode RIFF header.
     */

    let riff_head = read.read_4byte()?;
    let riff_size = read.read_u32()? as usize;
    let riff_name = read.read_4byte()?;

    if riff_head.cmp(b"RIFF") != Ordering::Equal || riff_name.cmp(b"SCR ") != Ordering::Equal {
        return Err(DecodeError::ScriptError(
            "Not a valid FoMT script binary (invalid magics)",
        ));
    }

    /*
     * Step 2: Decode RIFF structure (chunks).
     */

    let chunks = read_riff_chunks(read, riff_size)?;

    /*
     * Step 2.1: Decode CODE
     */

    let code_data = match chunks.get("CODE") {
        Some(code_data) => decode_code_chunk(code_data)?,
        None => return Err(DecodeError::MissingCodeChunk),
    };

    /*
     * Step 3: Find jump target and sources.
     */

    /* Step 3.1: Scan code for jumps. */

    let jump_map = get_code_jumps(code_data)?;

    /* Step 3.2: Decode JUMP chunk, if any. */

    let jumps = match chunks.get("JUMP") {
        Some(data) => decode_jump_chunk(data)?,
        None => DecodedJumpChunk::default(),
    };

    let case_map = build_case_map(&jumps);

    /* Step 4: Disassemble, while adding case and label meta-instructions */

    let instructions = disassemble(code_data, &jump_map, &case_map)?;

    /* Step 5: Get string table */

    let strings = match chunks.get("STR ") {
        Some(data) => decode_string_chunk(data)?,
        None => vec![],
    };

    /* DONE */

    Ok(Script::new(instructions, strings))
}
