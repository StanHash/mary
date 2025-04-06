use std::{
    collections::{HashMap, HashSet},
    fs, io,
    path::PathBuf,
};

use clap::Parser;
use mary::{
    bytecode::{self, DecodeError},
    ir::{CallId, Ins, Script},
    utility::rom_info,
};
use thiserror::Error;

const MAX_VALUES: usize = 99999;

#[derive(Debug, Clone, Copy)]
struct StackDisp {
    min: usize,
    max: usize,
}

impl Default for StackDisp {
    fn default() -> Self {
        Self {
            min: 0,
            max: MAX_VALUES,
        }
    }
}

impl StackDisp {
    fn zero() -> Self {
        Self { min: 0, max: 0 }
    }
}

#[derive(Debug)]
struct BlockInfo {
    instructions: Vec<Ins>,
    stack_disp: Vec<StackDisp>,

    #[allow(dead_code)]
    script_id: usize,
}

impl BlockInfo {
    fn new(instructions: Vec<Ins>, script_id: usize) -> Self {
        let mut stack_disp = Vec::with_capacity(instructions.len() + 1);
        stack_disp.resize_with(instructions.len() + 1, StackDisp::default);

        Self {
            instructions,
            stack_disp,
            script_id,
        }
    }

    fn len(&self) -> usize {
        self.instructions.len()
    }
}

#[derive(Debug)]
struct CallableInfo {
    is_func: Option<bool>,
    min_params: usize,
    max_params: usize,
}

impl Default for CallableInfo {
    fn default() -> Self {
        Self {
            is_func: None,
            min_params: 0,
            max_params: MAX_VALUES,
        }
    }
}

// these were used by knowledge preloading hacks that allowed for complete analysis
#[allow(dead_code)]
impl CallableInfo {
    fn new_func() -> Self {
        Self {
            is_func: Some(true),
            min_params: 0,
            max_params: MAX_VALUES,
        }
    }

    fn new_proc() -> Self {
        Self {
            is_func: Some(false),
            min_params: 0,
            max_params: MAX_VALUES,
        }
    }

    fn with_params(self, params: usize) -> Self {
        Self {
            is_func: self.is_func,
            min_params: params,
            max_params: params,
        }
    }
}

fn split_instructions(instructions: &[Ins], script_id: usize) -> Vec<BlockInfo> {
    /* we want to create balanced blocks. So special consideration need to be
     * taken reguarding cmp and switch constructs */

    let mut switch_jumps = HashSet::new();

    for i in 0..instructions.len() {
        if let [Ins::Label(jump_id), Ins::Switch(_), ..] = &instructions[i..] {
            switch_jumps.insert(*jump_id);
        }
    }

    let mut blocks = vec![];
    let mut current_block = vec![];

    let mut offset = 0;

    while offset < instructions.len() {
        match &instructions[offset..] {
            [Ins::Cmp, _, Ins::PushInt(_), Ins::Jmp(_), Ins::Label(_), Ins::PushInt(_), Ins::Label(_), ..] =>
            {
                /* skip the whole "convert the cmp result into a boolean" sequence and mark it as just a cmp
                 * we do that to simplify processing, as otherwise this would become a bunch of unbalanced blocks */

                current_block.push(Ins::Cmp);
                offset += 7;
            }

            [Ins::Label(_), ..] | [Ins::Case(_, _), ..] => {
                /* cut before this instruction */

                if !current_block.is_empty() {
                    blocks.push(BlockInfo::new(current_block, script_id));
                    current_block = vec![instructions[offset]];
                    offset += 1;
                } else {
                    current_block.push(instructions[offset]);
                    offset += 1;
                }
            }

            [Ins::Jmp(jump_id), ..] if switch_jumps.contains(jump_id) => {
                current_block.push(Ins::Discard); /* discard to make block balanced */
                current_block.push(instructions[offset]);
                blocks.push(BlockInfo::new(current_block, script_id));
                current_block = vec![];

                offset += 1;
            }

            [Ins::Switch(_), ..] => {
                /* ignore this instruction */

                if !current_block.is_empty() {
                    blocks.push(BlockInfo::new(current_block, script_id));
                    current_block = vec![];
                }

                offset += 1;
            }

            [Ins::Discard, ..] | [Ins::Exit, ..] => {
                /* cut after this instruction
                 * this is the last instruction of a statement */

                current_block.push(instructions[offset]);
                blocks.push(BlockInfo::new(current_block, script_id));
                current_block = vec![];

                offset += 1;
            }

            [Ins::Assign, Ins::Discard, ..] => {
                /* cut after this instruction
                 * this is the last instruction of a statement */

                current_block.push(instructions[offset]);
                current_block.push(instructions[offset + 1]);
                blocks.push(BlockInfo::new(current_block, script_id));
                current_block = vec![];

                offset += 2;
            }

            [Ins::Assign, ..] => {
                /* for some reason, sometimes, the first assign instruction of a script
                 * isn't followed by a discard. This is a bug that breaks analysis,
                 * we insert an artificial discard here. */

                current_block.push(instructions[offset]);
                current_block.push(Ins::Discard);
                blocks.push(BlockInfo::new(current_block, script_id));
                current_block = vec![];

                offset += 1;
            }

            [Ins::Jmp(_), ..]
            | [Ins::Beq(_), ..]
            | [Ins::Bne(_), ..]
            | [Ins::Bgt(_), ..]
            | [Ins::Bge(_), ..]
            | [Ins::Blt(_), ..]
            | [Ins::Ble(_), ..] => {
                /* cut after this instruction */

                current_block.push(instructions[offset]);
                blocks.push(BlockInfo::new(current_block, script_id));
                current_block = vec![];

                offset += 1;
            }

            _ => {
                current_block.push(instructions[offset]);
                offset += 1;
            }
        }
    }

    if !current_block.is_empty() {
        blocks.push(BlockInfo::new(current_block, script_id));
    }

    blocks
}

fn instruction_stack_disp(instruction: &Ins) -> isize {
    use Ins::*;

    match instruction {
        Assign | AssignAdd | AssignSub | AssignMul | AssignDiv | AssignMod => -1,

        Add | Sub | Mul | Div | Mod | LogicalAnd | LogicalOr => -1,

        Inc | Dec => 0,

        Neg | LogicalNot => 0,

        Cmp => -1,

        PushVar(_) | PushInt(_) => 1,

        PopVar(_) => -1,
        Discard => -1,

        Dupe => 1,

        Jmp(_) => 0,

        Blt(_) | Ble(_) | Beq(_) | Bne(_) | Bge(_) | Bgt(_) => -1,

        Call(_) => panic!(),

        Exit => 0,

        Switch(_) => -1,
        Case(_, _) => 0,
        Label(_) => 0,
    }
}

fn update_stats_from_disps(info: &mut CallableInfo, before: StackDisp, after: StackDisp) {
    let is_proc = after.max == 0;
    let is_func = after.min > 0;

    if is_proc || is_func {
        info.is_func = Some(is_func);
    }

    let (local_min_params, local_max_params) = match info.is_func {
        Some(true) => (
            before.min.saturating_sub(after.max.saturating_sub(1)),
            before.max.saturating_sub(after.min.saturating_sub(1)),
        ),

        /* procedures should drain the stack */
        Some(false) => (before.min, before.max),

        None => (
            before.min.saturating_sub(after.max),
            before.max.saturating_sub(after.min.saturating_sub(1)),
        ),
    };

    /* TODO: why does this happen ? */
    if local_min_params <= local_max_params {
        info.min_params = info.min_params.max(local_min_params);
        info.max_params = info.max_params.min(local_max_params);
    }
}

fn analyse_backwards(block: &mut BlockInfo, current_knowledge: &mut HashMap<CallId, CallableInfo>) {
    /* for each instruction, we give a minimum and maximum stack disp. */

    /* some important truths:
     * a procedure call will always get the stack disp at 0
     * a function call will never get the stack disp at 0
     * so if we go backwards, we would know. */

    block.stack_disp[block.instructions.len()] = StackDisp::zero();

    /* from end to start */
    for i in (0..block.len()).rev() {
        /* the state of the stack at i + 1 depends on the instruction at i, which is what we check */

        let next_disp = block.stack_disp[i + 1];
        let current_disp = &mut block.stack_disp[i];

        match &block.instructions[i] {
            Ins::Call(call_id) => {
                /* get or insert entry */

                let info = current_knowledge
                    .entry(*call_id)
                    .or_insert_with(CallableInfo::default);

                update_stats_from_disps(info, *current_disp, next_disp);

                /* now, update the emulated stack */

                /* knowing this is a function would raise output min by 1 */
                let (min_produced, max_produced) = match info.is_func {
                    Some(true) => (1, 1),
                    Some(false) => (0, 0),
                    None => (0, 1),
                };

                /* we remove produced and add consumed */
                current_disp.min = (next_disp.min + info.min_params).saturating_sub(max_produced);
                current_disp.max = (next_disp.max + info.max_params).saturating_sub(min_produced);

                /*
                /* when scanning from below like we do, we can updata the is_func field */

                match info.is_func {
                    Some(known_is_func) => {
                        is_proc = !known_is_func;
                        is_func = known_is_func;
                    }

                    None => match (is_proc, is_func) {
                        (true, false) => info.is_func = Some(false),
                        (false, true) => info.is_func = Some(true),
                        _ => {} /* we don't have new knowledge */
                    },
                }

                /* use callable params to update stack */

                let min_params = info.min_params;
                let max_params = info.max_params;

                /* functions produce one stack value, but procedures do not */

                current_disp.min = if is_proc {
                    next_disp.min + min_params
                } else {
                    (next_disp.min + min_params).max(1) - 1
                };

                current_disp.max = next_disp.max + max_params;
                */
            }

            ins => {
                let instruction_disp = instruction_stack_disp(ins);

                let imin = (next_disp.min as isize - instruction_disp).max(0);
                let imax = (next_disp.max as isize - instruction_disp).max(0);

                current_disp.min = imin as usize;
                current_disp.max = imax as usize;
            }
        }
    }
}

fn analyse_forwards(block: &mut BlockInfo, current_knowledge: &mut HashMap<CallId, CallableInfo>) {
    block.stack_disp[0] = StackDisp::zero();

    for i in 0..block.len() {
        let current_disp = block.stack_disp[i];
        let next_disp = &mut block.stack_disp[i + 1];

        match &block.instructions[i] {
            Ins::Call(call_id) => {
                /* get or insert entry */

                let info = current_knowledge
                    .entry(*call_id)
                    .or_insert_with(CallableInfo::default);

                update_stats_from_disps(info, current_disp, *next_disp);

                assert!(info.min_params <= info.max_params);

                /* knowing this is a function would raise output min by 1 */
                let (min_produced, max_produced) = match info.is_func {
                    Some(true) => (1, 1),
                    Some(false) => (0, 0),
                    None => (0, 1),
                };

                match info.is_func {
                    Some(false) => {
                        next_disp.min = 0;
                        next_disp.max = 0;
                    }

                    _ => {
                        /* we add produced and remove consumed */
                        next_disp.min =
                            current_disp.min.saturating_sub(info.max_params) + min_produced;
                        next_disp.max =
                            current_disp.max.saturating_sub(info.min_params) + max_produced;
                    }
                }

                /*

                let min_consumed = (current_disp.min + min_produced).saturating_sub(disp_next.max);
                let max_consumed = (current_disp.max + max_produced).saturating_sub(disp_next.min);

                assert!(min_consumed <= max_consumed);

                /* update param info */

                if info.max_params > max_consumed {
                    info.max_params = max_consumed;
                }

                if info.min_params < min_consumed {
                    info.min_params = min_consumed;
                }

                /* update disp_next */

                let next_min = current_disp.min.saturating_sub(max_consumed) + min_produced;
                let next_max = current_disp.max.saturating_sub(min_consumed) + max_produced;

                // assert!(disp_next.min <= next_min);
                // assert!(disp_next.max >= next_max);

                disp_next.min = next_min.max(disp_next.min);
                disp_next.max = next_max.min(disp_next.max);

                */
            }

            ins => {
                let instruction_disp = instruction_stack_disp(ins);

                next_disp.min = (current_disp.min as isize + instruction_disp) as usize;
                next_disp.max = (current_disp.max as isize + instruction_disp) as usize;
            }
        }
    }
}

fn analyse_callables(scripts: &[Script]) -> HashMap<CallId, CallableInfo> {
    let mut blocks = vec![];

    for i in 0..scripts.len() {
        blocks.extend(split_instructions(&scripts[i].instructions, i));
    }

    let mut current_knowledge = HashMap::new();

    /* TODO: better preloaded knowledge */

    /*

    // FOMT

    /* callable 0x105 is proc (possible SetTalkedTo) */
    current_knowledge.insert(CallId(0x105), CallableInfo::new_proc().with_params(2));

    /* callable 0x12F is proc */
    current_knowledge.insert(CallId(0x12F), CallableInfo::new_proc().with_params(0));

    /* callable 0x13B is func (possible GetFoodIcon) */
    current_knowledge.insert(CallId(0x13B), CallableInfo::new_func().with_params(1));

    // MFOMT

    /* callable 0x133 is proc */
    current_knowledge.insert(CallId(0x133), CallableInfo::new_proc().with_params(0));

    /* callable 0x13F is func (possible GetFoodIcon) */
    current_knowledge.insert(CallId(0x13F), CallableInfo::new_func().with_params(1));

    */

    /* each block should be "balanced", that is, it leaves the stack at the same
     * level as it entered at. This means we can analyse each block on their own
     * to infer callable parameter/result count */

    for _ in 0..50 {
        for i in 0..blocks.len() {
            let block = &mut blocks[i];
            analyse_backwards(block, &mut current_knowledge);
        }

        for i in 0..blocks.len() {
            let block = &mut blocks[i];
            analyse_forwards(block, &mut current_knowledge);
        }
    }

    current_knowledge
}

/*
fn display_block(block: &BlockInfo) {
    for i in 0..block.len() {
        let disp = block.stack_disp[i];
        let ins = block.instructions[i];
        println!("[{0}..{1}] {2:?}", disp.min, disp.max, ins);
    }
}
*/

#[derive(Debug, Error)]
enum MyError {
    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),

    #[error("Decode Error: {0}")]
    DecodeFailed(#[from] DecodeError),
}

#[derive(Parser)]
struct Args {
    /// Input file (ROM)
    input: PathBuf,
}

fn main() -> Result<(), MyError> {
    let args = Args::parse();
    let rom = fs::read(args.input)?;
    let script_data = rom_info::get_all_scripts(&rom)?;

    let scripts = {
        let mut scripts = vec![];

        for data in script_data {
            scripts.push(bytecode::decode_script(&mut &data[..])?);
        }

        scripts
    };

    let mut knowledge = analyse_callables(&scripts);

    let mut keys: Vec<&CallId> = knowledge.keys().collect();
    keys.sort();

    let max_call_id = keys.last().unwrap().0;

    for i in 0..=max_call_id {
        let call_id = CallId(i);
        let info = knowledge.entry(call_id).or_default();

        let (keyword, prefix) = match info.is_func {
            Some(true) => ("func", "Func"),
            Some(false) => ("proc", "Proc"),
            None => ("// ?", "Unk"),
        };

        let params: Vec<String> = (0..info.min_params)
            .map(|i| format!("arg_{0}", i + 1))
            .collect();
        let params = params.join(", ");

        let params = if info.min_params != info.max_params {
            format!("{params} /* ... potentially up to {0} */", info.max_params)
        } else {
            params
        };

        println!("{keyword} 0x{0:03X} {prefix}{0:03X}({params})", call_id.0);
    }

    Ok(())
}
