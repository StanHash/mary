use std::{
    fs::{self, File},
    io::{self, stdin, stdout, BufWriter, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use thiserror::Error;

use mary::{
    ast::Stmt,
    bytecode::{self, DecodeError},
    compiler::ScriptError,
    decompiler::DecompileError,
    ir::{IntValue, Script},
    pretty_print::{PrettyStmts, PrettyStringLit},
};

#[derive(Debug, Error)]
enum Error {
    #[error("Script Error: {0}")]
    ScriptError(#[from] ScriptError),

    #[error("Lib script error: {0}")]
    LibScriptError(ScriptError),

    #[error("Decode error: {0}")]
    DecodeFailed(#[from] DecodeError),

    #[error("Decompile error: {0}")]
    DecompileFailed(#[from] DecompileError),

    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),

    #[error("CLI Error: {0}")]
    CliError(&'static str),
}

#[derive(Subcommand)]
enum Command {
    Compile {
        /// Input script to compile (default: stdin)
        input: Option<PathBuf>,

        /// Output file (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Output as binary rather than C (limited to one script)
        #[arg(long)]
        binary: bool,

        /// Print compiled IR as comment in output
        #[arg(long)]
        print_ir: bool,
    },

    Decompile {
        /// Input binary
        input_binary: PathBuf,

        /// Input library script
        input_library: PathBuf,

        /// Output file (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Script ID (do not use with --offset!)
        #[arg(long)]
        script_id: Option<usize>,

        /// Script offset in binary (do not use with --script-id!)
        #[arg(long)]
        offset: Option<usize>,

        /// Print decoded IR as comment in output
        #[arg(long)]
        print_ir: bool,
    },
}

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn print_compiled_scripts_in_c<W: io::Write>(
    w: &mut W,
    scripts: Vec<(IntValue, String, Script)>,
    pretty_bytecode: bool,
) -> io::Result<()> {
    use mary::bytecode::encode_script;

    for (_, name, script) in &scripts {
        let bytecode = encode_script(script);

        // print bytecode as C

        if pretty_bytecode {
            write_ir_as_comment(w, script)?;
            writeln!(w)?;
        }

        let bytes = bytecode.len();

        writeln!(w, "// {name}: length of {bytes} bytes (+ padding)")?;
        write!(w, "unsigned int const {name}[] = {{")?;

        for i in 0..(bytecode.len() + 3) / 4 {
            let val = u32::from_le_bytes([
                bytecode[i * 4],
                *bytecode.get(i * 4 + 1).unwrap_or(&0),
                *bytecode.get(i * 4 + 2).unwrap_or(&0),
                *bytecode.get(i * 4 + 3).unwrap_or(&0),
            ]);

            if i % 8 == 0 {
                write!(w, "\n   ")?;
            }

            write!(w, " 0x{val:08X},")?;
        }

        writeln!(w, "\n}};")?;
    }

    Ok(())
}

fn write_ir_as_comment<W: io::Write>(w: &mut W, script: &Script) -> Result<(), io::Error> {
    use mary::ir::Ins;

    writeln!(w, "/*")?;

    writeln!(w, "Script instructions (IR):")?;

    for instruction in &script.instructions {
        match instruction {
            Ins::Label(_) | Ins::Case(_, _) => {
                writeln!(w, "  {0:?}", instruction)?;
            }

            _ => {
                writeln!(w, "    {0:?}", instruction)?;
            }
        }
    }

    writeln!(w)?;
    writeln!(w, "Script strings (IR):")?;

    for i in 0..script.strings.len() {
        let pretty_string_lit = PrettyStringLit::new(&script.strings[i]);
        writeln!(w, "    {i}: {pretty_string_lit}")?;
    }

    writeln!(w, "*/")
}

fn main_error() -> Result<(), Error> {
    let args = Args::parse();

    match args.command {
        Command::Compile {
            input,
            output,
            print_ir,
            binary,
        } => {
            use mary::bytecode::encode_script;
            use mary::compiler::parse_string;

            let code = match input {
                Some(path) => fs::read_to_string(path)?,
                None => io::read_to_string(stdin().lock())?,
            };

            // TODO: locations
            // TODO: func params and invoke args coherence

            let parse_result = parse_string(&code)?;

            if binary {
                if parse_result.scripts.len() != 1 {
                    Err(Error::CliError(
                        "In binary output mode, only and exactly one (1) script can be defined",
                    ))
                } else {
                    let script = &parse_result.scripts[0].2;
                    let bytecode = encode_script(script);

                    match output {
                        Some(out_path) => {
                            fs::write(out_path, bytecode)?;
                        }

                        None => {
                            stdout().write(&bytecode)?;
                        }
                    }

                    Ok(())
                }
            } else {
                if let Some(path) = output {
                    let output = File::create(path)?;
                    let mut buf_write = BufWriter::new(output);
                    print_compiled_scripts_in_c(&mut buf_write, parse_result.scripts, print_ir)?;
                    buf_write.flush()?;
                } else {
                    print_compiled_scripts_in_c(
                        &mut stdout().lock(),
                        parse_result.scripts,
                        print_ir,
                    )?;
                }

                Ok(())
            }
        }

        Command::Decompile {
            input_binary,
            input_library,
            output,
            script_id,
            offset,
            print_ir,
        } => {
            use mary::{compiler::parse_string, decompiler::decompile_script, utility::rom_info};

            let rom = fs::read(input_binary)?;

            let event_script_id;
            let event_script_name;

            let script = match (script_id, offset) {
                (Some(script_id), None) => {
                    let script_data = rom_info::get_all_scripts(&rom)?;

                    event_script_id = script_id;
                    event_script_name = format!("EventScript_{script_id}");

                    bytecode::decode_script(&mut &script_data[script_id - 1][..])?
                }

                (None, Some(offset)) => {
                    let offset = offset & 0x01FFFFFF;

                    event_script_id = 0;
                    event_script_name = format!("EventScript_{0:08X}", offset + 0x08000000);

                    /* TODO: verify this is a well-formed RIFF */
                    bytecode::decode_script(&mut &rom[offset..])?
                }

                (None, None) => {
                    event_script_id = 0;
                    event_script_name = format!("YourEventScriptNameHere");

                    bytecode::decode_script(&mut &rom[..])?
                }

                _ => {
                    return Err(Error::CliError(
                        "Expected at most one (1) of script ID or offset",
                    ))
                }
            };

            let library_code = fs::read_to_string(&input_library)?;

            let library_scope = match parse_string(&library_code) {
                Ok(parse_result) => parse_result.const_scope,
                Err(err) => return Err(Error::LibScriptError(err)),
            };

            let stmts = match decompile_script(&script, &library_scope) {
                Ok(stmts) => stmts,

                Err(error) => {
                    eprintln!("{0}", error.state_at_error());
                    return Err(Error::DecompileFailed(DecompileError::from(error)));
                }
            };

            let library_path_for_include = input_library.to_string_lossy();

            match output {
                Some(path) => {
                    // print to file

                    let output = File::create(path)?;
                    let mut buf_write = BufWriter::new(output);

                    print_decompiled_script(
                        &mut buf_write,
                        event_script_id,
                        event_script_name,
                        stmts,
                        &library_path_for_include,
                        print_ir.then(|| &script),
                    )?;

                    buf_write.flush()?;
                    Ok(())
                }

                None => {
                    // print to stdout

                    print_decompiled_script(
                        &mut stdout().lock(),
                        event_script_id,
                        event_script_name,
                        stmts,
                        &library_path_for_include,
                        print_ir.then(|| &script),
                    )?;

                    Ok(())
                }
            }
        }
    }
}

fn print_decompiled_script<W: io::Write>(
    w: &mut W,
    script_id: usize,
    script_name: String,
    stmts: Vec<Stmt>,
    library_path_for_include: &str,
    print_ir: Option<&Script>,
) -> io::Result<()> {
    writeln!(w, "#include \"{0}\"", library_path_for_include)?;
    writeln!(w)?;

    writeln!(w, "script {0} {1}", script_id, script_name)?;

    writeln!(w, "{{")?;

    let pretty_stmts = PrettyStmts::with_indent(&stmts, 1);
    writeln!(w, "{pretty_stmts}")?;

    writeln!(w, "}}")?;

    if let Some(script) = print_ir {
        writeln!(w)?;
        write_ir_as_comment(w, script)?;
    }

    Ok(())
}

fn main() -> Result<(), ()> {
    // TODO: is this good error reporting?

    // you'd think using Result as return val for main would make this for you
    // but it seems to print result through the Debug trait rather than Display?

    match main_error() {
        Ok(_) => Ok(()),

        Err(err) => {
            eprintln!("{0}", err);
            Err(())
        }
    }
}
