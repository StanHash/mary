use std::{
    fs::{self, File},
    io::{self, stdout, BufWriter, Write},
    path::PathBuf,
};

use clap::Parser;
use ir::{IntValue, Script};
use lexgen_util::{LexerErrorKind, Loc};
use parser::ScriptError;
use thiserror::Error;

mod ast;
mod bytecode;
mod compiler;
mod ir;
mod lexer;
mod parser;

#[derive(Debug, Error)]
enum Error {
    #[error("Script Error: {0}")]
    ScriptError(#[from] ScriptError),

    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),

    #[error("Lexer Error")]
    LexError(Loc),

    #[error("CLI Error: {0}")]
    CliError(&'static str),
}

#[derive(Parser)]
struct Args {
    /// Input file (script to compile)
    input: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Print resulting IR as comment in output
    #[arg(long)]
    debug_ir: bool,

    /// Output as binary rather than C (limited to one script)
    #[arg(long)]
    binary: bool,
}

fn print_c_scripts<W: io::Write>(
    w: &mut W,
    scripts: Vec<(IntValue, String, Script)>,
    pretty_bytecode: bool,
) -> io::Result<()> {
    use crate::bytecode::encode_script;

    for (_, name, script) in scripts {
        let bytecode = encode_script(&script);

        // print bytecode as C

        if pretty_bytecode {
            // not so pretty (or bytecode) just yet, just debug IR
            writeln!(w, "/*")?;

            writeln!(w, "{:?}", script)?;

            writeln!(w, "*/")?;
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

fn main_error() -> Result<(), Error> {
    use bytecode::encode_script;
    use lexer::Lexer;
    use parser::Parser;

    let args = Args::parse();
    let code = fs::read_to_string(args.input)?;

    // TODO: decompiler
    // TODO: locations
    // TODO: func params and invoke args coherence

    let l = Lexer::new(&code);
    let mut p = Parser::new(parser::ParseContext::new());

    for tok in l {
        match tok {
            Ok((_, tok, _)) => match p.parse(tok) {
                Ok(()) => {}
                Err(err) => return Err(err.into()),
            },

            Err(err) => match err.kind {
                LexerErrorKind::InvalidToken => return Err(Error::LexError(err.location)),
                LexerErrorKind::Custom(_) => unimplemented!(),
            },
        }
    }

    match p.end_of_input() {
        Ok((_, parse_ctx)) => {
            if args.binary {
                if parse_ctx.scripts.len() != 1 {
                    Err(Error::CliError(
                        "In binary output mode, only one script can be defined",
                    ))
                } else {
                    let script = &parse_ctx.scripts[0].2;
                    let bytecode = encode_script(script);

                    match args.output {
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
                if let Some(path) = args.output {
                    let output = File::create(path)?;
                    let mut buf_write = BufWriter::new(output);
                    print_c_scripts(&mut buf_write, parse_ctx.scripts, args.debug_ir)?;
                    buf_write.flush()?;
                } else {
                    print_c_scripts(&mut stdout(), parse_ctx.scripts, args.debug_ir)?;
                }

                Ok(())
            }
        }

        Err(err) => Err(err.into()),
    }
}

fn main() -> Result<(), ()> {
    // TODO: is this good error reporting?

    // you'd thing using Result as return val for main would make this for you
    // but it seems to print result through the Debug trait rather than Display?

    match main_error() {
        Ok(_) => Ok(()),

        Err(err) => {
            writeln!(io::stderr(), "{0}", err).unwrap();
            Err(())
        }
    }
}

// weird TODOs:
// TODO: prevent default case at offset 0 (due to a bug in the interpreter, this won't work)
