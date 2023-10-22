use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

use clap::Parser;
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
}

#[derive(Parser)]
struct Args {
    input: PathBuf,
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
            for (_, name, script) in parse_ctx.scripts {
                let bytecode = encode_script(&script);

                // print bytecode as very basic EA script for now

                println!("ALIGN 4");
                println!("{name}:");

                println!("    // length: {0} bytes", bytecode.len());
                print!("    BYTE");

                for byte in bytecode {
                    print!(" 0x{:02X}", byte);
                }

                println!();
            }

            Ok(())
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
