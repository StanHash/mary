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

                // print bytecode as C

                println!("// {name}: length of {0} bytes (+ padding)", bytecode.len());
                print!("unsigned int const {name}[] = {{");

                for i in 0..(bytecode.len() + 3) / 4 {
                    let val = u32::from_le_bytes([
                        bytecode[i * 4],
                        *bytecode.get(i * 4 + 1).unwrap_or(&0),
                        *bytecode.get(i * 4 + 2).unwrap_or(&0),
                        *bytecode.get(i * 4 + 3).unwrap_or(&0),
                    ]);

                    if i % 8 == 0 {
                        print!("\n   ");
                    }

                    print!(" 0x{val:08X},");
                }

                println!("\n}};");
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
