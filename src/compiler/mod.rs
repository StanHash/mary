mod allocator;
mod emitter;
pub mod error;
mod frontend;
mod lexer;
mod parser;

pub use lexer::Lexer;

pub use parser::ParseContext;
pub use parser::Parser;

pub use emitter::compile_script;

pub use error::ScriptError;

pub use frontend::parse_string;
