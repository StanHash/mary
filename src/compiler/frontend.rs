use crate::compiler::{ParseContext, ScriptError};

pub fn parse_string(code_string: &str) -> Result<ParseContext, ScriptError> {
    use lexgen_util::LexerErrorKind;

    use crate::compiler::Lexer;
    use crate::compiler::Parser;

    let l = Lexer::new(code_string);
    let mut p = Parser::new(ParseContext::new());

    for tok in l {
        match tok {
            Ok((_, tok, _)) => match p.parse(tok) {
                Ok(()) => {}
                Err(err) => return Err(err.into()),
            },

            Err(err) => match err.kind {
                LexerErrorKind::InvalidToken => return Err(ScriptError::LexError(err.location)),
                LexerErrorKind::Custom(_) => unimplemented!(),
            },
        }
    }

    match p.end_of_input() {
        Ok((_, parse_ctx)) => Ok(parse_ctx),
        Err(err) => Err(err.into()),
    }
}
