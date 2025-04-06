use std::fmt;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Cannot redeclare '{0}' in this scope")]
    NameAlreadyDeclared(String),

    #[error("Cannot assign value to '{0}' (not a variable)")]
    CannotAssignToNonVar(String),

    #[error("Cannot evaluate {0} '{1}' as a value")]
    CannotEvaluateCallable(&'static str, String),

    #[error("Failed to evaluate constant expression")]
    FailedConstantEvaluation,

    #[error("Expected constant integer but got string instead")]
    ExpectedConstantIntGotStr,

    #[error("Name '{0}' is not declared in this scope")]
    NameNotDeclared(String),

    #[error("Cannot call '{0}': only functions and procedures can be called")]
    NotCallable(String),

    #[error("Cannot evaluate call to '{0}': procedures do not yield results")]
    ProcNotFunction(String),

    #[error("Multiple 'default' blocks in switch")]
    MultipleDefaults,
}

#[derive(Debug, Error)]
pub struct CompileErrors(pub Vec<CompileError>);

impl fmt::Display for CompileErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for e in &self.0 {
            write!(f, "{}", e)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ScriptError {
    #[error("Compile errors while compiling {0}: {1}")]
    CompileErrors(String, CompileErrors),

    // TODO: this should be better!!!
    #[error("Lexer Error")]
    LexError(lexgen_util::Loc),

    #[error("Syntax error")]
    SyntaxError,

    #[error("Fatal syntax error")]
    ParseFail,

    #[error("Fatal parse error: stack overflow")]
    ParseStackOverflow,

    #[error("Failed to evaluate constant expression")]
    FailedConstantEvaluation,

    #[error("Toplevel declarations must be before script blocks")]
    DeclarationsAfterScript,

    #[error("Included files cannot define script blocks")]
    ScriptInIncludedFiles,

    #[error("{0} ids cannot be strings")]
    StringId(&'static str),
}
