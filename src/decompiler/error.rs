use std::{error, fmt};

use thiserror::Error;

use crate::ir::CallId;

use super::state::DecompileState;

#[derive(Debug, Error)]
pub enum DecompileError {
    #[error("Undefined callable id {0:?}")]
    UnknownCallableID(CallId),

    #[error("Callable id {0:?} expects too many parameters (at most {1} available)")]
    CallableExpectsTooManyParameters(CallId, usize),

    #[error("Couldn't reduce")]
    CouldntReduce,

    #[error("Unexpected lookahead")]
    UnexpectedLookahead,

    #[error("Couldn't reduce at end of input")]
    CouldntReduceAtEnd,

    #[error("Couldn't map all strings")]
    NotEnoughStringConsumers,
}

#[derive(Debug)]
pub struct DecompileErrorExtra<'a>(pub(super) DecompileError, pub(super) DecompileState<'a>);

impl<'a> DecompileErrorExtra<'a> {
    pub fn state_at_error(&self) -> &DecompileState<'a> {
        &self.1
    }
}

impl<'a> fmt::Display for DecompileErrorExtra<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> error::Error for DecompileErrorExtra<'a> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.0)
    }
}

impl<'a> From<DecompileErrorExtra<'a>> for DecompileError {
    fn from(value: DecompileErrorExtra<'a>) -> Self {
        value.0
    }
}
