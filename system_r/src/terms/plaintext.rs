//! Convert a Term<TExtDialect> to system_r plaintext (srpt) format; needs to be
//! implemented for each TExtDialect
use anyhow::Result;

use crate::dialect::{SystemRDialect, SystemRExtension};

pub trait Plaintext<TExtDialect: SystemRDialect> {
    fn to_plaintext<TExt: SystemRExtension<TExtDialect>>(&self, ext: &TExt) -> Result<String>;

    fn handle_ext(&self) -> Result<String> {
        Err(anyhow!(
            "handle_ext should be overriden in implementors (except BottomDialect)"
        ))
    }
}
