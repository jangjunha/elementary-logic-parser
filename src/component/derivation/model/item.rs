use serde::{Deserialize, Serialize};

use super::rule::DerivationRule;
use crate::parse::expression::exp;

#[derive(Debug, Serialize, Deserialize)]
pub struct DerivationItem {
    pub id: i32,
    pub sentence_text: String,
    pub rule: Option<DerivationRule>,
}

impl DerivationItem {
    pub fn is_valid_sentence(&self) -> bool {
        let parsed = exp(&self.sentence_text);
        match parsed {
            Result::Ok(("", _)) => true,
            Result::Ok((_, _)) => false,
            Result::Err(_) => false,
        }
    }
}
