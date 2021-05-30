use serde::{Deserialize, Serialize};

use super::rule::DerivationRule;
use crate::parse::{exp as parse_exp, Exp};

// TODO: Serialize 시 sentence_text 그대로 넣지 말고 ``create::parse::model::to_string(sentence())`` 쓰기
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct DerivationItem {
    pub id: i32,
    pub sentence_text: String,
    pub rule: Option<DerivationRule>,
}

impl DerivationItem {
    pub fn sentence(&self) -> Result<Exp, ()> {
        let parsed = parse_exp(&self.sentence_text);
        match parsed {
            Result::Ok(("", exp)) => Ok(exp),
            Result::Ok((_, _)) => Err(()),
            Result::Err(_) => Err(()),
        }
    }

    pub fn is_valid_sentence(&self) -> bool {
        self.sentence().is_ok()
    }
}
