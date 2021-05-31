use serde::ser::{Serialize, SerializeStruct, Serializer};
use serde::Deserialize;

use super::rule::DerivationRule;
use crate::parse::model::to_string as exp_to_string;
use crate::parse::{exp as parse_exp, Exp};

#[derive(Debug, Clone, Deserialize, Hash, Eq, PartialEq)]
pub struct DerivationItem {
    pub id: i32,
    pub sentence_text: String,
    pub rule: Option<DerivationRule>,
}

impl Serialize for DerivationItem {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("DerivationItem", 3)?;
        state.serialize_field("id", &self.id)?;
        if let Ok(exp) = self.sentence() {
            state.serialize_field("sentence_text", &exp_to_string(&exp))?;
        } else {
            state.skip_field("sentence_text")?;
        }
        state.serialize_field("rule", &self.rule)?;
        state.end()
    }
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
