use serde::{Deserialize, Serialize};

use super::item::DerivationItem;
use super::rule::DerivationRule;

#[derive(Debug, Serialize, Deserialize)]
pub struct Derivation {
    pub items: Vec<DerivationItem>,
}

impl Derivation {
    pub fn item_for_id(&self, id: i32) -> Option<&DerivationItem> {
        self.items.iter().find(|e| e.id == id)
    }

    pub fn item_for_id_mut(&mut self, id: i32) -> Option<&mut DerivationItem> {
        self.items.iter_mut().find(|e| e.id == id)
    }

    pub fn index_for_item(&self, id: i32) -> usize {
        self.items.iter().position(|r| r.id == id).unwrap()
    }

    pub fn get_premises_for_item<'a>(&self, item: &'a DerivationItem) -> Vec<&'a DerivationItem> {
        match item.rule {
            Some(DerivationRule::Premise) => vec![item],
            Some(DerivationRule::AndIntro(k, l)) => vec![],
            Some(DerivationRule::AndExclude(k)) => vec![],
            Some(DerivationRule::OrIntro(k, l)) => vec![],
            Some(DerivationRule::OrExclude(k, (l1, m1), (l2, m2))) => vec![],
            Some(DerivationRule::IfIntro((k, l))) => vec![],
            Some(DerivationRule::IfExclude(k, l)) => vec![],
            Some(DerivationRule::IffIntro(k, l)) => vec![],
            Some(DerivationRule::IffExclude(k)) => vec![],
            Some(DerivationRule::NegIntro((k, l))) => vec![],
            Some(DerivationRule::NegExclude((k, l))) => vec![],
            Some(DerivationRule::UnivQuntIntro(k)) => vec![],
            Some(DerivationRule::UnivQuntExclude(k)) => vec![],
            Some(DerivationRule::ExisQuntIntro(k)) => vec![],
            Some(DerivationRule::ExisQuntExclude(k, (l, m))) => vec![],
            None => vec![],
        }
    }
}
