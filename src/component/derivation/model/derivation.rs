use serde::{Deserialize, Serialize};

use super::item::DerivationItem;
use super::rule::DerivationRule;
use crate::parse::Exp;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Derivation {
    pub name: String,
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

    pub fn is_rule_valid(&self, item: &DerivationItem) -> bool {
        if let Some(rule) = &item.rule {
            let sentence_for_id = |k: &i32| self.item_for_id(*k).map(|i| i.sentence());
            let unordered_tuple_eq = |(a1, a2): (&Exp, &Exp), (b1, b2): (&Exp, &Exp)| {
                ((a1 == b1) && (a2 == b2)) || ((a1 == b2) && (a2 == b1))
            };
            match rule {
                DerivationRule::Premise => true,
                DerivationRule::AndIntro(Some(k), Some(l)) => {
                    match (sentence_for_id(k), sentence_for_id(l)) {
                        (Some(Ok(exp_k)), Some(Ok(exp_l))) => match item.sentence() {
                            Ok(Exp::And(lhs, rhs)) => (*lhs == exp_k) && (*rhs == exp_l),
                            Ok(_) | Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::AndIntro(_, _) => false,
                DerivationRule::AndExclude(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(Exp::And(exp_k_lhs, exp_k_rhs))) => match item.sentence() {
                        Ok(exp) => (exp == *exp_k_lhs) || (exp == *exp_k_rhs),
                        Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::AndExclude(_) => false,
                DerivationRule::OrIntro(Some(k), None) => match sentence_for_id(k) {
                    Some(Ok(exp_k)) => match item.sentence() {
                        Ok(Exp::Or(lhs, rhs)) => (exp_k == *lhs) || (exp_k == *rhs),
                        Ok(_) | Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::OrIntro(Some(k), Some(l)) => {
                    match (sentence_for_id(k), sentence_for_id(l)) {
                        (Some(Ok(exp_k)), Some(Ok(exp_l))) => match item.sentence() {
                            Ok(Exp::Or(lhs, rhs)) => {
                                unordered_tuple_eq((&exp_k, &exp_l), (&lhs, &rhs))
                            }
                            Ok(_) | Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::OrIntro(_, _) => false,
                DerivationRule::OrExclude(Some(k), (Some(l1), Some(m1)), (Some(l2), Some(m2))) => {
                    let item_l1 = self.item_for_id(*l1);
                    let item_l2 = self.item_for_id(*l2);
                    match (
                        sentence_for_id(k),
                        item_l1.map(|i| (i.sentence(), &i.rule)),
                        sentence_for_id(m1),
                        item_l2.map(|i| (i.sentence(), &i.rule)),
                        sentence_for_id(m2),
                    ) {
                        (
                            Some(Ok(Exp::Or(exp_k_lhs, exp_k_rhs))),
                            Some((Ok(exp_l1), Some(DerivationRule::Premise))),
                            Some(Ok(exp_m1)),
                            Some((Ok(exp_l2), Some(DerivationRule::Premise))),
                            Some(Ok(exp_m2)),
                        ) => match item.sentence() {
                            Ok(exp) => {
                                unordered_tuple_eq((&exp_k_lhs, &exp_k_rhs), (&exp_l1, &exp_l2))
                                    && (exp == exp_m1)
                                    && (exp == exp_m2)
                            }
                            Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::OrExclude(_, _, _) => false,
                DerivationRule::IfIntro((Some(k), Some(l))) => {
                    let item_k = self.item_for_id(*k);
                    match (item_k.map(|i| (i.sentence(), &i.rule)), sentence_for_id(l)) {
                        (Some((Ok(exp_k), Some(DerivationRule::Premise))), Some(Ok(exp_l))) => {
                            match item.sentence() {
                                Ok(Exp::Cond(lhs, rhs)) => (exp_k == *lhs) && (exp_l == *rhs),
                                Ok(_) | Err(_) => false,
                            }
                        }
                        _ => false,
                    }
                }
                DerivationRule::IfIntro(_) => false,
                DerivationRule::IfExclude(Some(k), Some(l)) => {
                    match (sentence_for_id(k), sentence_for_id(l), item.sentence()) {
                        (Some(Ok(exp_k)), Some(Ok(exp_l)), Ok(Exp::Falsum))
                            if (exp_k.negated() == exp_l) || (exp_k == exp_l.negated()) =>
                        {
                            true
                        }
                        (Some(Ok(Exp::Cond(exp_k_lhs, exp_k_rhs))), Some(Ok(exp_l)), Ok(exp)) => {
                            (*exp_k_lhs == exp_l) && (*exp_k_rhs == exp)
                        }
                        (_, _, Err(_)) => false,
                        _ => false,
                    }
                }
                DerivationRule::IfExclude(_, _) => false,
                DerivationRule::IffIntro(Some(k), Some(l)) => {
                    match (sentence_for_id(k), sentence_for_id(l)) {
                        (
                            Some(Ok(Exp::Cond(exp_k_lhs, exp_k_rhs))),
                            Some(Ok(Exp::Cond(exp_l_lhs, exp_l_rhs))),
                        ) => match item.sentence() {
                            Ok(Exp::Iff(lhs, rhs)) => {
                                unordered_tuple_eq((&lhs, &rhs), (&exp_k_lhs, &exp_k_rhs))
                                    && (*exp_k_lhs == *exp_l_rhs)
                                    && (*exp_k_rhs == *exp_l_lhs)
                            }
                            Ok(_) | Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::IffIntro(_, _) => false,
                DerivationRule::IffExclude(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(Exp::Iff(exp_k_lhs, exp_k_rhs))) => match item.sentence() {
                        Ok(Exp::Cond(lhs, rhs)) => {
                            unordered_tuple_eq((&lhs, &rhs), (&exp_k_lhs, &exp_k_rhs))
                        }
                        Ok(_) | Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::IffExclude(_) => false,
                DerivationRule::Falsum(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(Exp::Falsum)) => true,
                    _ => false,
                },
                DerivationRule::Falsum(_) => false,
                DerivationRule::NegIntro((Some(k), Some(l))) => {
                    match (sentence_for_id(k), sentence_for_id(l)) {
                        (Some(Ok(exp_k)), Some(Ok(Exp::Falsum))) => match item.sentence() {
                            Ok(Exp::Neg(negated)) => *negated == exp_k,
                            Ok(_) | Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::NegIntro(_) => false,
                DerivationRule::NegExclude((Some(k), Some(l))) => {
                    match (sentence_for_id(k), sentence_for_id(l)) {
                        (Some(Ok(Exp::Neg(exp_k_negated))), Some(Ok(Exp::Falsum))) => {
                            match item.sentence() {
                                Ok(exp) => exp == *exp_k_negated,
                                Err(_) => false,
                            }
                        }
                        _ => false,
                    }
                }
                DerivationRule::NegExclude(_) => false,
                DerivationRule::UnivQuntIntro(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(exp_k)) => match item.sentence() {
                        Ok(Exp::UnivGenr(var, inner)) => {
                            inner.var_replaced(&var, "<UNKNOWN>") == exp_k
                        }
                        Ok(_) | Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::UnivQuntIntro(_) => false,
                DerivationRule::UnivQuntExclude(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(Exp::UnivGenr(exp_k_var, exp_k_inner))) => match item.sentence() {
                        Ok(exp) => exp_k_inner.var_replaced(&exp_k_var, "<UNKNOWN>") == exp,
                        Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::UnivQuntExclude(_) => false,
                DerivationRule::ExisQuntIntro(Some(k)) => match sentence_for_id(k) {
                    Some(Ok(exp_k)) => match item.sentence() {
                        Ok(Exp::ExistGenr(var, inner)) => {
                            inner.var_replaced(&var, "<UNKNOWN>") == exp_k
                        }
                        Ok(_) | Err(_) => false,
                    },
                    _ => false,
                },
                DerivationRule::ExisQuntIntro(_) => false,
                DerivationRule::ExisQuntExclude(Some(k), (Some(l), Some(m))) => {
                    match (sentence_for_id(k), sentence_for_id(l), sentence_for_id(m)) {
                        (
                            Some(Ok(Exp::ExistGenr(exp_k_var, exp_k_inner))),
                            Some(Ok(exp_l)),
                            Some(Ok(exp_m)),
                        ) => match item.sentence() {
                            Ok(exp) => {
                                (exp_k_inner.var_replaced(&exp_k_var, "<UNKNOWN>") == exp_l)
                                    && (exp_m == exp)
                            }
                            Err(_) => false,
                        },
                        _ => false,
                    }
                }
                DerivationRule::ExisQuntExclude(_, _) => false,
            }
        } else {
            false
        }
    }
}
