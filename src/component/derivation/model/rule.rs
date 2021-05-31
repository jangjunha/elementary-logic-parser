use std::collections::BTreeMap;
use std::fmt;

use serde::de::{self, Visitor};
use serde::ser;
use serde::{Deserialize, Deserializer, Serialize};

use crate::parse::derivation_rule::{self, derivation_rule as parse_derivation_rule};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum DerivationRule {
    Premise,
    AndIntro(Option<i32>, Option<i32>),
    AndExclude(Option<i32>),
    OrIntro(Option<i32>, Option<i32>),
    OrExclude(
        Option<i32>,
        (Option<i32>, Option<i32>),
        (Option<i32>, Option<i32>),
    ),
    IfIntro((Option<i32>, Option<i32>)),
    IfExclude(Option<i32>, Option<i32>),
    IffIntro(Option<i32>, Option<i32>),
    IffExclude(Option<i32>),
    Falsum(Option<i32>),
    NegIntro((Option<i32>, Option<i32>)),
    NegExclude((Option<i32>, Option<i32>)),
    UnivQuntIntro(Option<i32>),
    UnivQuntExclude(Option<i32>),
    ExisQuntIntro(Option<i32>),
    ExisQuntExclude(Option<i32>, (Option<i32>, Option<i32>)),
}

const NAME_PREMISE: &str = "premise (P)";
const NAME_AND_INTRO: &str = "and intro (&I)";
const NAME_AND_EXCLUDE: &str = "and exclude (&E)";
const NAME_OR_INTRO: &str = "or intro (∨I)";
const NAME_OR_EXCLUDE: &str = "or exclude (∨E)";
const NAME_IF_INTRO: &str = "if intro (→I)";
const NAME_IF_EXCLUDE: &str = "if exclude (→E)";
const NAME_IFF_INTRO: &str = "iff intro (↔I)";
const NAME_IFF_EXCLUDE: &str = "iff exclude (↔E)";
const NAME_FALSUM: &str = "falsum (⊥)";
const NAME_NEG_INTRO: &str = "neg intro (-I)";
const NAME_NEG_EXCLUDE: &str = "neg exclude (-E)";
const NAME_UNIV_QUNT_INTRO: &str = "univ qunt intro (()I)";
const NAME_UNIV_QUNT_EXCLUDE: &str = "univ qunt exclude (()E)";
const NAME_EXIS_QUNT_INTRO: &str = "exis qunt intro (∃I)";
const NAME_EXIS_QUNT_EXCLUDE: &str = "exis qunt exclude (∃E)";

impl DerivationRule {
    pub fn all_names() -> &'static [&'static str] {
        &[
            NAME_PREMISE,
            NAME_AND_INTRO,
            NAME_AND_EXCLUDE,
            NAME_OR_INTRO,
            NAME_OR_EXCLUDE,
            NAME_IF_INTRO,
            NAME_IF_EXCLUDE,
            NAME_IFF_INTRO,
            NAME_IFF_EXCLUDE,
            NAME_FALSUM,
            NAME_NEG_INTRO,
            NAME_NEG_EXCLUDE,
            NAME_UNIV_QUNT_INTRO,
            NAME_UNIV_QUNT_EXCLUDE,
            NAME_EXIS_QUNT_INTRO,
            NAME_EXIS_QUNT_EXCLUDE,
        ]
    }

    pub fn name(&self) -> &str {
        match self {
            Self::Premise => NAME_PREMISE,
            Self::AndIntro(_, _) => NAME_AND_INTRO,
            Self::AndExclude(_) => NAME_AND_EXCLUDE,
            Self::OrIntro(_, _) => NAME_OR_INTRO,
            Self::OrExclude(_, _, _) => NAME_OR_EXCLUDE,
            Self::IfIntro(_) => NAME_IF_INTRO,
            Self::IfExclude(_, _) => NAME_IF_EXCLUDE,
            Self::IffIntro(_, _) => NAME_IFF_INTRO,
            Self::IffExclude(_) => NAME_IFF_EXCLUDE,
            Self::Falsum(_) => NAME_FALSUM,
            Self::NegIntro(_) => NAME_NEG_INTRO,
            Self::NegExclude(_) => NAME_NEG_EXCLUDE,
            Self::UnivQuntIntro(_) => NAME_UNIV_QUNT_INTRO,
            Self::UnivQuntExclude(_) => NAME_UNIV_QUNT_EXCLUDE,
            Self::ExisQuntIntro(_) => NAME_EXIS_QUNT_INTRO,
            Self::ExisQuntExclude(_, _) => NAME_EXIS_QUNT_EXCLUDE,
        }
    }

    pub fn from_name(name: &str) -> Result<Self, ()> {
        match name {
            NAME_PREMISE => Ok(Self::Premise),
            NAME_AND_INTRO => Ok(Self::AndIntro(None, None)),
            NAME_AND_EXCLUDE => Ok(Self::AndExclude(None)),
            NAME_OR_INTRO => Ok(Self::OrIntro(None, None)),
            NAME_OR_EXCLUDE => Ok(Self::OrExclude(None, (None, None), (None, None))),
            NAME_IF_INTRO => Ok(Self::IfIntro((None, None))),
            NAME_IF_EXCLUDE => Ok(Self::IfExclude(None, None)),
            NAME_IFF_INTRO => Ok(Self::IffIntro(None, None)),
            NAME_IFF_EXCLUDE => Ok(Self::IffExclude(None)),
            NAME_FALSUM => Ok(Self::Falsum(None)),
            NAME_NEG_INTRO => Ok(Self::NegIntro((None, None))),
            NAME_NEG_EXCLUDE => Ok(Self::NegExclude((None, None))),
            NAME_UNIV_QUNT_INTRO => Ok(Self::UnivQuntIntro(None)),
            NAME_UNIV_QUNT_EXCLUDE => Ok(Self::UnivQuntExclude(None)),
            NAME_EXIS_QUNT_INTRO => Ok(Self::ExisQuntIntro(None)),
            NAME_EXIS_QUNT_EXCLUDE => Ok(Self::ExisQuntExclude(None, (None, None))),
            _ => Err(()),
        }
    }

    // Filling deps and return it with is_completed.
    pub fn filling_next(&self, next_id: i32) -> (Self, bool) {
        let next = Some(next_id);
        match self {
            Self::Premise => (Self::Premise, true),
            Self::AndIntro(None, l) => (Self::AndIntro(next, *l), false),
            Self::AndIntro(k, None) => (Self::AndIntro(*k, next), true),
            Self::AndIntro(Some(_), Some(_)) => (Self::AndIntro(next, None), false),
            Self::AndExclude(_) => (Self::AndExclude(next), true),
            Self::OrIntro(None, l) => (Self::OrIntro(next, *l), false),
            Self::OrIntro(k, None) => (Self::OrIntro(*k, next), true),
            Self::OrIntro(_, _) => (Self::OrIntro(next, None), false),
            Self::OrExclude(None, (l1, m1), (l2, m2)) => {
                (Self::OrExclude(next, (*l1, *m1), (*l2, *m2)), false)
            }
            Self::OrExclude(k, (None, m1), (l2, m2)) => {
                (Self::OrExclude(*k, (next, *m1), (*l2, *m2)), false)
            }
            Self::OrExclude(k, (l1, None), (l2, m2)) => {
                (Self::OrExclude(*k, (*l1, next), (*l2, *m2)), false)
            }
            Self::OrExclude(k, (l1, m1), (None, m2)) => {
                (Self::OrExclude(*k, (*l1, *m1), (next, *m2)), false)
            }
            Self::OrExclude(k, (l1, m1), (l2, None)) => {
                (Self::OrExclude(*k, (*l1, *m1), (*l2, next)), true)
            }
            Self::OrExclude(Some(_), (Some(_), Some(_)), (Some(_), Some(_))) => {
                (Self::OrExclude(next, (None, None), (None, None)), false)
            }
            Self::IfIntro((k, None)) => (Self::IfIntro((*k, next)), false),
            Self::IfIntro((None, l)) => (Self::IfIntro((next, *l)), true),
            Self::IfIntro((Some(_), Some(_))) => (Self::IfIntro((None, next)), false),
            Self::IfExclude(None, l) => (Self::IfExclude(next, *l), false),
            Self::IfExclude(k, None) => (Self::IfExclude(*k, next), true),
            Self::IfExclude(Some(_), Some(_)) => (Self::IfExclude(next, None), false),
            Self::IffIntro(None, l) => (Self::IffIntro(next, *l), false),
            Self::IffIntro(k, None) => (Self::IffIntro(*k, next), true),
            Self::IffIntro(Some(_), Some(_)) => (Self::IffIntro(next, None), false),
            Self::IffExclude(_) => (Self::IffExclude(next), true),
            Self::Falsum(_) => (Self::Falsum(next), true),
            Self::NegIntro((None, l)) => (Self::NegIntro((next, *l)), false),
            Self::NegIntro((k, None)) => (Self::NegIntro((*k, next)), true),
            Self::NegIntro((Some(_), Some(_))) => (Self::NegIntro((next, None)), false),
            Self::NegExclude((None, l)) => (Self::NegExclude((next, *l)), false),
            Self::NegExclude((k, None)) => (Self::NegExclude((*k, next)), true),
            Self::NegExclude((Some(_), Some(_))) => (Self::NegExclude((next, None)), false),
            Self::UnivQuntIntro(_) => (Self::UnivQuntIntro(next), true),
            Self::UnivQuntExclude(_) => (Self::UnivQuntExclude(next), true),
            Self::ExisQuntIntro(_) => (Self::ExisQuntIntro(next), true),
            Self::ExisQuntExclude(None, (l, m)) => (Self::ExisQuntExclude(next, (*l, *m)), false),
            Self::ExisQuntExclude(k, (None, m)) => (Self::ExisQuntExclude(*k, (next, *m)), false),
            Self::ExisQuntExclude(k, (l, None)) => (Self::ExisQuntExclude(*k, (*l, next)), true),
            Self::ExisQuntExclude(Some(_), (Some(_), Some(_))) => {
                (Self::ExisQuntExclude(next, (None, None)), false)
            }
        }
    }

    pub fn id_replaced(&self, mapping: &BTreeMap<i32, i32>) -> Self {
        let r = |i: &Option<i32>| -> Option<i32> {
            if let Some(i) = i {
                Some(*mapping.get(i).unwrap_or(i))
            } else {
                *i
            }
        };
        match self {
            Self::Premise => Self::Premise,
            Self::AndIntro(k, l) => Self::AndIntro(r(k), r(l)),
            Self::AndExclude(k) => Self::AndExclude(r(k)),
            Self::OrIntro(k, l) => Self::OrIntro(r(k), r(l)),
            Self::OrExclude(k, (l1, m1), (l2, m2)) => {
                Self::OrExclude(r(k), (r(l1), r(m1)), (r(l2), r(m2)))
            }
            Self::IfIntro((k, l)) => Self::IfIntro((r(k), r(l))),
            Self::IfExclude(k, l) => Self::IfExclude(r(k), r(l)),
            Self::IffIntro(k, l) => Self::IffIntro(r(k), r(l)),
            Self::IffExclude(k) => Self::IffExclude(r(k)),
            Self::Falsum(k) => Self::Falsum(r(k)),
            Self::NegIntro((k, l)) => Self::NegIntro((r(k), r(l))),
            Self::NegExclude((k, l)) => Self::NegExclude((r(k), r(l))),
            Self::UnivQuntIntro(k) => Self::UnivQuntIntro(r(k)),
            Self::UnivQuntExclude(k) => Self::UnivQuntExclude(r(k)),
            Self::ExisQuntIntro(k) => Self::ExisQuntIntro(r(k)),
            Self::ExisQuntExclude(k, (l, m)) => Self::ExisQuntExclude(r(k), (r(l), r(m))),
        }
    }
}

impl ToString for DerivationRule {
    fn to_string(&self) -> String {
        to_string(
            self,
            |i| i.map_or_else(|| "".to_owned(), |e| e.to_string()),
            false,
        )
    }
}

impl Serialize for DerivationRule {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&to_string(
            self,
            |i| i.map_or_else(|| "".to_owned(), |e| e.to_string()),
            false,
        ))
    }
}

struct StrVisitor;

impl<'de> Visitor<'de> for StrVisitor {
    type Value = DerivationRule;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("formatted derivation rule")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match from_string(value) {
            Ok(r) => Ok(r),
            Err(Error::RemainingTokenError {
                remaining_string: _,
            }) => Err(E::custom("cannot parse rule")),
            Err(Error::ParseError) => Err(E::custom("cannot parse rule")),
        }
    }
}

impl<'de> Deserialize<'de> for DerivationRule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(StrVisitor)
    }
}

pub enum Error {
    RemainingTokenError { remaining_string: String },
    ParseError,
}

pub fn to_string<F>(value: &DerivationRule, format_id: F, always_show_blank: bool) -> String
where
    F: Fn(&Option<i32>) -> String,
{
    let fmt = format_id;
    match value {
        DerivationRule::Premise => "P".to_owned(),
        DerivationRule::AndIntro(k, l) => {
            format!("{}, {} &I", fmt(k), fmt(l))
        }
        DerivationRule::AndExclude(k) => format!("{} &E", fmt(k)),
        DerivationRule::OrIntro(k, l) => match l {
            None => format!("{} ∨I", fmt(k)),
            Some(l) => format!("{}, {} ∨I", fmt(k), fmt(&Some(*l))),
        },
        DerivationRule::OrExclude(k, (l1, m1), (l2, m2)) => format!(
            "{}, {}-{}, {}-{} ∨E",
            fmt(k),
            fmt(l1),
            fmt(m1),
            fmt(l2),
            fmt(m2)
        ),
        DerivationRule::IfIntro((None, l)) if !always_show_blank => {
            format!("{} →I", fmt(l))
        }
        DerivationRule::IfIntro((k, l)) => {
            format!("{}-{} →I", fmt(k), fmt(l))
        }
        DerivationRule::IfExclude(k, l) => {
            format!("{}, {} →E", fmt(k), fmt(l))
        }
        DerivationRule::IffIntro(k, l) => {
            format!("{}, {} ↔I", fmt(k), fmt(l))
        }
        DerivationRule::IffExclude(k) => format!("{} ↔E", fmt(k)),
        DerivationRule::Falsum(k) => format!("{} ⊥", fmt(k)),
        DerivationRule::NegIntro((k, l)) => {
            format!("{}-{} -I", fmt(k), fmt(l))
        }
        DerivationRule::NegExclude((k, l)) => {
            format!("{}-{} -E", fmt(k), fmt(l))
        }
        DerivationRule::UnivQuntIntro(k) => format!("{} ()I", fmt(k)),
        DerivationRule::UnivQuntExclude(k) => {
            format!("{} ()E", fmt(k))
        }
        DerivationRule::ExisQuntIntro(k) => format!("{} ∃I", fmt(k)),
        DerivationRule::ExisQuntExclude(k, (l, m)) => {
            format!("{}, {}-{} ∃E", fmt(k), fmt(l), fmt(m))
        }
    }
}

pub fn from_string(value: &str) -> Result<DerivationRule, Error> {
    match parse_derivation_rule(value.trim()) {
        Ok(("", rule)) => Ok(match rule {
            derivation_rule::DerivationRule::Premise => DerivationRule::Premise,
            derivation_rule::DerivationRule::AndIntro(k, l) => {
                DerivationRule::AndIntro(Some(k), Some(l))
            }
            derivation_rule::DerivationRule::AndExclude(k) => DerivationRule::AndExclude(Some(k)),
            derivation_rule::DerivationRule::OrIntro(k, l) => DerivationRule::OrIntro(Some(k), l),
            derivation_rule::DerivationRule::OrExclude(k, (l1, m1), (l2, m2)) => {
                DerivationRule::OrExclude(Some(k), (Some(l1), Some(m1)), (Some(l2), Some(m2)))
            }
            derivation_rule::DerivationRule::IfIntro((k, l)) => {
                DerivationRule::IfIntro((k, Some(l)))
            }
            derivation_rule::DerivationRule::IfExclude(k, l) => {
                DerivationRule::IfExclude(Some(k), Some(l))
            }
            derivation_rule::DerivationRule::IffIntro(k, l) => {
                DerivationRule::IffIntro(Some(k), Some(l))
            }
            derivation_rule::DerivationRule::IffExclude(k) => DerivationRule::IffExclude(Some(k)),
            derivation_rule::DerivationRule::NegIntro((k, l)) => {
                DerivationRule::NegIntro((Some(k), Some(l)))
            }
            derivation_rule::DerivationRule::Falsum(k) => DerivationRule::Falsum(Some(k)),
            derivation_rule::DerivationRule::NegExclude((k, l)) => {
                DerivationRule::NegExclude((Some(k), Some(l)))
            }
            derivation_rule::DerivationRule::UnivQuntIntro(k) => {
                DerivationRule::UnivQuntIntro(Some(k))
            }
            derivation_rule::DerivationRule::UnivQuntExclude(k) => {
                DerivationRule::UnivQuntExclude(Some(k))
            }
            derivation_rule::DerivationRule::ExisQuntIntro(k) => {
                DerivationRule::ExisQuntIntro(Some(k))
            }
            derivation_rule::DerivationRule::ExisQuntExclude(k, (l, m)) => {
                DerivationRule::ExisQuntExclude(Some(k), (Some(l), Some(m)))
            }
        }),
        Ok((rem, _)) => Err(Error::RemainingTokenError {
            remaining_string: rem.to_owned(),
        }),
        Err(_) => Err(Error::ParseError),
    }
}
