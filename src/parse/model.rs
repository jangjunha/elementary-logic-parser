use std::collections::{BTreeMap, BTreeSet};
use std::iter::FromIterator;

// TODO: FIXME: PartialEq
// UnivGenr(a, Rax) == UnivGenr(b, Rbx)
// B & A == A & B
#[derive(PartialEq, Clone, Debug)]
pub enum Exp {
    Atom(String, Vec<String>),
    Cond(Box<Exp>, Box<Exp>),
    Iff(Box<Exp>, Box<Exp>),
    And(Box<Exp>, Box<Exp>),
    Or(Box<Exp>, Box<Exp>),
    Neg(Box<Exp>),
    UnivGenr(String, Box<Exp>),
    ExistGenr(String, Box<Exp>),
    Falsum,
}

impl Exp {
    fn tuple_form_eq(s: (&Exp, &Exp), o: (&Exp, &Exp)) -> Option<BTreeMap<String, String>> {
        let (s_lhs, s_rhs) = s;
        let (o_lhs, o_rhs) = o;
        match (s_lhs.form_eq(o_lhs), s_rhs.form_eq(o_rhs)) {
            (Some(lm), Some(rm)) => {
                let mut res = lm.clone();
                for (k, v) in rm.into_iter() {
                    let existing = res.insert(k, v);
                    if let Some(_) = existing {
                        return None;
                    }
                }
                Some(res)
            }
            _ => None,
        }
    }

    // determine equal with predicate replacement
    pub fn form_eq(&self, other: &Self) -> Option<BTreeMap<String, String>> {
        match (self, other) {
            (Self::Atom(s_pre, s_inds), Self::Atom(o_pre, o_inds))
                if s_inds.is_empty() && o_inds.is_empty() =>
            {
                Some(BTreeMap::from_iter(
                    [(s_pre.clone(), o_pre.clone())].iter().cloned(),
                ))
            }
            (Self::Atom(_, _), _) => None,
            // not commtative
            (Self::Cond(s_lhs, s_rhs), Self::Cond(o_lhs, o_rhs)) => {
                Self::tuple_form_eq((s_lhs, s_rhs), (o_lhs, o_rhs))
            }
            (Self::Cond(_, _), _) => None,
            // commtative
            (Self::Iff(s_lhs, s_rhs), Self::Iff(o_lhs, o_rhs))
            | (Self::And(s_lhs, s_rhs), Self::And(o_lhs, o_rhs))
            | (Self::Or(s_lhs, s_rhs), Self::Or(o_lhs, o_rhs)) => {
                Self::tuple_form_eq((s_lhs, s_rhs), (o_lhs, o_rhs))
                    .or_else(|| Self::tuple_form_eq((s_lhs, s_rhs), (o_rhs, o_lhs)))
            }
            (Self::Iff(_, _), _) => None,
            (Self::And(_, _), _) => None,
            (Self::Or(_, _), _) => None,
            (Self::Neg(s), Self::Neg(o)) => s.form_eq(o),
            (Self::Neg(_), _) => None,
            (Self::UnivGenr(s_var, s_in), Self::UnivGenr(o_var, o_in))
            | (Self::ExistGenr(s_var, s_in), Self::ExistGenr(o_var, o_in)) => {
                s_in.form_eq(&o_in.var_replaced(o_var, s_var))
            }
            (Self::UnivGenr(_, _), _) => None,
            (Self::ExistGenr(_, _), _) => None,
            (Self::Falsum, Self::Falsum) => Some(BTreeMap::new()),
            (Self::Falsum, _) => None,
        }
    }

    pub fn free_variables(&self) -> BTreeSet<String> {
        match self {
            Self::Atom(_, inds) => inds.iter().cloned().collect(),
            Self::Cond(lhs, rhs)
            | Self::Iff(lhs, rhs)
            | Self::And(lhs, rhs)
            | Self::Or(lhs, rhs) => lhs
                .free_variables()
                .union(&rhs.free_variables())
                .cloned()
                .collect(),
            Self::Neg(lhs) => lhs.free_variables(),
            Self::UnivGenr(var, inner) | Self::ExistGenr(var, inner) => {
                let mut vars = inner.free_variables();
                vars.remove(var);
                vars
            }
            Self::Falsum => BTreeSet::new(),
        }
    }

    pub fn negated(&self) -> Exp {
        // TODO: Box 말고 레퍼런스를 쓰는 게 맞을지도
        Exp::Neg(Box::new(self.clone()))
    }

    pub fn replaced(&self, alpha: &Exp, beta: &Exp) -> Exp {
        if self == alpha {
            beta.clone()
        } else {
            match self {
                Self::Atom(_, _) => self.clone(),
                Self::Cond(lhs, rhs) => Self::Cond(
                    Box::new(lhs.replaced(alpha, beta)),
                    Box::new(rhs.replaced(alpha, beta)),
                ),
                Self::Iff(lhs, rhs) => Self::Iff(
                    Box::new(lhs.replaced(alpha, beta)),
                    Box::new(rhs.replaced(alpha, beta)),
                ),
                Self::And(lhs, rhs) => Self::And(
                    Box::new(lhs.replaced(alpha, beta)),
                    Box::new(rhs.replaced(alpha, beta)),
                ),
                Self::Or(lhs, rhs) => Self::Or(
                    Box::new(lhs.replaced(alpha, beta)),
                    Box::new(rhs.replaced(alpha, beta)),
                ),
                Self::Neg(lhs) => Self::Neg(Box::new(lhs.replaced(alpha, beta))),
                Self::UnivGenr(var, inner) => {
                    Self::UnivGenr(var.clone(), Box::new(inner.replaced(alpha, beta)))
                }
                Self::ExistGenr(var, inner) => {
                    Self::ExistGenr(var.clone(), Box::new(inner.replaced(alpha, beta)))
                }
                Self::Falsum => self.clone(),
            }
        }
    }

    // Return free variable replaced expression.
    pub fn var_replaced(&self, alpha: &str, beta: &str) -> Exp {
        match self {
            Self::Atom(pre, inds) => Self::Atom(
                pre.clone(),
                inds.iter()
                    .map(|i| {
                        if i == alpha {
                            beta.to_owned()
                        } else {
                            i.clone()
                        }
                    })
                    .collect(),
            ),
            Self::Cond(lhs, rhs) => Self::Cond(
                Box::new(lhs.var_replaced(alpha, beta)),
                Box::new(rhs.var_replaced(alpha, beta)),
            ),
            Self::Iff(lhs, rhs) => Self::Iff(
                Box::new(lhs.var_replaced(alpha, beta)),
                Box::new(rhs.var_replaced(alpha, beta)),
            ),
            Self::And(lhs, rhs) => Self::And(
                Box::new(lhs.var_replaced(alpha, beta)),
                Box::new(rhs.var_replaced(alpha, beta)),
            ),
            Self::Or(lhs, rhs) => Self::Or(
                Box::new(lhs.var_replaced(alpha, beta)),
                Box::new(rhs.var_replaced(alpha, beta)),
            ),
            Self::Neg(lhs) => Self::Neg(Box::new(lhs.var_replaced(alpha, beta))),
            Self::UnivGenr(var, _) if var == alpha => self.clone(),
            Self::UnivGenr(var, inner) => {
                Self::UnivGenr(var.clone(), Box::new(inner.var_replaced(alpha, beta)))
            }
            Self::ExistGenr(var, _) if var == alpha => self.clone(),
            Self::ExistGenr(var, inner) => {
                Self::ExistGenr(var.clone(), Box::new(inner.var_replaced(alpha, beta)))
            }
            Self::Falsum => Self::Falsum,
        }
    }
}

pub fn to_string(exp: &Exp) -> String {
    match exp {
        Exp::Atom(pre, inds) => format!("{}{}", pre, inds.join("")),
        Exp::Cond(lhs, rhs) => format!("({} → {})", to_string(lhs), to_string(rhs)),
        Exp::Iff(lhs, rhs) => format!("({} ↔ {})", to_string(lhs), to_string(rhs)),
        Exp::And(lhs, rhs) => format!("({} & {})", to_string(lhs), to_string(rhs)),
        Exp::Or(lhs, rhs) => format!("({} ∨ {})", to_string(lhs), to_string(rhs)),
        Exp::Neg(lhs) => format!("-{}", to_string(lhs)),
        Exp::UnivGenr(var, inner) => format!("({}){}", var, to_string(inner)),
        Exp::ExistGenr(var, inner) => format!("(∃{}){}", var, to_string(inner)),
        Exp::Falsum => "⊥".to_owned(),
    }
}
