use std::collections::BTreeSet;

// TODO: FIXME: PartialEq
// UnivGenr(a, Rax) == UnivGenr(b, Rbx)
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
