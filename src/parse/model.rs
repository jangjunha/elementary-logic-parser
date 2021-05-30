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
    pub fn replaced(&self, alpha: &str, beta: &str) -> Exp {
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
            Self::UnivGenr(var, _) if var == alpha => self.clone(),
            Self::UnivGenr(var, inner) => {
                Self::UnivGenr(var.clone(), Box::new(inner.replaced(alpha, beta)))
            }
            Self::ExistGenr(var, _) if var == alpha => self.clone(),
            Self::ExistGenr(var, inner) => {
                Self::ExistGenr(var.clone(), Box::new(inner.replaced(alpha, beta)))
            }
            Self::Falsum => Self::Falsum,
        }
    }
}
