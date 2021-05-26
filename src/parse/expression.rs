use super::individual_constant::{dim, ind_sym, pre};
use super::util::ws;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::map;
use nom::combinator::value;
use nom::multi::count;
use nom::multi::many0;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;

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

pub fn exp(s: &str) -> IResult<&str, Exp> {
    cond_exp(s)
}

fn _explicit_ind_sym(s: &str) -> IResult<&str, Vec<&str>> {
    let (s1, n) = dim(s)?;
    count(ind_sym, n as usize)(s1)
}

fn _implicit_ind_sym(s: &str) -> IResult<&str, Vec<&str>> {
    many0(ind_sym)(s)
}

fn atom_exp(s: &str) -> IResult<&str, Exp> {
    map(
        pair(pre, alt((_explicit_ind_sym, _implicit_ind_sym))),
        |(p, i)| Exp::Atom(p.to_owned(), i.iter().map(|&s| s.to_owned()).collect()),
    )(s)
}

fn right_arrow(s: &str) -> IResult<&str, &str> {
    alt((tag("→"), tag("->")))(s)
}

fn left_right_arrow(s: &str) -> IResult<&str, &str> {
    alt((tag("↔"), tag("<->")))(s)
}

fn if_exp(s: &str) -> IResult<&str, Exp> {
    map(
        tuple((cond_exp, ws(right_arrow), bool_exp)),
        |(lhs, _, rhs)| Exp::Cond(Box::new(lhs), Box::new(rhs)),
    )(s)
}

fn iff_exp(s: &str) -> IResult<&str, Exp> {
    map(
        tuple((cond_exp, ws(left_right_arrow), bool_exp)),
        |(lhs, _, rhs)| Exp::Iff(Box::new(lhs), Box::new(rhs)),
    )(s)
}

fn cond_exp(s: &str) -> IResult<&str, Exp> {
    alt((if_exp, iff_exp, bool_exp))(s)
}

fn and(s: &str) -> IResult<&str, &str> {
    tag("&")(s)
}

fn and_exp(s: &str) -> IResult<&str, Exp> {
    map(tuple((bool_exp, ws(and), neg_exp)), |(lhs, _, rhs)| {
        Exp::And(Box::new(lhs), Box::new(rhs))
    })(s)
}

fn or(s: &str) -> IResult<&str, &str> {
    tag("∨")(s)
}

fn or_exp(s: &str) -> IResult<&str, Exp> {
    map(tuple((bool_exp, ws(or), neg_exp)), |(lhs, _, rhs)| {
        Exp::Or(Box::new(lhs), Box::new(rhs))
    })(s)
}

fn bool_exp(s: &str) -> IResult<&str, Exp> {
    alt((and_exp, or_exp, neg_exp))(s)
}

fn negation(s: &str) -> IResult<&str, &str> {
    tag("-")(s)
}

fn negate_exp(s: &str) -> IResult<&str, Exp> {
    map(preceded(negation, preceded(multispace0, neg_exp)), |e| {
        Exp::Neg(Box::new(e))
    })(s)
}

fn neg_exp(s: &str) -> IResult<&str, Exp> {
    alt((negate_exp, f))(s)
}

fn parenthesesed_exp(s: &str) -> IResult<&str, Exp> {
    delimited(tag("("), ws(exp), tag(")"))(s)
}

fn falsum(s: &str) -> IResult<&str, Exp> {
    value(Exp::Falsum, tag("⊥"))(s)
}

fn f(s: &str) -> IResult<&str, Exp> {
    alt((atom_exp, falsum, parenthesesed_exp))(s)
}
