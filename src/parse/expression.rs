use super::common::{and, existential, falsum, left_right_arrow, negation, or, right_arrow};
use super::individual_constant::{dim, ind_sym, pre, var};
use super::util::ws;
use super::Exp;
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

pub fn exp(s: &str) -> IResult<&str, Exp> {
    cond_exp(s)
}

fn _explicit_ind_sym(s: &str) -> IResult<&str, Vec<&str>> {
    let (s1, n) = dim(s)?;
    count(preceded(multispace0, ind_sym), n as usize)(s1)
}

fn _implicit_ind_sym(s: &str) -> IResult<&str, Vec<&str>> {
    many0(preceded(multispace0, ind_sym))(s)
}

fn atom_exp(s: &str) -> IResult<&str, Exp> {
    map(
        pair(pre, alt((_explicit_ind_sym, _implicit_ind_sym))),
        |(p, i)| Exp::Atom(p.to_owned(), i.iter().map(|&e| e.to_owned()).collect()),
    )(s)
}

fn if_exp(s: &str) -> IResult<&str, Exp> {
    map(
        tuple((bool_exp, ws(right_arrow), cond_exp)),
        |(lhs, _, rhs)| Exp::Cond(Box::new(lhs), Box::new(rhs)),
    )(s)
}

fn iff_exp(s: &str) -> IResult<&str, Exp> {
    map(
        tuple((bool_exp, ws(left_right_arrow), cond_exp)),
        |(lhs, _, rhs)| Exp::Iff(Box::new(lhs), Box::new(rhs)),
    )(s)
}

fn cond_exp(s: &str) -> IResult<&str, Exp> {
    alt((if_exp, iff_exp, bool_exp))(s)
}

fn and_exp(s: &str) -> IResult<&str, Exp> {
    map(tuple((neg_exp, ws(and), bool_exp)), |(lhs, _, rhs)| {
        Exp::And(Box::new(lhs), Box::new(rhs))
    })(s)
}

fn or_exp(s: &str) -> IResult<&str, Exp> {
    map(tuple((neg_exp, ws(or), bool_exp)), |(lhs, _, rhs)| {
        Exp::Or(Box::new(lhs), Box::new(rhs))
    })(s)
}

fn bool_exp(s: &str) -> IResult<&str, Exp> {
    alt((and_exp, or_exp, neg_exp))(s)
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

fn univ_genr_exp(s: &str) -> IResult<&str, Exp> {
    map(
        pair(
            delimited(tag("("), ws(var), tag(")")),
            preceded(multispace0, f),
        ),
        |(v, e)| Exp::UnivGenr(v.to_owned(), Box::new(e)),
    )(s)
}

fn exist_genr_exp(s: &str) -> IResult<&str, Exp> {
    map(
        pair(
            delimited(
                tuple((tag("("), preceded(multispace0, existential))),
                ws(var),
                tag(")"),
            ),
            preceded(multispace0, f),
        ),
        |(v, e)| Exp::ExistGenr(v.to_owned(), Box::new(e)),
    )(s)
}

fn genr_exp(s: &str) -> IResult<&str, Exp> {
    alt((exist_genr_exp, univ_genr_exp))(s)
}

fn falsum_exp(s: &str) -> IResult<&str, Exp> {
    value(Exp::Falsum, falsum)(s)
}

fn f(s: &str) -> IResult<&str, Exp> {
    alt((atom_exp, falsum_exp, genr_exp, parenthesesed_exp))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    // ∃⊥∨→↔

    #[test]
    fn exp_valid() {
        let exp1 = Exp::Iff(
            Box::new(Exp::ExistGenr(
                "y".to_owned(),
                Box::new(Exp::And(
                    Box::new(Exp::Atom("F".to_owned(), vec!["y".to_owned()])),
                    Box::new(Exp::Atom(
                        "G".to_owned(),
                        vec!["y".to_owned(), "y".to_owned()],
                    )),
                )),
            )),
            Box::new(Exp::ExistGenr(
                "y".to_owned(),
                Box::new(Exp::And(
                    Box::new(Exp::Atom("F".to_owned(), vec!["y".to_owned()])),
                    Box::new(Exp::ExistGenr(
                        "x".to_owned(),
                        Box::new(Exp::And(
                            Box::new(Exp::Atom("F".to_owned(), vec!["x".to_owned()])),
                            Box::new(Exp::Atom(
                                "G".to_owned(),
                                vec!["y".to_owned(), "x".to_owned()],
                            )),
                        )),
                    )),
                )),
            )),
        );
        assert_eq!(
            exp("(∃y)(Fy & Gyy) ↔ (∃y)(Fy & (∃x)(Fx & Gyx))"),
            IResult::Ok(("", exp1.clone()))
        );
        assert_eq!(
            exp("( ∃ y )(Fy&Gyy) <-> (∃y)( F y & (∃x)( F x & G yx ))"),
            IResult::Ok(("", exp1.clone()))
        );
    }

    #[test]
    fn atom_exp_valid() {
        assert_eq!(
            atom_exp("P"),
            IResult::Ok(("", Exp::Atom("P".to_owned(), vec![])))
        );
        assert_eq!(
            atom_exp("P_10"),
            IResult::Ok(("", Exp::Atom("P_10".to_owned(), vec![])))
        );
        assert_eq!(
            atom_exp("R_2^3xya"),
            IResult::Ok((
                "",
                Exp::Atom(
                    "R_2".to_owned(),
                    vec!["x".to_owned(), "y".to_owned(), "a".to_owned()]
                )
            ))
        );
        assert_eq!(
            atom_exp("R_2xy_2a"),
            IResult::Ok((
                "",
                Exp::Atom(
                    "R_2".to_owned(),
                    vec!["x".to_owned(), "y_2".to_owned(), "a".to_owned()]
                )
            ))
        );
        assert_eq!(
            atom_exp("R_2 x y a"),
            IResult::Ok((
                "",
                Exp::Atom(
                    "R_2".to_owned(),
                    vec!["x".to_owned(), "y".to_owned(), "a".to_owned()]
                )
            ))
        );
    }

    #[test]
    fn atom_exp_invalid() {
        assert_eq!(
            atom_exp("P^2AB"),
            IResult::Ok(("^2AB", Exp::Atom("P".to_owned(), vec![])))
        );
        assert_eq!(
            atom_exp("P^2x"),
            IResult::Ok(("^2x", Exp::Atom("P".to_owned(), vec![])))
        );
        assert_eq!(
            atom_exp("P^2xyz"),
            IResult::Ok((
                "z",
                Exp::Atom("P".to_owned(), vec!["x".to_owned(), "y".to_owned()])
            ))
        );
        assert_eq!(
            atom_exp("P ^2"),
            IResult::Ok((" ^2", Exp::Atom("P".to_owned(), vec![])))
        );
        assert_eq!(
            atom_exp("R^1_2x"),
            IResult::Ok(("^1_2x", Exp::Atom("R".to_owned(), vec![])))
        );
    }
}
