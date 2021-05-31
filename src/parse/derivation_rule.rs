use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, multispace1};
use nom::combinator::opt;
use nom::combinator::{map, map_res, value};
use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use super::common::{
    and, existential, falsum as falsum_tag, left_right_arrow, negation, or, right_arrow,
};
use super::util::ws;

#[derive(Debug, Clone, PartialEq)]
pub enum DerivationRule {
    Premise,
    AndIntro(i32, i32),
    AndExclude(i32),
    OrIntro(i32, Option<i32>),
    OrExclude(i32, (i32, i32), (i32, i32)),
    IfIntro((Option<i32>, i32)),
    IfExclude(i32, i32),
    IffIntro(i32, i32),
    IffExclude(i32),
    Falsum(i32),
    NegIntro((i32, i32)),
    NegExclude((i32, i32)),
    UnivQuntIntro(i32),
    UnivQuntExclude(i32),
    ExisQuntIntro(i32),
    ExisQuntExclude(i32, (i32, i32)),
}

pub fn num(s: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse::<i32>())(s)
}

pub fn sep(s: &str) -> IResult<&str, &str> {
    ws(tag(","))(s)
}

pub fn range(s: &str) -> IResult<&str, (i32, i32)> {
    separated_pair(num, ws(tag("-")), num)(s)
}

pub fn premise(s: &str) -> IResult<&str, DerivationRule> {
    value(DerivationRule::Premise, tag("P"))(s)
}

pub fn and_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            separated_pair(num, sep, num),
            preceded(multispace1, pair(and, tag("I"))),
        ),
        |(k, l)| DerivationRule::AndIntro(k, l),
    )(s)
}

pub fn and_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(num, preceded(multispace1, pair(and, tag("E")))),
        |k| DerivationRule::AndExclude(k),
    )(s)
}

pub fn or_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            pair(num, opt(preceded(sep, num))),
            preceded(multispace1, pair(or, tag("I"))),
        ),
        |(k, l)| DerivationRule::OrIntro(k, l),
    )(s)
}

pub fn or_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            tuple((num, sep, range, sep, range)),
            preceded(multispace1, pair(or, tag("E"))),
        ),
        |(k, _, (l1, m1), _, (l2, m2))| DerivationRule::OrExclude(k, (l1, m1), (l2, m2)),
    )(s)
}

pub fn if_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            pair(opt(terminated(num, ws(tag("-")))), num),
            preceded(multispace1, pair(right_arrow, tag("I"))),
        ),
        |(k, l)| DerivationRule::IfIntro((k, l)),
    )(s)
}

pub fn if_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            separated_pair(num, sep, num),
            preceded(multispace1, pair(right_arrow, tag("E"))),
        ),
        |(k, l)| DerivationRule::IfExclude(k, l),
    )(s)
}

pub fn iff_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            separated_pair(num, sep, num),
            preceded(multispace1, pair(left_right_arrow, tag("I"))),
        ),
        |(k, l)| DerivationRule::IffIntro(k, l),
    )(s)
}

pub fn iff_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(num, preceded(multispace1, pair(left_right_arrow, tag("E")))),
        |k| DerivationRule::IffExclude(k),
    )(s)
}

pub fn falsum(s: &str) -> IResult<&str, DerivationRule> {
    map(terminated(num, preceded(multispace1, falsum_tag)), |k| {
        DerivationRule::Falsum(k)
    })(s)
}

pub fn neg_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(range, preceded(multispace1, pair(negation, tag("I")))),
        |(k, l)| DerivationRule::NegIntro((k, l)),
    )(s)
}

pub fn neg_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(range, preceded(multispace1, pair(negation, tag("E")))),
        |(k, l)| DerivationRule::NegExclude((k, l)),
    )(s)
}

pub fn univ_qunt_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(num, preceded(multispace1, pair(tag("()"), tag("I")))),
        |k| DerivationRule::UnivQuntIntro(k),
    )(s)
}

pub fn univ_qunt_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(num, preceded(multispace1, pair(tag("()"), tag("E")))),
        |k| DerivationRule::UnivQuntExclude(k),
    )(s)
}

pub fn exis_qunt_intro(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(num, preceded(multispace1, pair(existential, tag("I")))),
        |k| DerivationRule::ExisQuntIntro(k),
    )(s)
}

pub fn exis_qunt_exclude(s: &str) -> IResult<&str, DerivationRule> {
    map(
        terminated(
            tuple((num, sep, range)),
            preceded(multispace1, pair(existential, tag("E"))),
        ),
        |(k, _, (l, m))| DerivationRule::ExisQuntExclude(k, (l, m)),
    )(s)
}

pub fn derivation_rule(s: &str) -> IResult<&str, DerivationRule> {
    alt((
        premise,
        and_intro,
        and_exclude,
        or_intro,
        or_exclude,
        if_intro,
        if_exclude,
        iff_intro,
        iff_exclude,
        falsum,
        neg_intro,
        neg_exclude,
        univ_qunt_intro,
        univ_qunt_exclude,
        exis_qunt_intro,
        exis_qunt_exclude,
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn derivation_rule_valid() {
        assert_eq!(
            derivation_rule("3 →I"),
            IResult::Ok(("", DerivationRule::IfIntro((None, 3))))
        );
        assert_eq!(
            derivation_rule("2-3 →I"),
            IResult::Ok(("", DerivationRule::IfIntro((Some(2), 3))))
        );
        assert_eq!(
            derivation_rule("7-14 →I"),
            IResult::Ok(("", DerivationRule::IfIntro((Some(7), 14))))
        );
    }
}
