use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::character::complete::satisfy;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::IResult;

pub fn dim_num(s: &str) -> IResult<&str, u8> {
    map_res(digit1, |s: &str| s.parse::<u8>())(s)
}

pub fn subscr(s: &str) -> IResult<&str, &str> {
    recognize(pair(tag("_"), digit1))(s)
}

pub fn dim(s: &str) -> IResult<&str, u8> {
    preceded(tag("^"), dim_num)(s)
}

pub fn var_ch(s: &str) -> IResult<&str, char> {
    satisfy(|c| c >= 'u' && c <= 'z')(s)
}

pub fn ind_ch(s: &str) -> IResult<&str, char> {
    satisfy(|c| c >= 'a' && c <= 't')(s)
}

pub fn var(s: &str) -> IResult<&str, &str> {
    recognize(pair(var_ch, opt(subscr)))(s)
}

pub fn ind(s: &str) -> IResult<&str, &str> {
    recognize(pair(ind_ch, opt(subscr)))(s)
}

// 개체문자(개체상항 + 변항)
pub fn ind_sym(s: &str) -> IResult<&str, &str> {
    alt((var, ind))(s)
}

pub fn pre(s: &str) -> IResult<&str, &str> {
    recognize(pair(satisfy(|c| c >= 'A' && c <= 'Z'), opt(subscr)))(s)
}
