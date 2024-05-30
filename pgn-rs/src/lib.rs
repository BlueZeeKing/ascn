use nom::{
    combinator::map,
    multi::many0_count,
    sequence::{delimited, preceded, terminated},
    IResult,
};
use parse::{
    parse_partial_move, parse_tag_pair, parse_termination, whitespace, PartialMove, TagPair,
    Termination,
};

pub mod parse;

pub trait Visitor {
    fn tag_pair(&mut self, pair: TagPair);
    fn partial_move(&mut self, mv: PartialMove);
    fn termination(&mut self, term: Termination);
}

pub fn parse<'a, V: Visitor>(input: &'a str, visitor: &mut V) -> IResult<&'a str, ()> {
    let (input, _) = preceded(
        whitespace,
        many0_count(delimited(
            whitespace,
            map(parse_tag_pair, |pair| visitor.tag_pair(pair)),
            whitespace,
        )),
    )(input)?;

    let (input, _) = many0_count(map(parse_partial_move, |partial_move| {
        visitor.partial_move(partial_move)
    }))(input)?;

    terminated(
        map(parse_termination, |term| visitor.termination(term)),
        whitespace,
    )(input)
}
