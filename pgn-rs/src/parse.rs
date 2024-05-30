use std::num::NonZeroUsize;

use chess::{File, Piece, Rank, Square};
use nom::{
    branch::alt,
    bytes::streaming::*,
    character::{streaming::*, *},
    combinator::*,
    error::{Error as NomError, ErrorKind},
    multi::{many0, many1_count},
    sequence::*,
    IResult,
};

fn parse_symbol(input: &str) -> IResult<&str, &str> {
    verify(
        take_while(|char: char| {
            char.is_alphanumeric()
                || char == '_'
                || char == '+'
                || char == '#'
                || char == '='
                || char == ':'
                || char == '-'
        }),
        |value: &str| is_alphanumeric(value.as_bytes()[0]),
    )(input)
}

fn parse_nag(input: &str) -> IResult<&str, NAG> {
    map(preceded(tag("$"), u8), |num| unsafe {
        std::mem::transmute::<u8, NAG>(num)
    })(input)
}

pub fn whitespace(input: &str) -> IResult<&str, ()> {
    let res: IResult<&str, ()> = value((), take_while(|char: char| char.is_whitespace()))(input);

    match res {
        Ok((input, _)) => Ok((input, ())),
        Err(err) if err.is_incomplete() => Ok((input, ())),
        n => n,
    }
}

fn parse_string<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    delimited(
        tag("\""),
        |input: &'a str| -> IResult<&'a str, &'a str> {
            let mut idx = 0;

            while {
                if idx >= input.len() {
                    return IResult::Err(nom::Err::Incomplete(nom::Needed::Unknown));
                }
                while !input.is_char_boundary(idx) || !input.is_char_boundary(idx + 1) {
                    idx += 1;
                    if idx >= input.len() {
                        return IResult::Err(nom::Err::Incomplete(nom::Needed::Unknown));
                    }
                }

                if &input[idx..idx + 1] == "\\" {
                    idx += 1;
                }

                if idx >= input.len() {
                    return IResult::Err(nom::Err::Incomplete(nom::Needed::Unknown));
                }

                while !input.is_char_boundary(idx) || !input.is_char_boundary(idx + 1) {
                    idx += 1;
                    if idx >= input.len() {
                        return IResult::Err(nom::Err::Incomplete(nom::Needed::Unknown));
                    }
                }

                &input[idx..idx + 1] != "\""
            } {
                idx += 1;
            }

            IResult::Ok((&input[idx..], &input[..idx]))
        },
        tag("\""),
    )(input)
}

#[derive(Debug, PartialEq)]
pub struct TagPair<'a> {
    pub key: &'a str,
    pub value: &'a str,
}

pub fn parse_tag_pair(input: &str) -> IResult<&str, TagPair<'_>> {
    delimited(
        terminated(tag("["), whitespace),
        map(
            separated_pair(parse_symbol, whitespace, parse_string),
            |(key, value)| TagPair { key, value },
        ),
        preceded(whitespace, tag("]")),
    )(input)
}

/// Returns the move number and then the number of periods
fn parse_move_text_number(input: &str) -> IResult<&str, (u32, usize)> {
    pair(u32, many1_count(tag(".")))(input)
}

fn parse_file(input: &str) -> IResult<&str, File> {
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::Size(
            NonZeroUsize::new(1).unwrap(),
        )));
    }

    Ok((
        &input[1..],
        match input.chars().next().unwrap() {
            'a' => File::A,
            'b' => File::B,
            'c' => File::C,
            'd' => File::D,
            'e' => File::E,
            'f' => File::F,
            'g' => File::G,
            'h' => File::H,
            _ => {
                return Err(nom::Err::Error(NomError {
                    input: &input[0..1],
                    code: ErrorKind::Tag,
                }));
            }
        },
    ))
}

fn parse_rank(input: &str) -> IResult<&str, Rank> {
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::Size(
            NonZeroUsize::new(1).unwrap(),
        )));
    }

    Ok((
        &input[1..],
        match input.chars().next().unwrap() {
            '1' => Rank::First,
            '2' => Rank::Second,
            '3' => Rank::Third,
            '4' => Rank::Fourth,
            '5' => Rank::Fifth,
            '6' => Rank::Sixth,
            '7' => Rank::Seventh,
            '8' => Rank::Eighth,
            _ => {
                return Err(nom::Err::Error(NomError {
                    input: &input[0..1],
                    code: ErrorKind::Tag,
                }));
            }
        },
    ))
}

fn parse_square(input: &str) -> IResult<&str, Square> {
    map(pair(parse_file, parse_rank), |(file, rank)| {
        Square::make_square(rank, file)
    })(input)
}

fn parse_piece(input: &str) -> IResult<&str, Piece> {
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::Size(
            NonZeroUsize::new(1).unwrap(),
        )));
    }

    Ok((
        &input[1..],
        match input.chars().next().unwrap() {
            'P' => Piece::Pawn,
            'N' => Piece::Knight,
            'B' => Piece::Bishop,
            'R' => Piece::Rook,
            'Q' => Piece::Queen,
            'K' => Piece::King,
            _ => {
                return Err(nom::Err::Error(NomError {
                    input: &input[0..1],
                    code: ErrorKind::Tag,
                }));
            }
        },
    ))
}

pub fn parse_rav(input: &str) -> IResult<&str, Vec<PartialMove>> {
    delimited(ws(tag("(")), many0(parse_partial_move), ws(tag(")")))(input)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Castle {
    KingSide,
    QueenSide,
}

fn parse_castle(input: &str) -> IResult<&str, CastleInfo> {
    map(
        tuple((
            alt((
                value(Castle::QueenSide, tag("O-O-O")),
                value(Castle::KingSide, tag("O-O")),
            )),
            alt((
                value(Some(CheckResult::Check), tag("+")),
                value(Some(CheckResult::Checkmate), tag("#")),
                success(None),
            )),
        )),
        |(castle, check_value)| CastleInfo {
            castle,
            check_value,
        },
    )(input)
}

fn parse_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("{"), is_not("}"), tag("}"))(input)
}

#[derive(Debug)]
pub struct BasicSAN {
    pub piece: Piece,
    pub square: Square,
    pub capture: bool,
    pub promotion: Option<Piece>,
    pub starting_file: Option<File>,
    pub starting_rank: Option<Rank>,
    pub check_value: Option<CheckResult>,
}

#[derive(Debug)]
pub enum SAN {
    Basic(BasicSAN),
    Castle(CastleInfo),
}

#[derive(Debug)]
pub struct CastleInfo {
    pub castle: Castle,
    pub check_value: Option<CheckResult>,
}

pub fn parse_san(input: &str) -> IResult<&str, SAN> {
    alt((
        map(parse_castle, SAN::Castle),
        map(parse_basic_san, SAN::Basic),
    ))(input)
}

struct InnerSAN {
    square: Square,
    capture: bool,
    starting_file: Option<File>,
    starting_rank: Option<Rank>,
}

#[derive(Clone, Debug)]
pub enum CheckResult {
    Check,
    Checkmate,
}

fn parse_basic_san(input: &str) -> IResult<&str, BasicSAN> {
    let (input, piece) = alt((parse_piece, success(Piece::Pawn)))(input)?;

    let (input, inner) = alt((
        map(preceded(tag("x"), parse_square), |square| InnerSAN {
            square,
            capture: true,
            starting_rank: None,
            starting_file: None,
        }),
        map(
            tuple((
                opt(parse_file),
                opt(parse_rank),
                map(opt(tag("x")), |val| val.is_some()),
                parse_square,
            )),
            |(starting_file, starting_rank, capture, square)| InnerSAN {
                square,
                capture,
                starting_file,
                starting_rank,
            },
        ),
        map(parse_square, |square| InnerSAN {
            square,
            capture: false,
            starting_rank: None,
            starting_file: None,
        }),
    ))(input)?;

    let (input, promotion) = opt(preceded(tag("="), parse_piece))(input)?;

    let (input, check_result) = alt((
        value(Some(CheckResult::Check), tag("+")),
        value(Some(CheckResult::Checkmate), tag("#")),
        success(None),
    ))(input)?;

    Ok((
        input,
        BasicSAN {
            capture: inner.capture,
            square: inner.square,
            starting_file: inner.starting_file,
            starting_rank: inner.starting_rank,
            check_value: check_result,
            promotion,
            piece,
        },
    ))
}

#[derive(Debug)]
pub struct PartialMove<'a> {
    pub number: Option<(u32, usize)>,
    pub san: SAN,
    pub nag: Option<NAG>,
    pub suffix: Option<NAG>,
    pub initial_comment: Option<&'a str>,
    pub post_san_comment: Option<&'a str>,
    pub post_nag_comment: Option<&'a str>,
    pub post_num_comment: Option<&'a str>,
    pub rav: Option<Vec<PartialMove<'a>>>,
    pub post_rav_comment: Option<&'a str>,
}

pub fn parse_partial_move(input: &str) -> IResult<&str, PartialMove> {
    map(
        tuple((
            ws(opt(parse_comment)),
            opt(parse_move_text_number),
            ws(opt(parse_comment)),
            parse_san,
            opt(parse_suffix),
            ws(opt(parse_comment)),
            opt(parse_nag),
            ws(opt(parse_comment)),
            opt(parse_rav),
            ws(opt(parse_comment)),
        )),
        |(
            initial_comment,
            number,
            post_num_comment,
            san,
            suffix,
            post_san_comment,
            nag,
            post_nag_comment,
            rav,
            post_rav_comment,
        )| {
            PartialMove {
                number,
                san,
                nag,
                suffix,
                initial_comment,
                post_san_comment,
                post_nag_comment,
                post_num_comment,
                rav,
                post_rav_comment,
            }
        },
    )(input)
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum NAG {
    NullAnnotation = 0,
    GoodMoveTraditional = 1,
    PoorMoveTraditional = 2,
    VeryGoodMoveTraditional = 3,
    VeryPoorMoveTraditional = 4,
    SpeculativeMoveTraditional = 5,
    QuestionableMoveTraditional = 6,
    ForcedMove = 7,
    SingularMove = 8,
    WorstMove = 9,
    DrawishPosition = 10,
    EqualChancesQuietPosition = 11,
    EqualChancesActivePosition = 12,
    UnclearPosition = 13,
    WhiteSlightAdvantage = 14,
    BlackSlightAdvantage = 15,
    WhiteModerateAdvantage = 16,
    BlackModerateAdvantage = 17,
    WhiteDecisiveAdvantage = 18,
    BlackDecisiveAdvantage = 19,
    WhiteCrushingAdvantageResign = 20,
    BlackCrushingAdvantageResign = 21,
    WhiteInZugzwang = 22,
    BlackInZugzwang = 23,
    WhiteSlightSpaceAdvantage = 24,
    BlackSlightSpaceAdvantage = 25,
    WhiteModerateSpaceAdvantage = 26,
    BlackModerateSpaceAdvantage = 27,
    WhiteDecisiveSpaceAdvantage = 28,
    BlackDecisiveSpaceAdvantage = 29,
    WhiteSlightTimeAdvantage = 30,
    BlackSlightTimeAdvantage = 31,
    WhiteModerateTimeAdvantage = 32,
    BlackModerateTimeAdvantage = 33,
    WhiteDecisiveTimeAdvantage = 34,
    BlackDecisiveTimeAdvantage = 35,
    WhiteInitiative = 36,
    BlackInitiative = 37,
    WhiteLastingInitiative = 38,
    BlackLastingInitiative = 39,
    WhiteAttack = 40,
    BlackAttack = 41,
    WhiteInsufficientCompensationMaterialDeficit = 42,
    BlackInsufficientCompensationMaterialDeficit = 43,
    WhiteSufficientCompensationMaterialDeficit = 44,
    BlackSufficientCompensationMaterialDeficit = 45,
    WhiteMoreThanAdequateCompensationMaterialDeficit = 46,
    BlackMoreThanAdequateCompensationMaterialDeficit = 47,
    WhiteSlightCenterControlAdvantage = 48,
    BlackSlightCenterControlAdvantage = 49,
    WhiteModerateCenterControlAdvantage = 50,
    BlackModerateCenterControlAdvantage = 51,
    WhiteDecisiveCenterControlAdvantage = 52,
    BlackDecisiveCenterControlAdvantage = 53,
    WhiteSlightKingsideControlAdvantage = 54,
    BlackSlightKingsideControlAdvantage = 55,
    WhiteModerateKingsideControlAdvantage = 56,
    BlackModerateKingsideControlAdvantage = 57,
    WhiteDecisiveKingsideControlAdvantage = 58,
    BlackDecisiveKingsideControlAdvantage = 59,
    WhiteSlightQueensideControlAdvantage = 60,
    BlackSlightQueensideControlAdvantage = 61,
    WhiteModerateQueensideControlAdvantage = 62,
    BlackModerateQueensideControlAdvantage = 63,
    WhiteDecisiveQueensideControlAdvantage = 64,
    BlackDecisiveQueensideControlAdvantage = 65,
    WhiteVulnerableFirstRank = 66,
    BlackVulnerableFirstRank = 67,
    WhiteWellProtectedFirstRank = 68,
    BlackWellProtectedFirstRank = 69,
    WhitePoorlyProtectedKing = 70,
    BlackPoorlyProtectedKing = 71,
    WhiteWellProtectedKing = 72,
    BlackWellProtectedKing = 73,
    WhitePoorlyPlacedKing = 74,
    BlackPoorlyPlacedKing = 75,
    WhiteWellPlacedKing = 76,
    BlackWellPlacedKing = 77,
    WhiteVeryWeakPawnStructure = 78,
    BlackVeryWeakPawnStructure = 79,
    WhiteModeratelyWeakPawnStructure = 80,
    BlackModeratelyWeakPawnStructure = 81,
    WhiteModeratelyStrongPawnStructure = 82,
    BlackModeratelyStrongPawnStructure = 83,
    WhiteVeryStrongPawnStructure = 84,
    BlackVeryStrongPawnStructure = 85,
    WhitePoorKnightPlacement = 86,
    BlackPoorKnightPlacement = 87,
    WhiteGoodKnightPlacement = 88,
    BlackGoodKnightPlacement = 89,
    WhitePoorBishopPlacement = 90,
    BlackPoorBishopPlacement = 91,
    WhiteGoodBishopPlacement = 92,
    BlackGoodBishopPlacement = 93,
    WhitePoorRookPlacement = 94,
    BlackPoorRookPlacement = 95,
    WhiteGoodRookPlacement = 96,
    BlackGoodRookPlacement = 97,
    WhitePoorQueenPlacement = 98,
    BlackPoorQueenPlacement = 99,
    WhiteGoodQueenPlacement = 100,
    BlackGoodQueenPlacement = 101,
    WhitePoorPieceCoordination = 102,
    BlackPoorPieceCoordination = 103,
    WhiteGoodPieceCoordination = 104,
    BlackGoodPieceCoordination = 105,
    WhitePlayedOpeningVeryPoorly = 106,
    BlackPlayedOpeningVeryPoorly = 107,
    WhitePlayedOpeningPoorly = 108,
    BlackPlayedOpeningPoorly = 109,
    WhitePlayedOpeningWell = 110,
    BlackPlayedOpeningWell = 111,
    WhitePlayedOpeningVeryWell = 112,
    BlackPlayedOpeningVeryWell = 113,
    WhitePlayedMiddlegameVeryPoorly = 114,
    BlackPlayedMiddlegameVeryPoorly = 115,
    WhitePlayedMiddlegamePoorly = 116,
    BlackPlayedMiddlegamePoorly = 117,
    WhitePlayedMiddlegameWell = 118,
    BlackPlayedMiddlegameWell = 119,
    WhitePlayedMiddlegameVeryWell = 120,
    BlackPlayedMiddlegameVeryWell = 121,
    WhitePlayedEndingVeryPoorly = 122,
    BlackPlayedEndingVeryPoorly = 123,
    WhitePlayedEndingPoorly = 124,
    BlackPlayedEndingPoorly = 125,
    WhitePlayedEndingWell = 126,
    BlackPlayedEndingWell = 127,
    WhitePlayedEndingVeryWell = 128,
    BlackPlayedEndingVeryWell = 129,
    WhiteSlightCounterplay = 130,
    BlackSlightCounterplay = 131,
    WhiteModerateCounterplay = 132,
    BlackModerateCounterplay = 133,
    WhiteDecisiveCounterplay = 134,
    BlackDecisiveCounterplay = 135,
    WhiteModerateTimeControlPressure = 136,
    BlackModerateTimeControlPressure = 137,
    WhiteSevereTimeControlPressure = 138,
    BlackSevereTimeControlPressure = 139,
}

fn ws<'a, F: FnMut(&'a str) -> IResult<&'a str, O>, O>(
    func: F,
) -> impl FnMut(&'a str) -> IResult<&str, O> {
    delimited(whitespace, func, whitespace)
}

#[derive(Clone, Debug)]
pub enum Termination {
    WhiteWins,
    BlackWins,
    Tie,
    Unknown,
}

pub fn parse_termination(input: &str) -> IResult<&str, Termination> {
    alt((
        value(Termination::WhiteWins, tag("1-0")),
        value(Termination::BlackWins, tag("0-1")),
        value(Termination::Tie, tag("1/2-1/2")),
        value(Termination::Unknown, tag("*")),
    ))(input)
}

pub fn parse_suffix(input: &str) -> IResult<&str, NAG> {
    alt((
        value(NAG::VeryGoodMoveTraditional, tag("!!")),
        value(NAG::VeryPoorMoveTraditional, tag("??")),
        value(NAG::SpeculativeMoveTraditional, tag("!?")),
        value(NAG::QuestionableMoveTraditional, tag("?!")),
        value(NAG::GoodMoveTraditional, tag("!")),
        value(NAG::PoorMoveTraditional, tag("?")),
    ))(input)
}
