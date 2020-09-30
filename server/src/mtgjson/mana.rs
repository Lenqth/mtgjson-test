use nom::error::VerboseError;
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};
use std::fmt::Display;

#[derive(Serialize, Deserialize, Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum Color {
    W,
    U,
    B,
    R,
    G,
}
impl ToString for Color {
    fn to_string(&self) -> String {
        (match self {
            Self::W => "W",
            Self::U => "U",
            Self::B => "B",
            Self::R => "R",
            Self::G => "G",
        })
        .to_string()
    }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum ManaSymbolAtom {
    Number(i32),
    Colored(Color),
    Colorless,
    Phyrexian,
    Snow,
    X,
    Y,
    Z,
}

impl ToString for ManaSymbolAtom {
    fn to_string(&self) -> String {
        match self {
            Self::Number(a) => format!("({})", a.to_string()),
            Self::Colored(a) => format!("({})", a.to_string()),
            Self::Colorless => "C".to_string(),
            Self::Phyrexian => "P".to_string(),
            Self::Snow => "S".to_string(),
            Self::X => "X".to_string(),
            Self::Y => "Y".to_string(),
            Self::Z => "Z".to_string(),
        }
    }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum ManaSymbol {
    Atom(ManaSymbolAtom),
    Hybrid(ManaSymbolAtom, ManaSymbolAtom),
    Half(ManaSymbolAtom),
}

impl ToString for ManaSymbol {
    fn to_string(&self) -> String {
        match self {
            Self::Atom(a) => format!("({})", a.to_string()),
            Self::Hybrid(a, b) => format!("({}/{})", a.to_string(), b.to_string()),
            Self::Half(a) => format!("(H{})", a.to_string()),
        }
    }
}

impl From<ManaSymbolAtom> for ManaSymbol {
    fn from(s: ManaSymbolAtom) -> Self {
        Self::Atom(s)
    }
}

#[derive(Debug, Clone)]
pub struct ManaCost {
    value: Vec<ManaSymbol>,
}

impl ManaCost {
    pub fn new(s: Vec<ManaSymbol>) -> Self {
        Self { value: s }
    }
}
impl From<Vec<ManaSymbol>> for ManaCost {
    fn from(s: Vec<ManaSymbol>) -> Self {
        Self { value: s }
    }
}
impl From<ManaCost> for Vec<ManaSymbol> {
    fn from(s: ManaCost) -> Self {
        s.value
    }
}

impl PartialEq for ManaCost {
    fn eq(&self, other: &Self) -> bool {
        self.value.clone().sort() == other.value.clone().sort()
    }
}
impl Eq for ManaCost {}

impl Serialize for ManaCost {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let res: String = self
            .value
            .iter()
            .map(|ms| ms.to_string())
            .collect::<Vec<_>>()
            .join("");
        serializer.serialize_str(&res)
    }
}
impl<'de> Deserialize<'de> for ManaCost {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: String = Deserialize::deserialize(deserializer)?;
        Ok(Self::new(symbols::<VerboseError<&str>>(&s).unwrap().1))
    }
}

use nom::IResult;
fn inner_symbol<'a, E: nom::error::ParseError<&'a str>>(
    s: &'a str,
) -> IResult<&'a str, ManaSymbol, E> {
    use nom::branch::{alt, permutation};
    use nom::bytes::complete::tag;
    use nom::character::complete::{char, digit1, multispace0, one_of};
    use nom::combinator::{all_consuming, map, map_res};
    use nom::error::ErrorKind;
    use nom::multi::{count, many0};
    use nom::sequence::{delimited, separated_pair};

    let symbol_chars = |s| {
        map(one_of("WUBRGCSPXYZ"), |r| match r {
            'W' => (ManaSymbolAtom::Colored(Color::W)),
            'U' => (ManaSymbolAtom::Colored(Color::U)),
            'B' => (ManaSymbolAtom::Colored(Color::B)),
            'R' => (ManaSymbolAtom::Colored(Color::R)),
            'G' => (ManaSymbolAtom::Colored(Color::G)),
            'C' => (ManaSymbolAtom::Colorless),
            'S' => (ManaSymbolAtom::Snow),
            'X' => (ManaSymbolAtom::X),
            'Y' => (ManaSymbolAtom::Y),
            'Z' => (ManaSymbolAtom::Z),
            'P' => (ManaSymbolAtom::Phyrexian),
            _ => panic!(),
        })(s)
    };
    let symbol_number = |s| {
        map_res(digit1, |r: &str| {
            Ok::<ManaSymbolAtom, nom::Err<(&str, ErrorKind)>>(ManaSymbolAtom::Number(
                r.parse::<i32>()
                    .or_else(|e| Err(nom::Err::Error((r, ErrorKind::Digit))))?,
            ))
        })(s)
    };
    let atom_symbols = |s| alt((symbol_number, symbol_chars))(s);
    let res = alt((
        map(
            separated_pair(atom_symbols, tag("/"), atom_symbols),
            |(r1, r2)| ManaSymbol::Hybrid(r1, r2),
        ),
        map(atom_symbols, ManaSymbol::Atom),
        map(
            delimited(char('H'), atom_symbols, |s| Ok((s, 0))),
            ManaSymbol::Half,
        ),
    ))(s);
    res.map(|r| r.clone())
}
fn symbols<'a, E: nom::error::ParseError<&'a str>>(
    s: &'a str,
) -> IResult<&'a str, Vec<ManaSymbol>, E> {
    use nom::branch::{alt, permutation};
    use nom::bytes::complete::tag;
    use nom::character::complete::{char, digit1, multispace0, one_of};
    use nom::combinator::{all_consuming, map, map_res};
    use nom::error::ErrorKind;
    use nom::multi::{count, many0};
    use nom::sequence::{delimited, separated_pair};
    let parser = |s| {
        all_consuming(many0(|s| {
            let (s, open_brace) = one_of("({")(s)?;
            let close_brace = match open_brace {
                '(' => ')',
                '{' => '}',
                _ => panic!(),
            };
            let (s, sym) = delimited(multispace0, inner_symbol, multispace0)(s)?;
            let (s, _) = char(close_brace)(s)?;
            Ok((s, sym))
        }))(s)
    };
    parser(s).map(|r| r.clone())
}

pub fn mana_parser(s: &str) -> ManaCost {
    ManaCost {
        value: symbols::<VerboseError<&str>>(s).expect("error").1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_error() {
        use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};

        assert_eq!(
            symbols::<VerboseError<&str>>("(3)("),
            Err(nom::Err::Error(VerboseError {
                errors: vec![("(", VerboseErrorKind::Nom(ErrorKind::Eof))]
            }))
        );

        assert_eq!(
            symbols::<VerboseError<&str>>("(3}"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![("(3}", VerboseErrorKind::Nom(ErrorKind::Eof))]
            }))
        );
        assert_eq!(
            symbols::<VerboseError<&str>>("(RR)"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![("(RR)", VerboseErrorKind::Nom(ErrorKind::Eof))]
            }))
        );
        assert_eq!(
            symbols::<VerboseError<&str>>("(3R)"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![("(3R)", VerboseErrorKind::Nom(ErrorKind::Eof))]
            }))
        );
    }
    #[test]
    fn test_mana_parser() {
        use Color::*;
        use ManaSymbol::*;
        use ManaSymbolAtom::*;
        assert_eq!(
            mana_parser("(3)(W)(U)"),
            ManaCost::new(vec![Atom(Number(3)), Atom(Colored(W)), Atom(Colored(B))])
        );

        assert_eq!(mana_parser("(3)"), ManaCost::new(vec![Atom(Number(3))]));

        assert_eq!(mana_parser("{HW}"), ManaCost::new(vec![Half(Colored(W))]));

        assert_eq!(
            mana_parser("{U/P}"),
            ManaCost::new(vec![Hybrid(Colored(U), Phyrexian)])
        );

        assert_eq!(
            mana_parser("(2/W)"),
            ManaCost::new(vec![Hybrid(Number(2), Colored(W))])
        );
        assert_eq!(mana_parser("(11)"), ManaCost::new(vec![Atom(Number(11))]));
        assert_eq!(mana_parser("(C)"), ManaCost::new(vec![Atom(Colorless)]));
        assert_eq!(mana_parser("(S)"), ManaCost::new(vec![Atom(Snow)]));
        assert_eq!(
            mana_parser("(W/B)"),
            ManaCost::new(vec![Hybrid(Colored(U), Colored(B))])
        );
        assert_eq!(
            mana_parser("{1000000}"),
            ManaCost::new(vec![Atom(Number(1000000))])
        );
    }
}
