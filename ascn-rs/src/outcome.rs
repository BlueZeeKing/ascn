#[derive(Clone, Debug, PartialEq)]
pub enum Outcome {
    WhiteWon,
    BlackWon,
    Draw,
    Unknown,
}

impl Outcome {
    pub fn get_id(&self) -> u8 {
        match self {
            Outcome::WhiteWon => 1,
            Outcome::BlackWon => 2,
            Outcome::Draw => 3,
            Outcome::Unknown => 0,
        }
    }

    pub fn from_id(id: u8) -> Self {
        match id {
            1 => Outcome::WhiteWon,
            2 => Outcome::BlackWon,
            3 => Outcome::Draw,
            0 => Outcome::Unknown,
            _ => panic!("Unknown ID"), // TODO: Error handling
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            Outcome::WhiteWon => "1-0",
            Outcome::BlackWon => "0-1",
            Outcome::Draw => "1/2-1/2",
            Outcome::Unknown => "*",
        }
    }

    pub fn from_string(str: &str) -> Self {
        match str {
            "1-0" => Outcome::WhiteWon,
            "0-1" => Outcome::BlackWon,
            "1/2-1/2" => Outcome::Draw,
            "*" => Outcome::Unknown,
            _ => panic!("Unknown outcome"), // TODO: Error handling
        }
    }
}

impl Default for Outcome {
    fn default() -> Self {
        Self::Unknown
    }
}
