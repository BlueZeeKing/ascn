use chess::{Board, ChessMove};

use crate::{
    bitbuffer::BitBuffer,
    filters::{diagonal::Diagonal, knight::Knight, straight::Straight, Filter},
    outcome::Outcome,
    PROMOTION_KEY,
};

#[derive(Clone)]
pub struct Writer {
    core: Vec<u8>,
    overflow: Vec<(u8, u8)>, // data, # of bits
}

impl Default for Writer {
    fn default() -> Self {
        Self::new()
    }
}

impl Writer {
    /// Creates a new empty instance of the `Writer`
    pub fn new() -> Self {
        Self {
            core: Vec::new(),
            overflow: Vec::new(),
        }
    }

    /// Consumes a mutable self and creates a vector of bytes that represents the data
    pub fn get_data(mut self, outcome: Option<Outcome>) -> Vec<u8> {
        self.core.push(outcome.unwrap_or_default().get_id());

        self.core
            .iter()
            .chain(Self::get_overflow_data(self.overflow).iter())
            .copied()
            .collect()
    }

    fn get_overflow_data(overflow: Vec<(u8, u8)>) -> Vec<u8> {
        let mut bit_buffer = BitBuffer::new();

        for (data, length) in overflow {
            bit_buffer.add(data, length);
        }

        bit_buffer.to_bytes()
    }

    /// Takes in a chess move and a position from shakmaty and adds them to the output
    ///
    /// # Arguments
    ///
    /// * `chess_move` - A valid move for the position inputted
    /// * `position` - The current chess position before the `chess_move` has been processed
    pub fn add_move(&mut self, chess_move: &ChessMove, position: &Board) {
        let to_square = chess_move.get_dest();
        let id: u8;
        let overflow: Option<(u8, u8)>; // data, num bits

        if chess_move
            .get_source()
            .get_rank()
            .to_index()
            .abs_diff(chess_move.get_dest().get_rank().to_index())
            == chess_move
                .get_source()
                .get_file()
                .to_index()
                .abs_diff(chess_move.get_dest().get_file().to_index())
        {
            id = Diagonal::get_id();
            overflow =
                Diagonal::get_overflow(&chess_move.get_dest(), &chess_move.get_source(), position);
        } else if chess_move.get_dest().get_rank() == chess_move.get_source().get_rank()
            || chess_move.get_dest().get_file() == chess_move.get_source().get_file()
        {
            id = Straight::get_id();
            overflow =
                Straight::get_overflow(&chess_move.get_dest(), &chess_move.get_source(), position);
        } else {
            id = Knight::get_id();
            overflow =
                Knight::get_overflow(&chess_move.get_dest(), &chess_move.get_source(), position);
        }

        self.core.push(to_square.to_int() | id);

        if let Some(data) = overflow {
            self.overflow.push(data);
        }

        if let Some(promotion) = chess_move.get_promotion() {
            let promotion_index = PROMOTION_KEY
                .iter()
                .position(|role| *role == promotion)
                .expect("Not a valid promotion piece") as u8;

            self.overflow.push((promotion_index, 2))
        }
    }
}
