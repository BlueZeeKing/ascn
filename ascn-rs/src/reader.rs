use chess::{Board, ChessMove, Piece, Square};

use crate::{
    bitbuffer::BitBuffer,
    filters::{diagonal::Diagonal, knight::Knight, straight::Straight, Filter},
    outcome::Outcome,
    PROMOTION_KEY,
};

#[derive(Clone)]
pub struct Reader {
    data: Vec<u8>,
    chess: Board,
    bit_buffer: BitBuffer,
    outcome: Option<Outcome>,
}

impl Reader {
    /// Creates a new reader based on the buffer provided
    /// It also creates a new default chess board
    pub fn new(data: &[u8]) -> Self {
        Reader {
            data: data.to_vec(),
            chess: Board::default(),
            bit_buffer: BitBuffer::from_bytes(data),
            outcome: None,
        }
    }

    pub fn get_outcome(&self) -> &Option<Outcome> {
        &self.outcome
    }
}

fn safe_get_square(index: u8) -> Square {
    assert!((0..64).contains(&index));

    unsafe { Square::new(index) }
}

impl Iterator for Reader {
    /// Returns the next move processed and the current state of the board after the move has been applied
    fn next(&mut self) -> Option<Self::Item> {
        let to = safe_get_square(self.data[0] & 0b00111111);
        let id = self.data[0] >> 6;

        let (square_data, overflow_length) = match id {
            3 => (Straight::get_square_data(&to, &self.chess), 2),
            2 => (Diagonal::get_square_data(&to, &self.chess), 2),
            1 => (Knight::get_square_data(&to, &self.chess), 3),
            0 => {
                self.outcome = Some(Outcome::from_id(self.data[0]));
                return None;
            }
            _ => panic!("Unknown filter"),
        };

        self.data.remove(0);

        let index = if square_data.iter().filter(|square| square.is_some()).count() == 1 {
            square_data
                .iter()
                .position(|square| square.is_some())
                .expect("Could not find previously found valid move (radioactive particle?)")
        } else {
            self.bit_buffer.read(overflow_length) as usize
        };

        let from = square_data[index].expect("Could not find valid move from overflow index");

        let from_piece = self
            .chess
            .piece_on(from)
            .expect("Could not find piece at previously validated square (radioactive particle?)");

        let chess_move = ChessMove::new(
            from,
            to,
            if from_piece == Piece::Pawn
                && self.chess.color_on(from).unwrap().to_their_backrank() == to.get_rank()
            {
                Some(PROMOTION_KEY[self.bit_buffer.read(2) as usize])
            } else {
                None
            },
        );

        self.chess.clone().make_move(chess_move, &mut self.chess);

        Some((chess_move, self.chess))
    }

    type Item = (ChessMove, Board);
}
