use chess::{Board, ChessMove, File, Piece, Rank, Square};

use super::Filter;

pub struct Knight {}

impl Filter for Knight {
    fn get_raw_id() -> u8 {
        1
    }

    fn get_square_data(to: &Square, position: &Board) -> Vec<Option<Square>> {
        let mut square_data: Vec<Option<Square>> = vec![None; 8];

        for (index, square) in [
            (2i32, 1i32),
            (2, -1),
            (-2, 1),
            (-2, -1),
            (1, 2),
            (1, -2),
            (-1, 2),
            (-1, -2),
        ]
        .iter()
        .filter(|(file, rank)| {
            (0..8).contains(&(to.get_file().to_index() as i32 + file))
                && (0..8).contains(&(to.get_rank().to_index() as i32 + rank))
        })
        .map(|(file, rank)| {
            Square::make_square(
                Rank::from_index((to.get_rank().to_index() as i32 + rank) as usize),
                File::from_index((to.get_file().to_index() as i32 + file) as usize),
            )
        })
        .enumerate()
        {
            if matches!(position.piece_on(square), Some(Piece::Knight))
                && position.legal(ChessMove::new(square, *to, None))
            {
                square_data[index] = Some(square);
            }
        }

        square_data
    }

    fn get_overflow(to: &Square, from: &Square, position: &Board) -> Option<(u8, u8)> {
        // the number of the overflow bits
        let square_data = Self::get_square_data(to, position);

        if square_data.iter().filter(|square| square.is_some()).count() == 1 {
            None
        } else {
            Some((
                square_data
                    .iter()
                    .position(|square| {
                        if let Some(square) = square {
                            square == from
                        } else {
                            false
                        }
                    })
                    .expect("Could not find original square") as u8,
                3,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use chess::{Board, BoardBuilder, Color::*, Piece::*, Square};

    use crate::filters::{knight::Knight, Filter};

    #[test]
    fn square_data_test() {
        let mut setup = BoardBuilder::new();
        setup.piece(Square::A1, King, White);
        setup.piece(Square::H8, King, Black);

        setup.piece(Square::B5, Knight, White);
        setup.piece(Square::C2, Knight, White);
        setup.piece(Square::E6, Knight, White);
        setup.piece(Square::F5, Knight, White);
        setup.piece(Square::F3, Knight, White);

        let chess = Board::try_from(setup).unwrap();

        assert_eq!(
            Knight::get_square_data(&Square::D4, &chess),
            vec![
                Some(Square::F5),
                Some(Square::F3),
                Some(Square::B5),
                None,
                Some(Square::E6),
                None,
                None,
                Some(Square::C2)
            ]
        );
    }
}
