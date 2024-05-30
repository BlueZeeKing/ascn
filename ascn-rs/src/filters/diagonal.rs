use chess::{Board, ChessMove, Color, File, Piece, Rank, Square};

use super::Filter;

pub struct Diagonal {}

impl Filter for Diagonal {
    fn get_raw_id() -> u8 {
        2
    }

    fn get_square_data(to: &Square, position: &Board) -> Vec<Option<Square>> {
        let mut square_data: Vec<Option<Square>> = vec![None; 4];

        for square in (to.get_rank().to_index() + 1..8)
            .zip(to.get_file().to_index() + 1..8)
            .map(|(rank, file)| Square::make_square(Rank::from_index(rank), File::from_index(file)))
        {
            // up and right
            if diagonal_checks(0, &mut square_data, to, position, square) {
                break;
            }
        }

        for square in (0..to.get_rank().to_index())
            .rev()
            .zip(to.get_file().to_index() + 1..8)
            .map(|(rank, file)| Square::make_square(Rank::from_index(rank), File::from_index(file)))
        {
            // down and right
            if diagonal_checks(1, &mut square_data, to, position, square) {
                break;
            }
        }

        for square in (0..to.get_rank().to_index())
            .rev()
            .zip((0..to.get_file().to_index()).rev())
            .map(|(rank, file)| Square::make_square(Rank::from_index(rank), File::from_index(file)))
        {
            // right
            if diagonal_checks(2, &mut square_data, to, position, square) {
                break;
            }
        }

        for square in (to.get_rank().to_index() + 1..8)
            .zip((0..to.get_file().to_index()).rev())
            .map(|(rank, file)| Square::make_square(Rank::from_index(rank), File::from_index(file)))
        {
            // left
            if diagonal_checks(3, &mut square_data, to, position, square) {
                break;
            }
        }

        square_data
    }
}

fn diagonal_checks(
    index: usize,
    square_data: &mut [Option<Square>],
    to: &Square,
    position: &Board,
    square: Square,
) -> bool {
    let possible_piece = position.piece_on(square);

    let piece: Piece;

    if let Some(temp_piece) = possible_piece {
        piece = temp_piece;
    } else {
        return false;
    }

    if (piece == Piece::Bishop
        || piece == Piece::Queen
        || (piece == Piece::King
            && square
                .get_rank()
                .to_index()
                .abs_diff(to.get_rank().to_index())
                == 1))
        && position.legal(ChessMove::new(square, *to, None))
    {
        square_data[index] = Some(square);
        return true;
    }

    if piece == Piece::Pawn
        && square
            .get_rank()
            .to_index()
            .abs_diff(to.get_rank().to_index())
            == 1
    {
        let should_move_forward = position.color_on(square).unwrap() == Color::White;

        if (should_move_forward && square.get_rank() > to.get_rank())
            || (!should_move_forward && square.get_rank() < to.get_rank())
        {
            return true;
        }

        if (position.piece_on(*to).is_some()
            || matches!(position.en_passant(), Some(opponent_square) if matches!(to.backward(position.color_on(square).unwrap()), Some(to) if to == opponent_square)))
            && position.legal(ChessMove::new(
                square,
                *to,
                if to.get_rank() == position.color_on(square).unwrap().to_their_backrank() {
                    Some(Piece::Queen)
                } else {
                    None
                },
            ))
        {
            square_data[index] = Some(square);
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use chess::{Board, BoardBuilder, ChessMove, Color::*, Piece::*, Square};

    use crate::filters::{diagonal::Diagonal, Filter};

    #[test]
    fn square_data_test() {
        let mut setup = BoardBuilder::new();

        setup.piece(Square::D3, Pawn, White);
        setup.piece(Square::H1, Bishop, White);
        setup.piece(Square::G6, Queen, White);
        setup.piece(Square::D5, King, White);

        setup.piece(Square::A8, King, Black);

        setup.piece(Square::A2, Pawn, White);
        setup.piece(Square::B3, Pawn, Black);

        let chess = Board::try_from(setup).unwrap();

        assert_eq!(
            Diagonal::get_square_data(&Square::B3, &chess),
            vec![None, None, Some(Square::A2), None]
        );
        assert_eq!(
            Diagonal::get_square_data(&Square::E4, &chess),
            vec![Some(Square::G6), Some(Square::H1), None, Some(Square::D5)]
        );
    }

    #[test]
    fn ep_square_data_test() {
        let chess = Board::default()
            .make_move_new(ChessMove::new(Square::A2, Square::A4, None))
            .make_move_new(ChessMove::new(Square::H7, Square::H6, None))
            .make_move_new(ChessMove::new(Square::A4, Square::A5, None))
            .make_move_new(ChessMove::new(Square::B7, Square::B5, None));

        assert_eq!(
            Diagonal::get_square_data(&Square::B6, &chess),
            vec![None, None, Some(Square::A5), None]
        );
    }
}
