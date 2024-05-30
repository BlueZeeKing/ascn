use chess::{Board, ChessMove, Color, File, Piece, Rank, Square};

use super::Filter;

pub struct Straight {}

impl Filter for Straight {
    fn get_raw_id() -> u8 {
        3
    }

    fn get_square_data(to: &Square, position: &Board) -> Vec<Option<Square>> {
        let mut square_data: Vec<Option<Square>> = vec![None; 4];

        for rank in to.get_rank().to_index() + 1..8 {
            // up
            if vertical_checks(0, &mut square_data, to, position, rank) {
                break;
            }
        }

        for rank in (0..to.get_rank().to_index()).rev() {
            // down
            if vertical_checks(2, &mut square_data, to, position, rank) {
                break;
            }
        }

        for file in to.get_file().to_index() + 1..8 {
            // right
            if horizontal_checks(1, &mut square_data, to, position, file) {
                break;
            }
        }

        for file in (0..to.get_file().to_index()).rev() {
            // left
            if horizontal_checks(3, &mut square_data, to, position, file) {
                break;
            }
        }

        square_data
    }
}

fn vertical_checks(
    index: usize,
    square_data: &mut [Option<Square>],
    to: &Square,
    position: &Board,
    rank: usize,
) -> bool {
    let rank = Rank::from_index(rank);
    let square = Square::make_square(rank, to.get_file());

    let possible_piece = position.piece_on(square);

    let piece: Piece;

    if let Some(temp_piece) = possible_piece {
        piece = temp_piece;
    } else {
        return false;
    }

    if (piece == Piece::Rook
        || piece == Piece::Queen
        || (piece == Piece::King && rank.to_index().abs_diff(to.get_rank().to_index()) == 1))
        && position.legal(ChessMove::new(square, *to, None))
    {
        square_data[index] = Some(square);
        return true;
    }

    if piece == Piece::Pawn {
        let should_move_forward = position.color_on(square).unwrap() == Color::White;

        if (should_move_forward && square.get_rank() > to.get_rank())
            || (!should_move_forward && square.get_rank() < to.get_rank())
        {
            return true;
        }

        if ((rank.to_index().abs_diff(to.get_rank().to_index()) == 2
            && rank == position.color_on(square).unwrap().to_second_rank())
            || rank.to_index().abs_diff(to.get_rank().to_index()) == 1)
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

fn horizontal_checks(
    index: usize,
    square_data: &mut [Option<Square>],
    to: &Square,
    position: &Board,
    file: usize,
) -> bool {
    let file = File::from_index(file);
    let square = Square::make_square(to.get_rank(), file);

    let possible_piece = position.piece_on(square);

    let piece: Piece;

    if let Some(temp_piece) = possible_piece {
        piece = temp_piece;
    } else {
        return false;
    }

    if (piece == Piece::Rook
        || piece == Piece::Queen
        || (piece == Piece::King && file.to_index().abs_diff(to.get_file().to_index()) == 1))
        && position.legal(ChessMove::new(square, *to, None))
    {
        square_data[index] = Some(square);
        return true;
    }

    if piece == Piece::King && file.to_index().abs_diff(to.get_file().to_index()) == 2 {
        let castles = position.castle_rights(position.color_on(square).unwrap());

        let is_king_size = to.get_file() == File::G;

        if (is_king_size && castles.has_kingside())
            || (!is_king_size && castles.has_queenside())
                && position.legal(ChessMove::new(square, *to, None))
        {
            square_data[index] = Some(square);
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use chess::{Board, BoardBuilder, ChessMove, Color::*, Piece::*, Square};

    use crate::filters::Filter;

    use super::Straight;

    #[test]
    fn simple_square_data_test() {
        let chess = Board::default();

        assert_eq!(
            Straight::get_square_data(&Square::A4, &chess),
            vec![None, None, Some(Square::A2), None]
        );
    }

    #[test]
    fn castling_square_data_test() {
        let chess = Board::default()
            .make_move_new(ChessMove::new(Square::D2, Square::D3, None))
            .make_move_new(ChessMove::new(Square::H7, Square::H6, None))
            .make_move_new(ChessMove::new(Square::C1, Square::D2, None))
            .make_move_new(ChessMove::new(Square::G7, Square::G6, None))
            .make_move_new(ChessMove::new(Square::B1, Square::C2, None))
            .make_move_new(ChessMove::new(Square::F7, Square::F6, None))
            .make_move_new(ChessMove::new(Square::E2, Square::E3, None))
            .make_move_new(ChessMove::new(Square::E7, Square::E6, None))
            .make_move_new(ChessMove::new(Square::D1, Square::E2, None))
            .make_move_new(ChessMove::new(Square::D7, Square::D6, None));

        assert_eq!(
            Straight::get_square_data(&Square::C1, &chess),
            vec![None, Some(Square::E1), None, Some(Square::A1)]
        );
    }

    #[test]
    fn complex_square_data_test() {
        let mut setup = BoardBuilder::new();
        setup.piece(Square::D4, Pawn, Black);
        setup.piece(Square::H4, Rook, White);
        setup.piece(Square::D1, Queen, White);
        setup.piece(Square::D5, King, White);
        setup.piece(Square::A8, King, Black);
        setup.piece(Square::A2, Pawn, White);

        let chess = Board::try_from(setup).unwrap();

        assert_eq!(
            Straight::get_square_data(&Square::D4, &chess),
            vec![Some(Square::D5), Some(Square::H4), Some(Square::D1), None]
        );
        assert_eq!(
            Straight::get_square_data(&Square::A4, &chess),
            vec![None, None, Some(Square::A2), None]
        );
    }
}
