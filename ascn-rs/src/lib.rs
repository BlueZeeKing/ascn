use chess::Piece;

mod bitbuffer;
mod filters;
pub mod outcome;
pub mod reader;
pub mod writer;

const PROMOTION_KEY: [Piece; 4] = [Piece::Queen, Piece::Bishop, Piece::Rook, Piece::Knight];
