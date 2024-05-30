use ascn_rs::{outcome::Outcome, reader::Reader, writer::Writer};
use chess::{BitBoard, Board, BoardStatus, ChessMove, Color, MoveGen, Piece, Square};
use clap::Parser;
use pgn_rs::{
    parse::{Castle, Termination, SAN},
    Visitor,
};
use std::{
    fs::{read_to_string, File},
    io::{Read, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
struct Arguments {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
}

#[derive(Debug, PartialEq)]
enum Format {
    Ascn,
    Pgn,
}

impl Format {
    pub fn opposite(&self) -> Self {
        match self {
            Self::Ascn => Self::Pgn,
            Self::Pgn => Self::Ascn,
        }
    }

    pub fn get_extension(&self) -> &'static str {
        match self {
            Self::Ascn => "ascn",
            Self::Pgn => "pgn",
        }
    }
}

struct PGNVisitor {
    board: Board,
    writer: Writer,
    outcome: Option<Outcome>,
}

impl PGNVisitor {
    fn new() -> Self {
        Self {
            board: Board::default(),
            writer: Writer::new(),
            outcome: None,
        }
    }
}

impl Visitor for PGNVisitor {
    fn tag_pair(&mut self, _pair: pgn_rs::parse::TagPair) {}

    fn partial_move(&mut self, mv: pgn_rs::parse::PartialMove) {
        let mv = match mv.san {
            SAN::Castle(san) => ChessMove::new(
                self.board.king_square(self.board.side_to_move()),
                Square::make_square(
                    self.board.side_to_move().to_my_backrank(),
                    if Castle::KingSide == san.castle {
                        chess::File::G
                    } else {
                        chess::File::C
                    },
                ),
                None,
            ),
            SAN::Basic(san) => {
                MoveGen::new_legal(&self.board).find(|mv| {
                    self.board.piece_on(mv.get_source()).unwrap() == san.piece &&
                    mv.get_dest() == san.square &&
                    mv.get_promotion() == san.promotion &&
                    !matches!(san.starting_file, Some(file) if mv.get_source().get_file() != file) &&
                    !matches!(san.starting_rank, Some(rank) if mv.get_source().get_rank() != rank)
                }).expect("Could not find move")
            }
        };

        println!("src: {} dest: {}", mv.get_source(), mv.get_dest());

        self.writer.add_move(&mv, &self.board);
        self.board = self.board.make_move_new(mv);
        println!("{}", self.board);
    }

    fn termination(&mut self, term: pgn_rs::parse::Termination) {
        let outcome = term_to_outcome(term);

        let mut reader = Reader::new(&self.writer.clone().get_data(Some(outcome.clone())));

        let (_, board) = reader.clone().last().unwrap();
        assert_eq!(self.board, board);

        #[allow(clippy::while_let_on_iterator)] // consumes without taking ownership
        while let Some(_) = reader.next() {}

        assert_eq!(reader.get_outcome(), &Some(outcome));
    }
}

fn term_to_outcome(term: Termination) -> Outcome {
    match term {
        Termination::WhiteWins => Outcome::WhiteWon,
        Termination::BlackWins => Outcome::BlackWon,
        Termination::Tie => Outcome::Draw,
        Termination::Unknown => Outcome::Unknown,
    }
}

impl PGNVisitor {
    fn start_game(&mut self) {
        self.board = Board::default();
        self.writer = Writer::new();
        self.outcome = None
    }
}

fn main() {
    let args = Arguments::parse();

    let input_format = match args
        .input
        .extension()
        .expect("Could not determine input file type")
        .to_str()
        .expect("Could not parse extension")
    {
        "ascn" => Format::Ascn,
        "pgn" => Format::Pgn,
        _ => panic!("Unknown input file type"),
    };

    let mut output_file = File::create(
        args.output.unwrap_or(
            args.input
                .with_extension(input_format.opposite().get_extension()),
        ),
    )
    .expect("Could not create output file");

    if input_format == Format::Pgn {
        let data = read_to_string(args.input).expect("Could not find/read input file");

        let mut visitor = PGNVisitor::new();
        visitor.start_game();
        pgn_rs::parse(&data, &mut visitor).unwrap();

        output_file
            .write_all(&visitor.writer.get_data(visitor.outcome))
            .expect("Could not write to the output file");
        output_file
            .flush()
            .expect("Could not write to the output file");
    } else {
        let mut movetext = String::new();

        let mut input_file = File::open(args.input).expect("Could not find input file");
        let mut input_buf = Vec::new();
        input_file
            .read_to_end(&mut input_buf)
            .expect("Could not read input file");

        let reader = Reader::new(&input_buf);

        let outcome = reader
            .get_outcome()
            .as_ref()
            .unwrap_or(&Outcome::Unknown)
            .to_string();

        let mut current_move = 1u32;
        let mut board = Board::default();

        for (chess_move, new_board) in reader {
            // dbg!(chess_move.get_dest().to_string());
            let san = get_san(chess_move, &board);

            if board.side_to_move() == Color::White {
                movetext += &format!("{}. {} ", current_move, san);
                current_move += 1;
            } else {
                movetext += &format!("{} ", san)
            }

            board = new_board;
        }

        write!(
            output_file,
            "[Result \"{}\"]\n\n{}{}",
            outcome, movetext, outcome
        )
        .expect("Could not write to the output file");
        output_file
            .flush()
            .expect("Could not write to the output file");
    }
}

fn get_piece(piece: Piece) -> &'static str {
    match piece {
        Piece::Pawn => "P",
        Piece::Knight => "N",
        Piece::Bishop => "B",
        Piece::Rook => "R",
        Piece::Queen => "Q",
        Piece::King => "K",
    }
}

fn get_san(chess_move: ChessMove, board: &Board) -> String {
    let new_board = board.make_move_new(chess_move);

    let mut result = get_piece(board.piece_on(chess_move.get_source()).unwrap()).to_string()
        + &chess_move.get_source().to_string();

    if board.piece_on(chess_move.get_dest()).is_some() {
        result += "x"
    }

    result += &chess_move.get_dest().to_string();

    if let Some(promotion) = chess_move.get_promotion() {
        result += "=";
        result += get_piece(promotion);
    }

    if new_board.status() == BoardStatus::Checkmate {
        result += "#"
    } else if new_board.checkers() != &BitBoard(0) {
        result += "+"
    }

    result
}
