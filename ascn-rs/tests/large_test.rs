use std::fs::read_to_string;

use ascn_rs::{outcome::Outcome, reader::Reader, writer::Writer};
use chess::{Board, ChessMove, File, MoveGen, Square};
use indicatif::ProgressBar;
use pgn_rs::{
    parse::{Castle, Termination, SAN},
    Visitor,
};

const NUM_GAMES: u64 = 121332;

struct TestVisitor {
    board: Board,
    writer: Writer,
    progress_bar: ProgressBar,
    outcome: Option<Outcome>,
}

impl TestVisitor {
    fn new() -> Self {
        Self {
            board: Board::default(),
            writer: Writer::new(),
            progress_bar: ProgressBar::new(NUM_GAMES),
            outcome: None,
        }
    }
}

impl Visitor for TestVisitor {
    fn tag_pair(&mut self, _pair: pgn_rs::parse::TagPair) {}

    fn partial_move(&mut self, mv: pgn_rs::parse::PartialMove) {
        let mv = match mv.san {
            SAN::Castle(san) => ChessMove::new(
                self.board.king_square(self.board.side_to_move()),
                Square::make_square(
                    self.board.side_to_move().to_my_backrank(),
                    if Castle::KingSide == san.castle {
                        File::G
                    } else {
                        File::C
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
        let outcome = term_to_outcom(term);

        let mut reader = Reader::new(&self.writer.clone().get_data(Some(outcome.clone())));

        let (_, board) = reader.clone().last().unwrap();
        assert_eq!(self.board, board);

        #[allow(clippy::while_let_on_iterator)] // consumes without taking ownership
        while let Some(_) = reader.next() {}

        assert_eq!(reader.get_outcome(), &Some(outcome));

        self.progress_bar.inc(1);
    }
}

fn term_to_outcom(term: Termination) -> Outcome {
    match term {
        Termination::WhiteWins => Outcome::WhiteWon,
        Termination::BlackWins => Outcome::BlackWon,
        Termination::Tie => Outcome::Draw,
        Termination::Unknown => Outcome::Unknown,
    }
}

impl TestVisitor {
    fn start_game(&mut self) {
        self.board = Board::default();
        self.writer = Writer::new();
        self.outcome = None
    }
}

#[test]
#[ignore]
fn test() {
    let mut visitor = TestVisitor::new();

    let data = read_to_string("tests/lichess_test_collection.pgn").unwrap();
    let mut remaining = data.as_str();

    loop {
        visitor.start_game();
        let Ok((new_remaining, ())) = pgn_rs::parse(remaining, &mut visitor) else {
            break;
        };

        remaining = new_remaining;
    }
}
