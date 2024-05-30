# ascn-rs

Assumed Standard Chess Notation is a format for encoding the entire history of
a chess game in as small a space as possible in reasonable time. This algorithm
stores each move in about a byte and occasionally overflows to handle edge
cases. Made for the AP Computer Science Principles Create Task in 2023.

## Dependencies

This project uses [chess](https://crates.io/crates/ches) and
pgn-rs

This was originally 3 repos that I combined
