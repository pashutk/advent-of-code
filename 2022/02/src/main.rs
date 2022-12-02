const INPUT_STR: &str = include_str!("../input.txt");

#[derive(Copy, Clone)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

impl Move {
    fn score(&self) -> i32 {
        match self {
            Move::Rock => 1,
            Move::Paper => 2,
            Move::Scissors => 3,
        }
    }

    fn from_char(ch: char) -> Option<Move> {
        match ch {
            'A' | 'X' => Some(Move::Rock),
            'B' | 'Y' => Some(Move::Paper),
            'C' | 'Z' => Some(Move::Scissors),
            _ => None,
        }
    }
}

enum Outcome {
    Lose,
    Draw,
    Win,
}

impl Outcome {
    fn score(&self) -> i32 {
        match self {
            Outcome::Lose => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }

    fn from_moves(me: &Move, opponent: &Move) -> Self {
        match (me, opponent) {
            (Move::Rock, Move::Paper)
            | (Move::Paper, Move::Scissors)
            | (Move::Scissors, Move::Rock) => Outcome::Lose,

            (Move::Rock, Move::Rock)
            | (Move::Paper, Move::Paper)
            | (Move::Scissors, Move::Scissors) => Outcome::Draw,

            (Move::Rock, Move::Scissors)
            | (Move::Paper, Move::Rock)
            | (Move::Scissors, Move::Paper) => Outcome::Win,
        }
    }

    fn from_char(ch: char) -> Option<Self> {
        match ch {
            'X' => Some(Self::Lose),
            'Y' => Some(Self::Draw),
            'Z' => Some(Self::Win),
            _ => None,
        }
    }

    fn others_move(&self, opponent_move: &Move) -> Move {
        match self {
            Outcome::Lose => match opponent_move {
                Move::Rock => Move::Scissors,
                Move::Paper => Move::Rock,
                Move::Scissors => Move::Paper,
            },
            Outcome::Draw => *opponent_move,
            Outcome::Win => match opponent_move {
                Move::Rock => Move::Paper,
                Move::Paper => Move::Scissors,
                Move::Scissors => Move::Rock,
            },
        }
    }
}

fn first() -> i32 {
    INPUT_STR
        .split("\n")
        .map(|round| {
            let opponent_move_char = round.chars().nth(0).expect("Can't read opponent move");
            let my_move_char = round.chars().nth(2).expect("Can't read my move");

            let opponent_move =
                Move::from_char(opponent_move_char).expect("Unexpected opponent move");
            let my_move = Move::from_char(my_move_char).expect("Unexpected my move");

            my_move.score() + Outcome::from_moves(&my_move, &opponent_move).score()
        })
        .sum()
}

fn second() -> i32 {
    INPUT_STR
        .split("\n")
        .map(|round| {
            let opponent_move_char = round.chars().nth(0).expect("Can't read opponent move");
            let outcome_char = round.chars().nth(2).expect("Can't read outcome");

            let opponent_move =
                Move::from_char(opponent_move_char).expect("Unexpected opponent move");
            let outcome = Outcome::from_char(outcome_char).expect("Unexpected outcome");

            let my_move = outcome.others_move(&opponent_move);

            my_move.score() + outcome.score()
        })
        .sum()
}

fn main() {
    println!("First: {}", first());
    println!("Second: {}", second());
}
