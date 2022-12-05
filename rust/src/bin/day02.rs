use std::fs;

#[derive(Debug, Clone)]
enum Shape {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

#[derive(Debug, Clone)]
enum Winner {
    Left = 0,
    Right = 6,
    Draw = 3,
}

type Code = String;
type Pair = (Code, Code);
type Round = (Shape, Shape);

fn get_left_shape(code: Code) -> Option<Shape> {
    return match code.as_str() {
        "A" => Some(Shape::Rock),
        "B" => Some(Shape::Paper),
        "C" => Some(Shape::Scissors),
        _ => None,
    };
}

fn get_right_shape(code: Code) -> Option<Shape> {
    return match code.as_str() {
        "X" => Some(Shape::Rock),
        "Y" => Some(Shape::Paper),
        "Z" => Some(Shape::Scissors),
        _ => None,
    };
}

// get (shape, shape) for given pair of codes
fn get_round((l, r): Pair) -> Option<Round> {
    return match (get_left_shape(l), get_right_shape(r)) {
        (Some(left), Some(right)) => Some((left, right)),
        _ => None,
    };
}

// get round to produce given winner against given left shape
fn get_round_for(left: Shape, winner: Winner) -> Round {
    let right = match (left.clone(), winner) {
        // Left
        (Shape::Rock, Winner::Left) => Shape::Scissors,
        (Shape::Paper, Winner::Left) => Shape::Rock,
        (Shape::Scissors, Winner::Left) => Shape::Paper,
        // Right
        (Shape::Rock, Winner::Right) => Shape::Paper,
        (Shape::Paper, Winner::Right) => Shape::Scissors,
        (Shape::Scissors, Winner::Right) => Shape::Rock,
        // Draw
        (Shape::Rock, Winner::Draw) => Shape::Rock,
        (Shape::Paper, Winner::Draw) => Shape::Paper,
        (Shape::Scissors, Winner::Draw) => Shape::Scissors,
    };

    return (left, right);
}

fn get_winner(round: Round) -> Winner {
    match round {
        // Left
        (Shape::Rock, Shape::Scissors) => Winner::Left,
        (Shape::Paper, Shape::Rock) => Winner::Left,
        (Shape::Scissors, Shape::Paper) => Winner::Left,
        // Right
        (Shape::Rock, Shape::Paper) => Winner::Right,
        (Shape::Paper, Shape::Scissors) => Winner::Right,
        (Shape::Scissors, Shape::Rock) => Winner::Right,
        // Draw
        (Shape::Rock, Shape::Rock) => Winner::Draw,
        (Shape::Paper, Shape::Paper) => Winner::Draw,
        (Shape::Scissors, Shape::Scissors) => Winner::Draw,
    }
}

// The score for a single round is
//
//   1. The value for the shape you selected
//   2. Plus the score for the outcome of the round
//     - 0 if you lost
//     - 3 if the round was a draw
//     - 6 if you won
//
fn get_points((left, right): Round) -> u32 {
    let shape_points = right as u32;
    let round_points = get_winner((left, right)) as u32;
    return shape_points + round_points;
}

fn main() {
    let input_filename = "../data/input02.txt";

    // vec of lines from file
    let lines: Vec<String> = fs
        ::read_to_string(input_filename)
        .expect("Failed to read input")
        .trim()
        .split("\n")
        .map(String::from)
        .collect();

    // vec of (string, string) pairs of codes
    let mut pairs: Vec<Pair> = Vec::new();
    for line in lines.iter() {
        let mut moves: Vec<String> = line.to_string().split(" ").map(String::from).collect();

        if moves.len() >= 2 {
            let left = moves.remove(0);
            let right = moves.remove(0);
            pairs.push((left, right));
        }
    }

    // vec of (shape, shape) pairs
    let mut rounds: Vec<Round> = Vec::new();
    for (l, r) in pairs.iter() {
        if let Some(round) = get_round((l.to_string(), r.to_string())) {
            rounds.push(round);
        }
    }

    // calculate points
    let mut points: Vec<u32> = Vec::new();
    for (left, right) in rounds {
        let round_points = get_points((left, right));
        points.push(round_points);
    }

    let total: u32 = points.iter().sum();

    // Round Two

    // the second column says how the round needs to end:
    //   - X means you need to lose
    //   - Y means you need to end the round in a draw
    //   - Z means you need to win
    let mut rounds2: Vec<Round> = Vec::new();
    for (l, r) in pairs.iter() {
        let left = get_left_shape(l.to_string()).unwrap();

        #[allow(unused_parens)]
        if
            let Some(desired_winner) = (match r.as_str() {
                "X" => Some(Winner::Left),
                "Y" => Some(Winner::Draw),
                "Z" => Some(Winner::Right),
                _ => None,
            })
        {
            let round = get_round_for(left, desired_winner);
            rounds2.push(round);
        }
    }

    // calculate points
    let mut points2: Vec<u32> = Vec::new();
    for (left, right) in rounds2 {
        let round_points = get_points((left, right));
        points2.push(round_points);
    }

    let total2: u32 = points2.iter().sum();

    println!("{:?}, {:?}", total, total2);
}
