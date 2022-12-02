import { readFileSync } from 'fs';

// type A = Shape.Rock;
// type B = Shape.Paper;
// type C = Shape.Scissors;
// type X = Shape.Rock;
// type Y = Shape.Paper;
// type Z = Shape.Scissors;

// type Rock = { keys: ['A', 'X']; value: 1 };
// type Paper = { keys: ['B', 'Y']; value: 2 };
// type Scissors = { keys: ['C', 'Z']; value: 3 };

// type Shape = Rock | Paper | Scissors;
// const strategies = ['A', 'B', 'C', 'X', 'Y', 'Z'];

interface Rock {
  value: 1;
}

enum Shape {
  Rock = 1,
  Paper,
  Scissors,
}

enum Strategy {
  A = 'Rock',
  B = 'Paper',
  C = 'Scissors',
  X = 'Rock',
  Y = 'Paper',
  Z = 'Scissors',
  // B = Shape.Paper,
  // C = Shape.Scissors,
  // X = Shape.Rock,
  // Y = Shape.Paper,
  // Z = Shape.Scissors,
}

type StrategyKey = 'A' | 'B' | 'C' | 'X' | 'Y' | 'Z';

// type StrategyKey = 'A' | 'B' | 'C' | 'X' | 'Y' | 'Z';

const strategyShape = (key: StrategyKey): Shape => Shape[Strategy[key]];

// Rock defeats Scissors
// Scissors defeats Paper
// Paper defeats Rock

// const rounds: Shape[] = readFileSync('input.txt', 'utf8')
//   .split('\n')
//   .map((line) => {
//     const [x, y] = line.split(' ') as StrategyKey[];
//     return [strategyShape(x), strategyShape(x)];
//   });

console.log(strategyShape('X'));

// Shape.Rock;

// type Shape.Rock = 'A' | 'X';

// type Rock = 'A' | 'X';
// type Paper = 'B' | 'Y';
// type Scissors = 'C' | 'Z';
