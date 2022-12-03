import { readFileSync } from 'fs';
import { match, P } from 'ts-pattern';

const inputFilename: string = '../data/input02.txt';
const lines = readFileSync(inputFilename, 'utf8').split('\n');

type Shape = 1 | 2 | 3;
type StrategyKey = 'A' | 'B' | 'C' | 'X' | 'Y' | 'Z';

const Rock: Shape = 1;
const Paper: Shape = 2;
const Scissors: Shape = 3;

const Strategy: Record<StrategyKey | string, Shape> = {
  A: Rock,
  B: Paper,
  C: Scissors,
  X: Rock,
  Y: Paper,
  Z: Scissors,
};

type Left = 0;
type Right = 1;
type Draw = -1;

type Winner = Left | Right | Draw;
type Round = [Shape, Shape];

const getWinner = (round: Round): Winner =>
  match(round)
    // Draw
    .with([Rock, Rock], () => -1 as Draw)
    .with([Paper, Paper], () => -1 as Draw)
    .with([Scissors, Scissors], () => -1 as Draw)
    // Left wins
    .with([Rock, Scissors], () => 0 as Left)
    .with([Paper, Rock], () => 0 as Left)
    .with([Scissors, Paper], () => 0 as Left)
    // Right wins
    .with([Rock, Paper], () => 1 as Right)
    .with([Paper, Scissors], () => 1 as Right)
    .with([Scissors, Rock], () => 1 as Right)
    .exhaustive();

const getStrategyShape = (key: string): Shape | undefined => Strategy[key];

const rounds: Round[] = lines.reduce((acc, line) => {
  const [x, y] = line.split(' ');
  const [a, b] = [getStrategyShape(x), getStrategyShape(y)];

  if (a && b) {
    acc.push([a, b]);
  }

  return acc;
}, [] as Round[]);

// The score for a single round is
//
//   1. The value for the shape you selected
//   2. Plus the score for the outcome of the round
//     - 0 if you lost
//     - 3 if the round was a draw
//     - 6 if you won
//
const calcPoints = (rounds: Round[]) =>
  rounds.map(([left, right]) => {
    const winner = getWinner([left, right]);

    const points =
      right +
      match(winner)
        .with(0 as Left, () => 0)
        .with(1 as Right, () => 6)
        .with(-1 as Draw, () => 3)
        .exhaustive();

    return points;
  });

// The score for a single round is
//
//   1. The value for the shape you selected
//   2. Plus the score for the outcome of the round
//     - 0 if you lost
//     - 3 if the round was a draw
//     - 6 if you won
//
const points = calcPoints(rounds);
const totalScore = points.reduce((sum, n) => sum + n, 0);

console.log({
  'rounds[0..5]': rounds.slice(0, 5),
  'points[0...5]': points.slice(0, 5),
  totalScore,
});

// Part Two

// Get the Shape to produce given Winner against given Shape
const getShapeFor = (left: Shape, winner: Winner): Shape =>
  match([left, winner] as [Shape, Winner])
    // Draw
    .with([Rock, -1 as Draw], () => Rock)
    .with([Paper, -1 as Draw], () => Paper)
    .with([Scissors, -1 as Draw], () => Scissors)
    // Left wins
    .with([Rock, 0 as Left], () => Scissors)
    .with([Paper, 0 as Left], () => Rock)
    .with([Scissors, 0 as Left], () => Paper)
    // Right wins
    .with([Rock, 1 as Right], () => Paper)
    .with([Paper, 1 as Right], () => Scissors)
    .with([Scissors, 1 as Right], () => Rock)
    .exhaustive();

// the second column says how the round needs to end:
//   - X means you need to lose
//   - Y means you need to end the round in a draw
//   - Z means you need to win
const rounds2: Round[] = lines.reduce((acc, line) => {
  const [x, y] = line.split(' ');

  const left = getStrategyShape(x);

  if (left) {
    const desiredWinner: Winner = match(y)
      .with('X', () => 0 as Left)
      .with('Y', () => -1 as Draw)
      .with('Z', () => 1 as Right)
      .run();

    const right = getShapeFor(left, desiredWinner);
    acc.push([left, right]);
  }

  return acc;
}, [] as Round[]);

const points2 = calcPoints(rounds2);
const totalScore2 = points2.reduce((sum, n) => sum + n, 0);

console.log({
  'rounds2[0..5]': rounds2.slice(0, 5),
  'points2[0...5]': points2.slice(0, 5),
  totalScore2,
});
