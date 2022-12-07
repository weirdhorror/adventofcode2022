import { range } from 'lodash';

export type Crate = string;
export type Stack = Crate[];

export interface Move {
  move: number;
  from: number;
  to: number;
}

/*
Parse input data into stacks and moves with this shape:

  {
    stacks: [
      ['C', 'W', 'R', 'D', 'J'],
      ['Z', 'D'],
      ['F', 'H', 'C'],
      ...
    ],

    moves: [
      {move: 1, from: 8, to: 5},
      {move: 1, from: 5, to: 1},
      {move: 1, from: 0, to: 4},
      ...
    ]
  }

*/
export const parseData = (inputData: string) => {
  // list of stacks, list of moves
  const [stacksLines, moveLines] = inputData
    .split('\n\n')
    .map((x) => x.split('\n'));

  // pop stack_numbers from stacks list
  const stackNumbers = stacksLines.pop()!;

  // list of stack number string indexes
  let crateIndexes = [] as number[];
  for (let i = 0; i < stackNumbers.length; i++) {
    const char = stackNumbers[i];

    if (/\S+/.test(char)) {
      crateIndexes.push(i);
    }
  }

  // build list of stack crates
  let stacks: Stack[] = [];
  for (const index of crateIndexes) {
    let stack: Crate[] = [];
    for (const stackLine of stacksLines) {
      const crate: Crate = stackLine[index];
      if (/\S+/.test(crate)) {
        stack.unshift(crate);
      }
    }
    stacks.push(stack);
  }

  // build list of moves
  const re = /move (?<move>\d+) from (?<from>\d+) to (?<to>\d+)/;
  const moves = moveLines.flatMap((line) => {
    const { move, from, to } = line.match(re)!.groups!;

    // note: multi-crate moves are unfolded into separate single moves
    return range(parseInt(move)).map(
      (_n) =>
        ({
          move: 1,
          from: parseInt(from) - 1,
          to: parseInt(to) - 1,
        } as Move)
    );
  });

  return { stacks, moves };
};
