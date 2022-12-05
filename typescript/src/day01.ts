import { readFileSync } from 'fs';

const inputFilename: string = '../data/input01.txt';
const lines = readFileSync(inputFilename, 'utf8').split('\n');

type Calorie = number;
type Elf = Calorie[];
type Elves = Elf[];
type Partition = [Elf, Elves];

const sum = (ns: number[]) => ns.reduce((sum, n) => sum + n, 0);

const parse = (lines: string[]): Elves =>
  lines.reduce(
    ([elf, elves], line): Partition => {
      if (line.trim() === '') {
        elf.length > 0 && elves.push(elf);
        return [[], elves];
      } else {
        const calories = parseInt(line);
        elf.push(calories);
        return [elf, elves];
      }
    },
    [[], []] as Partition
  )[1];

const elves: Elves = parse(lines);
const calories = elves.map(sum).sort((a, b) => b - a);

const [top] = calories;
const topThree = sum(calories.slice(0, 3));

console.log({ top, topThree });
