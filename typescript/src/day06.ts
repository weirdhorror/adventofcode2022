import { readFileSync } from 'fs';

const inputFilename: string = '../data/input06.txt';
const inputData = readFileSync(inputFilename, 'utf8');
// const sampleData = 'mjqjpqmgbljsphdztnvjfqwrcgsmlb';

const allUnique = (str: string) => {
  let chars: any = {};
  for (let i = 0; i < str.length; i++) {
    const char = str[i];
    if (chars[char]) {
      return false;
    }
    chars[char] = true;
  }
  return true;
};

const findMarker = (data: string, seqLength: number) => {
  for (let i = seqLength; i < data.length; i++) {
    let chars = data.slice(i - seqLength, i);
    if (allUnique(chars)) {
      return i;
    }
  }
  return null;
};

console.log({
  'packetMarker at': findMarker(inputData, 4),
  'messageMarker at': findMarker(inputData, 14),
});
