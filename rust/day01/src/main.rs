use std::fs;

type Calorie = u32;
type Elf = Vec<Calorie>;
type Elves = Vec<Elf>;

fn main() {
    let input_filename = "../../data/input01.txt";

    // get vector of lines from input file
    let lines: Vec<String> = fs
        ::read_to_string(input_filename)
        .expect("Failed to read input")
        .split("\n")
        .map(|line| line.to_string())
        .collect();

    // build nested vector of calories integers
    let mut elf_part: Elf = Vec::new();
    let mut elves: Elves = Vec::new();

    for line in lines.iter() {
        // empty line found, end of current elf calories
        if line == "" {
            if elf_part.len() > 0 {
                let elf = elf_part.to_vec();
                elves.push(elf);
            }
            elf_part.clear();
            continue;
        }

        if let Ok(calorie) = line.parse::<u32>() {
            elf_part.push(calorie);
        }
    }

    // build vector of calorie sums sorted descending
    let mut sums: Vec<Calorie> = Vec::new();

    for elf in elves.iter() {
        let sum: Calorie = elf.iter().sum();
        sums.push(sum);
    }

    sums.sort();
    sums.reverse();

    // TODO: can i do this more functionally:
    // let sums: Vec<Calorie> = elves.iter().map(|elf: Elf| {
    //     return elf.iter().sum();
    // });

    // get first and sum of first three calorie totals
    let top: Calorie = sums[0];
    let top_three: Calorie = sums[0..3].to_vec().iter().sum();

    println!("{:?}, {:?}", top, top_three);
}
