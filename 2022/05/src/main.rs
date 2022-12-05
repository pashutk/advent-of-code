use std::collections::HashMap;

const INPUT_STR: &str = include_str!("../input.txt");

fn main() {
    let input: (HashMap<&str, Vec<char>>, Vec<(i32, &str, &str)>, Vec<&str>) = {
        let (stack, moves) = INPUT_STR.split_once("\n\n").unwrap();
        let (cols, indices) = stack.rsplit_once('\n').unwrap();

        (
            HashMap::from_iter(indices.split_whitespace().enumerate().map(|(index, key)| {
                (
                    key,
                    Vec::from_iter(
                        cols.lines()
                            .map(|line| line.chars().nth(1 + index * 4).unwrap())
                            .filter(|crate_id| !crate_id.is_whitespace())
                            .rev(),
                    ),
                )
            })),
            moves
                .lines()
                .map(|step| {
                    let mut words = step.split_whitespace();
                    (
                        words.nth(1).unwrap().parse::<i32>().unwrap(),
                        words.nth(1).unwrap(),
                        words.nth(1).unwrap(),
                    )
                })
                .collect(),
            indices.split_whitespace().collect(),
        )
    };

    let first = {
        let (mut stacks, moves, indices) = input.clone();
        moves.iter().for_each(|&(amount, from_key, to_key)| {
            for _ in 0..amount {
                let from_stack = stacks.get_mut(from_key).unwrap();
                let val = from_stack.pop().unwrap();
                let to_stack = stacks.get_mut(to_key).unwrap();
                to_stack.push(val);
            }
        });

        indices
            .iter()
            .map(|index| stacks.get(index).unwrap().last().unwrap().to_string())
            .collect::<Vec<String>>()
            .concat()
    };

    let second = {
        let (mut stacks, moves, indices) = input.clone();
        moves.iter().for_each(|&(amount, from_key, to_key)| {
            let from_stack = stacks.get_mut(from_key).unwrap();
            let mut buf = vec![];
            for _ in 0..amount {
                buf.push(from_stack.pop().unwrap())
            }
            let to_stack = stacks.get_mut(to_key).unwrap();
            for _ in 0..amount {
                to_stack.push(buf.pop().unwrap())
            }
        });

        indices
            .iter()
            .map(|index| stacks.get(index).unwrap().last().unwrap().to_string())
            .collect::<Vec<String>>()
            .concat()
    };

    println!("First: {}", first);
    println!("Second: {}", second);
}
