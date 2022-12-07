use std::collections::{HashSet, VecDeque};

const INPUT_STR: &str = include_str!("../input.txt");

fn main() {
    let first = {
        let mut i = 4;
        let (start, _) = INPUT_STR.split_at(4);
        let mut buf: VecDeque<char> = start.chars().collect();
        loop {
            let set: HashSet<&char> = buf.iter().collect();
            if set.len() == 4 {
                break i;
            }

            buf.pop_back();
            buf.push_front(INPUT_STR.chars().nth(i).unwrap());

            i += 1;
        }
    };

    let second = {
        let mut i = 14;
        let (start, _) = INPUT_STR.split_at(14);
        let mut buf: VecDeque<char> = start.chars().collect();
        loop {
            let set: HashSet<&char> = buf.iter().collect();
            if set.len() == 14 {
                break i;
            }

            buf.pop_back();
            buf.push_front(INPUT_STR.chars().nth(i).unwrap());

            i += 1;
        }
    };

    println!("First: {}", first);
    println!("Second: {}", second);
}
