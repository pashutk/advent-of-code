use std::collections::HashSet;

const INPUT_STR: &str = include_str!("../input.txt");

const CHAR_CODE_A_LOWERCASE: i32 = 'a' as i32;
const CHAR_CODE_A_UPPERCASE: i32 = 'A' as i32;

fn char_priority(ch: &char) -> i32 {
    match ch {
        'a'..='z' => (*ch as i32) - CHAR_CODE_A_LOWERCASE + 1,
        'A'..='Z' => (*ch as i32) - CHAR_CODE_A_UPPERCASE + 27,
        _ => -1,
    }
}

fn first() -> i32 {
    INPUT_STR
        .lines()
        .map(|rucksack| {
            let (left, right) = rucksack.split_at(rucksack.len() / 2);
            let left_set: HashSet<char> = HashSet::from_iter(left.chars());
            let right_set = HashSet::from_iter(right.chars());
            let mut intersection = left_set.intersection(&right_set);
            let a = intersection
                .next()
                .expect("No intersections between left and right");
            char_priority(a)
        })
        .sum()
}

fn second() -> i32 {
    Vec::from_iter(
        INPUT_STR
            .lines()
            .map(|line| HashSet::from_iter(line.chars())),
    )
    .chunks(3)
    .map(|group: &[HashSet<char>]| {
        let g1_and_g2 = HashSet::from_iter(group[1].intersection(&group[2]).map(|char| *char));
        let mut intersection = group[0].intersection(&g1_and_g2);
        char_priority(
            intersection
                .next()
                .expect("No intersections between groups"),
        )
    })
    .sum()
}

fn main() {
    println!("First: {}", first());
    println!("Second: {}", second());
}
