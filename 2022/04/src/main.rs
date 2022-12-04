use std::collections::HashSet;

const INPUT_STR: &str = include_str!("../input.txt");

fn first() -> i32 {
    INPUT_STR
        .lines()
        .map(|pairs| {
            let mut pairs = pairs.split(',').map(|pair| {
                let mut pair = pair
                    .split('-')
                    .map(|section_num| section_num.parse::<i32>().unwrap());
                (pair.next().unwrap(), pair.next().unwrap())
            });
            (pairs.next().unwrap(), pairs.next().unwrap())
        })
        .filter_map(|a| {
            let left: HashSet<i32> = HashSet::from_iter(a.0 .0..=a.0 .1);
            let right: HashSet<i32> = HashSet::from_iter(a.1 .0..=a.1 .1);
            if left.is_subset(&right) || left.is_superset(&right) {
                Some(1)
            } else {
                None
            }
        })
        .sum()
}

fn second() -> i32 {
    INPUT_STR
        .lines()
        .map(|pairs| {
            let mut pairs = pairs.split(',').map(|pair| {
                let mut pair = pair
                    .split('-')
                    .map(|section_num| section_num.parse::<i32>().unwrap());
                (pair.next().unwrap(), pair.next().unwrap())
            });
            (pairs.next().unwrap(), pairs.next().unwrap())
        })
        .filter_map(|a| {
            let left: HashSet<i32> = HashSet::from_iter(a.0 .0..=a.0 .1);
            let right: HashSet<i32> = HashSet::from_iter(a.1 .0..=a.1 .1);
            if left.intersection(&right).count() != 0 {
                Some(1)
            } else {
                None
            }
        })
        .sum()
}

fn main() {
    println!("First: {}", first());
    println!("Second: {}", second());
}
