use std::collections::{HashMap, HashSet};

const INPUT_STR: &str = include_str!("../input.txt");

type Coord = (usize, usize);

fn get_2d_neighbors(&(x, y): &Coord) -> Vec<Coord> {
    let mut result = vec![(x + 1, y), (x, y + 1)];

    if x > 0 {
        result.push((x - 1, y));
    }

    if y > 0 {
        result.push((x, y - 1));
    }

    result
}

fn main() {
    let mut start = (0, 0);
    let mut end = (0, 0);

    let zero_ground: u32 = 'a'.into();
    let heights: HashMap<Coord, u32> = INPUT_STR
        .lines()
        .enumerate()
        .flat_map(|(line_index, line_value)| {
            line_value
                .char_indices()
                .map(move |(char_index, char_value)| ((char_index, line_index), char_value))
        })
        .map(|(coord, ch)| {
            (
                coord,
                match ch {
                    'S' => {
                        start = coord;
                        'a'
                    }
                    'E' => {
                        end = coord;
                        'z'
                    }
                    c => c,
                },
            )
        })
        .map(|(coord, ch)| (coord, ch as u32 - zero_ground))
        .collect();

    let mut stack: Vec<Coord> = vec![end];
    let mut visited: HashSet<Coord> = Default::default();

    let mut step: usize = 0;
    let mut nearest_zero_steps = 0;

    let steps: usize = 'search: loop {
        let current_stack = stack.clone();
        stack.clear();

        for item in current_stack {
            if visited.contains(&item) {
                continue;
            }

            visited.insert(item);

            let &item_value = heights.get(&item).unwrap();
            if item_value == 0 && nearest_zero_steps == 0 {
                nearest_zero_steps = step;
            }

            if item == start {
                break 'search step;
            }

            for neighbor in get_2d_neighbors(&item) {
                if visited.contains(&neighbor) {
                    continue;
                }

                if heights
                    .get(&neighbor)
                    .filter(|&&neighbor_value| neighbor_value + 1 >= item_value)
                    .is_some()
                {
                    stack.push(neighbor)
                }
            }
        }

        step += 1
    };

    println!("First: {}", steps);
    println!("Second: {}", nearest_zero_steps);
}
