use std::collections::HashSet;

const INPUT_STR: &str = include_str!("../input.txt");

fn main() {
    let input: Vec<(i32, i32)> = INPUT_STR
        .lines()
        .map(|line| line.split_once(" ").unwrap())
        .flat_map(|(direction, count)| {
            vec![
                match direction {
                    "D" => (1, 0),
                    "U" => (-1, 0),
                    "R" => (0, 1),
                    "L" => (0, -1),
                    _ => panic!("Invalid input"),
                };
                count.parse().unwrap()
            ]
        })
        .collect();

    fn get_uniq_tail_positions(knots_count: usize, steps: &Vec<(i32, i32)>) -> usize {
        let mut knots = vec![(0, 0); knots_count];
        let mut visited: Vec<(i32, i32)> = vec![];

        for (delta_x, delta_y) in steps {
            let (mut knot_prev, rest_knots) = knots.split_first_mut().unwrap();
            knot_prev.0 += delta_x;
            knot_prev.1 += delta_y;

            for knot in rest_knots.iter_mut() {
                if knot_prev.0.abs().max(knot_prev.1.abs()) > 1 {
                    let (new_delta_x, new_delta_y) = (knot_prev.0.signum(), knot_prev.1.signum());
                    knot_prev.0 -= new_delta_x;
                    knot_prev.1 -= new_delta_y;

                    knot.0 += new_delta_x;
                    knot.1 += new_delta_y;
                }
                knot_prev = knot;
            }

            visited.push(knots.last().unwrap().clone());
        }

        let set: HashSet<(i32, i32)> = HashSet::from_iter(visited.clone());
        set.len()
    }

    println!("First: {}", get_uniq_tail_positions(2, &input));
    println!("Second: {}", get_uniq_tail_positions(10, &input));
}
