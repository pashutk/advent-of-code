const INPUT_STR: &str = include_str!("../input.txt");

fn first() -> i32 {
    INPUT_STR
        .trim()
        .split("\n\n")
        .map(|rows| {
            rows.split("\n")
                .map(|row| {
                    row.parse::<i32>()
                        .expect("Every inventory record should be a valid i32")
                })
                .sum::<i32>()
        })
        .max()
        .expect("Input is empty")
}

fn second() -> i32 {
    let calories_iterator = INPUT_STR.trim().split("\n\n").map(|rows| {
        rows.split("\n")
            .map(|row| {
                row.parse::<i32>()
                    .expect("Every inventory record should be a valid i32")
            })
            .sum::<i32>()
    });
    let vec = Vec::from_iter(calories_iterator);

    let max1 = *vec.iter().max().expect("Input file is too short");
    let not_max_1 = vec.iter().filter(|a| **a != max1);
    let max2 = *not_max_1.max().expect("Input file is too short");
    let not_max_1_and_2 = vec.iter().filter(|a| **a != max1).filter(|a| **a != max2);
    let max3 = not_max_1_and_2.max().expect("Input file is too short");

    max1 + max2 + max3
}

fn main() {
    println!("First: {}", first());
    println!("Second: {}", second());
}
