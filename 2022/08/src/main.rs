use std::collections::HashMap;

const INPUT_STR: &str = include_str!("../input.txt");

fn main() {
    let input: Vec<Vec<i8>> = INPUT_STR
        .lines()
        .map(|line| {
            line.chars()
                .map(|char| char.to_digit(10).unwrap())
                .map(|a| a.try_into().unwrap())
                .collect()
        })
        .collect();

    let mut visible_from_left: Vec<Vec<bool>> = input
        .iter()
        .map(|row| row.iter().map(|_| false).collect())
        .collect();
    let mut visible_from_right = visible_from_left.clone();
    let mut visible_from_top = visible_from_left.clone();
    let mut visible_from_bottom = visible_from_left.clone();

    for (row_index, row) in input.iter().enumerate() {
        let mut last_visible_height = -1;
        for (column_index, &value) in row.iter().enumerate() {
            visible_from_left[row_index][column_index] = value > last_visible_height;
            last_visible_height = last_visible_height.max(value);
        }

        last_visible_height = -1;
        for (column_index, &value) in row.iter().enumerate().rev() {
            visible_from_right[row_index][column_index] = value > last_visible_height;
            last_visible_height = last_visible_height.max(value);
        }
    }

    let first_row = &input[0];
    for (column_index, _) in first_row.iter().enumerate() {
        let mut last_visible_height = -1;
        for (row_index, _) in input.iter().enumerate() {
            let value = input[row_index][column_index];
            visible_from_top[row_index][column_index] = value > last_visible_height;
            last_visible_height = last_visible_height.max(value);
        }

        last_visible_height = -1;
        for (row_index, row) in input.iter().enumerate().rev() {
            let value = row[column_index];
            visible_from_bottom[row_index][column_index] = value > last_visible_height;
            last_visible_height = last_visible_height.max(value);
        }
    }

    let mut visiable_tree_count = 0;
    for (row_index, row) in input.iter().enumerate() {
        for (column_index, _) in row.iter().enumerate() {
            if visible_from_left[row_index][column_index]
                || visible_from_right[row_index][column_index]
                || visible_from_top[row_index][column_index]
                || visible_from_bottom[row_index][column_index]
            {
                visiable_tree_count += 1
            }
        }
    }

    println!("First: {}", visiable_tree_count);

    let mut scenic_score: Vec<Vec<usize>> = input
        .iter()
        .map(|row| row.iter().map(|_| 1).collect())
        .collect();

    for (row_index, row) in input.iter().enumerate() {
        let mut visibility_map: HashMap<usize, usize> = HashMap::new();
        let mut furthest_index = 0;
        for (column_index, &value) in row.iter().enumerate() {
            let visible = visibility_map
                .get_mut(&(value as usize))
                .unwrap_or(&mut furthest_index)
                .clone();

            for i in 0..=value {
                visibility_map.insert(i as usize, column_index);
            }
            scenic_score[row_index][column_index] *= column_index.abs_diff(visible);
        }
    }

    for (row_index, row) in input.iter().enumerate() {
        let mut visibility_map: HashMap<usize, usize> = HashMap::new();
        let mut furthest_index = row.len() - 1;
        for (column_index, &value) in row.iter().enumerate().rev() {
            let visible = visibility_map
                .get_mut(&(value as usize))
                .unwrap_or(&mut furthest_index)
                .clone();

            for i in 0..=value {
                visibility_map.insert(i as usize, column_index);
            }
            scenic_score[row_index][column_index] *= column_index.abs_diff(visible);
        }
    }

    let first_row = &input[0];
    for (column_index, _) in first_row.iter().enumerate() {
        let mut furthest_index = 0;
        let mut visibility_map: HashMap<usize, usize> = HashMap::new();
        for (row_index, row) in input.iter().enumerate() {
            let value = row[column_index];
            let visible = visibility_map
                .get_mut(&(value as usize))
                .unwrap_or(&mut furthest_index)
                .clone();

            for i in 0..=value {
                visibility_map.insert(i as usize, row_index);
            }
            scenic_score[row_index][column_index] *= row_index.abs_diff(visible);
        }
    }

    for (column_index, _) in first_row.iter().enumerate() {
        let mut furthest_index = input.len() - 1;
        let mut visibility_map: HashMap<usize, usize> = HashMap::new();
        for (row_index, row) in input.iter().enumerate().rev() {
            let value = row[column_index];
            let visible = visibility_map
                .get_mut(&(value as usize))
                .unwrap_or(&mut furthest_index)
                .clone();

            for i in 0..=value {
                visibility_map.insert(i as usize, row_index);
            }
            scenic_score[row_index][column_index] *= row_index.abs_diff(visible);
        }
    }

    let max_scenic_score = scenic_score
        .iter()
        .map(|row| row.iter().max().unwrap())
        .max()
        .unwrap();

    println!("Second: {}", max_scenic_score);
}
