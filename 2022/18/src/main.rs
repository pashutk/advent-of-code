use std::collections::HashSet;

const INPUT_STR: &str = include_str!("../input.txt");

type Point = (i32, i32, i32);

fn get_neighbors((x, y, z): Point) -> Vec<Point> {
    vec![
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1),
    ]
}

fn surface(points: &Vec<Point>) -> i32 {
    let cache: HashSet<Point> = points.clone().into_iter().collect();
    points
        .iter()
        .map(|&point| {
            get_neighbors(point)
                .iter()
                .map(|point| if !cache.contains(point) { 1 } else { 0 })
                .sum::<i32>()
        })
        .sum()
}

fn main() {
    let droplets: Vec<Point> = INPUT_STR
        .lines()
        .map(|line| {
            let vec: Vec<i32> = line.split(",").map(|num| num.parse().unwrap()).collect();
            let mut iter = vec.into_iter();
            (
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            )
        })
        .collect();

    let droplet_set: HashSet<Point> = droplets.clone().into_iter().collect();

    let droplets_total_surface = surface(&droplets);

    println!("First: {}", droplets_total_surface);

    let min_x = droplets.iter().min_by_key(|(x, _, _)| x).unwrap().0;
    let max_x = droplets.iter().max_by_key(|(x, _, _)| x).unwrap().0;
    let min_y = droplets.iter().min_by_key(|(_, y, _)| y).unwrap().1;
    let max_y = droplets.iter().max_by_key(|(_, y, _)| y).unwrap().1;
    let min_z = droplets.iter().min_by_key(|(_, _, z)| z).unwrap().2;
    let max_z = droplets.iter().max_by_key(|(_, _, z)| z).unwrap().2;

    let mut empty: Vec<Point> = vec![];
    for x in (min_x - 1)..=(max_x + 1) {
        for y in (min_y - 1)..=(max_y + 1) {
            for z in (min_z - 1)..=(max_z + 1) {
                let point = (x, y, z);
                if !droplet_set.contains(&point) {
                    empty.push(point);
                }
            }
        }
    }

    let empty_set: HashSet<Point> = empty.clone().into_iter().collect();

    let outer_example = (min_x - 1, min_y - 1, min_z - 1);

    let mut outer_empty_set: HashSet<Point> = Default::default();

    let mut buffer = vec![outer_example];
    let mut checked_if_outer_empty: HashSet<Point> = Default::default();
    while buffer.len() > 0 {
        let mut new_buffer: Vec<Point> = vec![];
        for point in buffer.clone() {
            if checked_if_outer_empty.contains(&point) {
                continue;
            }
            checked_if_outer_empty.insert(point);
            outer_empty_set.insert(point);
            get_neighbors(point).into_iter().for_each(|point| {
                if !checked_if_outer_empty.contains(&point) && empty_set.contains(&point) {
                    new_buffer.push(point);
                }
            })
        }
        buffer = new_buffer;
    }

    let mut cavity_set: HashSet<Point> = Default::default();
    for x in (min_x - 1)..=(max_x + 1) {
        for y in (min_y - 1)..=(max_y + 1) {
            for z in (min_z - 1)..=(max_z + 1) {
                let point = (x, y, z);
                if !outer_empty_set.contains(&point) && !droplet_set.contains(&point) {
                    cavity_set.insert(point);
                }
            }
        }
    }

    let mut clusters: Vec<Vec<Point>> = Default::default();
    let mut visited_cavities: HashSet<Point> = Default::default();

    for cavity in cavity_set.clone() {
        if visited_cavities.contains(&cavity) {
            continue;
        }

        visited_cavities.insert(cavity);

        let mut cluster: Vec<Point> = Default::default();
        let mut buffer: Vec<Point> = vec![cavity];
        let mut checked: HashSet<Point> = Default::default();

        while buffer.len() > 0 {
            let mut new_buffer: Vec<Point> = Default::default();
            for point in buffer.clone() {
                visited_cavities.insert(point);

                if checked.contains(&point) {
                    continue;
                }

                checked.insert(point);
                cluster.push(point);
                for neighbor in get_neighbors(point) {
                    if !checked.contains(&neighbor) && cavity_set.contains(&neighbor) {
                        new_buffer.push(neighbor);
                    }
                }
            }
            buffer = new_buffer;
        }
        clusters.push(cluster);
    }

    let clusters_surface: i32 = clusters.iter().map(|cluster| surface(cluster)).sum();

    println!("Second: {}", droplets_total_surface - clusters_surface);
}
