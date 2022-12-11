use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    str::FromStr,
};

const INPUT_STR: &str = include_str!("../input.txt");

type Size = i64;

#[derive(Debug, Clone, Copy)]
enum OperationArg {
    Const(Size),
    Dup,
}

#[derive(Clone)]
struct Monkey {
    items: VecDeque<Size>,
    operation: (char, OperationArg),
    test_div_by: Size,
    test_true: usize,
    test_false: usize,
}

impl Monkey {
    fn inspect(&self, item: Size) -> Size {
        match self.operation {
            ('+', OperationArg::Const(arg)) => item + arg,
            ('*', OperationArg::Const(arg)) => item * arg,
            ('+', OperationArg::Dup) => item + item,
            ('*', OperationArg::Dup) => item * item,
            _ => panic!("Unknown operation"),
        }
    }

    fn test(&self, item: Size) -> usize {
        if item % self.test_div_by == 0 {
            self.test_true
        } else {
            self.test_false
        }
    }
}

fn parse_prefixed<A: FromStr>(s: &str, prefix: &str) -> A
where
    <A as FromStr>::Err: Debug,
{
    s.trim_start_matches(prefix).parse().unwrap()
}

fn main() {
    let monkeys: Vec<Monkey> = INPUT_STR
        .split("\n\n")
        .map(|str| {
            let lines: Vec<&str> = str.split("\n").map(|a| a.trim()).collect();
            let items = lines[1]
                .trim_start_matches("Starting items: ")
                .split(", ")
                .map(|a| a.parse().unwrap())
                .collect();

            let operation = lines[2]
                .trim_start_matches("Operation: new = old ")
                .split_once(" ")
                .map(|(op_str, arg_str)| {
                    (
                        op_str.chars().next().unwrap(),
                        match arg_str {
                            "old" => OperationArg::Dup,
                            value => OperationArg::Const(value.parse().unwrap()),
                        },
                    )
                })
                .unwrap();

            let test_div_by = parse_prefixed(lines[3], "Test: divisible by ");
            let test_true = parse_prefixed(lines[4], "If true: throw to monkey ");
            let test_false = parse_prefixed(lines[5], "If false: throw to monkey ");

            Monkey {
                items,
                operation,
                test_div_by,
                test_true,
                test_false,
            }
        })
        .collect();

    fn monkey_business(mut monkeys: &mut Vec<Monkey>, do_relief: bool, rounds: usize) -> usize {
        let modulo: Size = monkeys.iter().map(|a| a.test_div_by).product();

        let mut inspected_counter: HashMap<usize, usize> = Default::default();

        let mut turn = |monkey_index: usize, monkeys: &mut Vec<Monkey>| {
            let monkey = &mut monkeys[monkey_index];
            let monkey_items = monkey.items.clone();
            monkey.items = Default::default();
            let monkey = monkey.clone();
            for item in monkey_items {
                *inspected_counter.entry(monkey_index).or_insert(0) += 1;
                let inspected = monkey.inspect(item);
                let relieved = if do_relief { inspected / 3 } else { inspected };
                let relieved = relieved % modulo;
                let next_monkey = monkey.test(relieved);
                let next_monkey = &mut monkeys.get_mut(next_monkey).unwrap();
                next_monkey.items.push_front(relieved);
            }
        };

        for _ in 0..rounds {
            for index in 0..monkeys.iter_mut().count() {
                turn(index, &mut monkeys);
            }
        }

        let mut counters: Vec<usize> = inspected_counter.into_values().collect();
        counters.sort();
        counters.into_iter().rev().take(2).product()
    }

    let first = monkey_business(&mut monkeys.clone(), true, 20);
    println!("First: {}", first);

    let second = monkey_business(&mut monkeys.clone(), false, 10000);
    println!("Second: {}", second);
}
