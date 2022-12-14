use nom::{
    character::complete::{char, digit1, newline},
    combinator::recognize,
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, terminated},
    IResult, Parser,
};

const INPUT_STR: &str = include_str!("../input.txt");

#[derive(Debug, PartialEq, Clone, Eq)]
enum ListElem {
    List(Vec<ListElem>),
    Number(i32),
}

impl Ord for ListElem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (ListElem::List(a), ListElem::List(b)) => a.cmp(b),
            (ListElem::List(a), b @ ListElem::Number(_)) => a.cmp(&vec![b.clone()]),
            (a @ ListElem::Number(_), ListElem::List(b)) => vec![a.clone()].cmp(b),
            (ListElem::Number(a), ListElem::Number(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for ListElem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

type Res<'a, A> = IResult<&'a str, A>;

fn list(input: &str) -> Res<ListElem> {
    delimited(char('['), separated_list0(char(','), list_elem), char(']'))
        .map(|items| ListElem::List(items))
        .parse(input)
}

fn number(input: &str) -> Res<ListElem> {
    recognize(digit1)
        .map(|digits: &str| ListElem::Number(digits.parse().unwrap()))
        .parse(input)
}

fn list_elem(input: &str) -> Res<ListElem> {
    list.or(number).parse(input)
}

type Pair = (ListElem, ListElem);

fn parser(input: &str) -> Res<Vec<Pair>> {
    separated_list1(
        newline,
        pair(terminated(list, newline), terminated(list, newline)),
    )
    .parse(input)
}

fn is_right_order((left, right): &(ListElem, ListElem)) -> bool {
    right > left
}

fn main() {
    let (_, input) = parser(INPUT_STR).unwrap();
    let first: usize = input
        .clone()
        .into_iter()
        .enumerate()
        .filter(|(_, pair)| is_right_order(pair))
        .map(|(index, _)| index + 1)
        .sum();
    println!("First: {}", first);

    let mut second_vec: Vec<ListElem> = input
        .clone()
        .into_iter()
        .flat_map(|(left, right)| vec![left, right])
        .collect();

    let first_delim = ListElem::List(vec![ListElem::List(vec![ListElem::Number(2)])]);
    let second_delim = ListElem::List(vec![ListElem::List(vec![ListElem::Number(6)])]);

    second_vec.push(first_delim.clone());
    second_vec.push(second_delim.clone());

    second_vec.sort();

    let second: usize = second_vec
        .into_iter()
        .enumerate()
        .filter(|(_, elem)| elem == &first_delim || elem == &second_delim)
        .map(|(index, _)| index + 1)
        .product();
    println!("Second: {}", second);
}
