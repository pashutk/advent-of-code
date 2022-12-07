use std::collections::HashMap;

use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1, newline},
    combinator::recognize,
    multi::{many1, separated_list0, separated_list1},
    sequence::{preceded, separated_pair},
    IResult, Parser as NomParser,
};

const INPUT_STR: &str = include_str!("../input.txt");

#[derive(Debug, Clone)]
enum FolderContent<'a> {
    Dir(&'a str),
    File(&'a str, u32),
}

#[derive(Debug)]
enum CdArgument<'a> {
    Up,
    Root,
    Down(&'a str),
}

#[derive(Debug)]
enum ExecItem<'a> {
    Ls(Vec<FolderContent<'a>>),
    Cd(CdArgument<'a>),
}

type Res<'a, A> = IResult<&'a str, A>;

fn dir_item(input: &str) -> Res<FolderContent> {
    preceded(tag("dir "), alpha1)
        .map(|a| FolderContent::Dir(a))
        .parse(input)
}

fn filename(input: &str) -> Res<&str> {
    recognize(many1(alpha1.or(tag(".")))).parse(input)
}

fn file_item(input: &str) -> Res<FolderContent> {
    separated_pair(recognize(digit1), tag(" "), filename)
        .map(|(size, file)| FolderContent::File(file, size.parse().unwrap()))
        .parse(input)
}

fn ls_output(input: &str) -> Res<Vec<FolderContent>> {
    separated_list0(newline, dir_item.or(file_item)).parse(input)
}

fn ls_command(input: &str) -> Res<ExecItem> {
    preceded(tag("$ ls\n"), ls_output)
        .map(|content| ExecItem::Ls(content))
        .parse(input)
}

fn cd_command(input: &str) -> Res<ExecItem> {
    preceded(tag("$ cd "), tag("..").or(tag("/")).or(recognize(alpha1)))
        .map(|arg| {
            ExecItem::Cd(match arg {
                ".." => CdArgument::Up,
                "/" => CdArgument::Root,
                folder_name => CdArgument::Down(folder_name),
            })
        })
        .parse(input)
}

fn parser(input: &str) -> Res<Vec<ExecItem>> {
    separated_list1(newline, cd_command.or(ls_command)).parse(input)
}

fn main() {
    let (_, output) = parser(INPUT_STR).unwrap();

    #[derive(Clone, Debug)]
    struct FolderMeta<'a> {
        content: Vec<FolderContent<'a>>,
        size: Option<u32>,
    }

    let mut path_stack: Vec<&str> = vec![];
    let mut map: HashMap<String, FolderMeta> = HashMap::new();

    for op in output {
        match op {
            ExecItem::Cd(arg) => match arg {
                CdArgument::Root => {
                    path_stack.clear();
                }
                CdArgument::Up => {
                    path_stack.pop();
                }
                CdArgument::Down(segment) => {
                    path_stack.push(segment);
                }
            },
            ExecItem::Ls(content) => {
                let path = "/".to_owned() + &path_stack.join("/");
                map.insert(
                    path,
                    FolderMeta {
                        content,
                        size: None,
                    },
                );
            }
        }
    }

    fn calculate_size(folder_path: String, map: &mut HashMap<String, FolderMeta>) -> u32 {
        let meta = map.get_mut(&folder_path).unwrap().clone();
        if let Some(size) = meta.size {
            return size;
        }

        let sum = meta
            .content
            .iter()
            .map(|item| match item {
                FolderContent::File(_, size) => *size,
                FolderContent::Dir(name) => {
                    let prefix: String = if folder_path.eq("/") {
                        "/".to_owned()
                    } else {
                        folder_path.clone() + "/"
                    };
                    calculate_size(prefix + name, map)
                }
            })
            .sum();

        map.get_mut(&folder_path).unwrap().size = Some(sum);
        sum
    }

    for (path, mut meta) in map.clone() {
        if let None = meta.size {
            meta.size = Some(calculate_size(path, &mut map));
        }
    }

    let sizes: Vec<u32> = map.iter().map(|(_path, meta)| meta.size.unwrap()).collect();
    let first: u32 = sizes.iter().filter(|&&size| size <= 100000).sum();

    let total_space: u32 = 70000000;
    let space_needed: u32 = 30000000;
    let used_space: u32 = map.get("/").unwrap().size.unwrap();
    let free_space = total_space - used_space;
    let need_to_free = space_needed - free_space;

    let mut sorted_sizes = sizes.clone();
    sorted_sizes.sort();
    let second = sorted_sizes.iter().find(|&&a| a >= need_to_free).unwrap();

    println!("First: {}", first);
    println!("Second: {}", second);
}
