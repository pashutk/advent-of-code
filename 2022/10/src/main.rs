const INPUT_STR: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Noop,
    Add(i32),
}

impl Instruction {
    fn cycles(&self) -> usize {
        match self {
            Instruction::Noop => 1,
            Instruction::Add(_) => 2,
        }
    }
}

fn main() {
    let input: Vec<Instruction> = INPUT_STR
        .lines()
        .map(|line| match line.split_at(4) {
            ("noop", _) => Instruction::Noop,
            ("addx", arg) => Instruction::Add(arg.trim().parse().unwrap()),
            _ => panic!("Invalid input"),
        })
        .collect();

    let mut cycle: usize = 1;
    let mut instruction_cycle = 1;
    let mut register: i32 = 1;
    let (first_instruction, rest_instructions) = input.split_first().unwrap();
    let mut current_instruction = Some(first_instruction);
    let mut signal_strengths_sum: i32 = 0;
    let mut render = vec![" "; 40 * 6];

    let mut instructions = rest_instructions.iter();
    while let Some(&instruction) = current_instruction {
        if cycle >= 20 && (cycle - 20) % 40 == 0 {
            signal_strengths_sum += register * cycle as i32;
        }

        let pix_index = cycle - 1;
        let pixel_line_index_i32 = (pix_index % 40) as i32;
        if pixel_line_index_i32 == register
            || pixel_line_index_i32 == register - 1
            || pixel_line_index_i32 == register + 1
        {
            render[pix_index] = "#";
        }

        if instruction.cycles() == instruction_cycle {
            match instruction {
                Instruction::Noop => {}
                Instruction::Add(value) => {
                    register += value;
                }
            };
            current_instruction = instructions.next();
            instruction_cycle = 0;
        }

        instruction_cycle += 1;
        cycle += 1;
    }

    let first = signal_strengths_sum;

    println!("First: {}", first);

    let screen_lines: Vec<_> = render.chunks(40).map(|line| line.join("")).collect();
    let screen = screen_lines.join("\n");

    println!("Second:\n{}", screen);
}
