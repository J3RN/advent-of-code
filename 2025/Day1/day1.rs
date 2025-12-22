use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &String) -> () {
    let mut pos: i32 = 50;
    let mut zero_count = 0;

    for line in contents.split("\n") {
        if line == "" {
            break;
        }
        let mul = if line.chars().nth(0) == Some('L') {-1} else { 1 };
        let clicks = line[1..].parse::<i32>().unwrap() * mul;
        pos = (pos + clicks).rem_euclid(100);
        if pos == 0 { zero_count += 1 }
    }

    println!("{}", zero_count);
}

fn part2(contents: &String) {
    let mut pos: u32 = 50;
    let mut zero_count = 0;

    for line in contents.split("\n") {
        if line == "" {
            break;
        }

        let clicks: u32 = line[1..].parse().unwrap();

        match line.chars().next() {
            Some('L') => {
                zero_count += count_zeros_left(pos, clicks);
                pos = ((100 + pos) - (clicks % 100)) % 100;
            },
            Some('R') => {
                zero_count += count_zeros_right(pos, clicks);
                pos = (pos + clicks).rem_euclid(100);
            },
            _ => todo!(),
        }
    }

    println!("{}", zero_count);
}

// There has to be a simpler way
fn count_zeros_left(pos: u32, clicks: u32) -> u32 {
    if pos == 0 {
        clicks / 100
    } else if clicks >= pos {
        1 + (clicks - pos) / 100
    } else {
        0
    }
}

fn count_zeros_right(pos: u32, clicks: u32) -> u32 {
    (pos + clicks) / 100
}
