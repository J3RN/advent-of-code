use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

fn main() -> () {
    let depths = read_file();

    println!("Problem1: {}", count_increasing_depths(&depths));
    println!(
        "Problem2: {}",
        count_increasing_depths(&combine_depths(&depths))
    );
}

fn count_increasing_depths(depths: &Vec<i32>) -> i32 {
    let mut count = 0;

    for pair in depths.windows(2) {
        if pair[1] > pair[0] {
            count += 1
        }
    }

    count
}

fn combine_depths(depths: &Vec<i32>) -> Vec<i32> {
    depths
        .windows(3)
        .map(|window| window[0] + window[1] + window[2])
        .collect()
}

fn read_file() -> Vec<i32> {
    let mut f = File::open("input.txt").unwrap();
    let mut contents = String::new();

    f.read_to_string(&mut contents).unwrap();

    contents
        .split('\n')
        .filter(|x| x.len() > 0)
        .map(|x| i32::from_str(x).unwrap())
        .collect()
}
