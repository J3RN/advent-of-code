use std::fs;
use std::iter::Iterator;

fn main() {
    let contents = fs::read_to_string("input").expect("Input file missing!");
    let groups = split_into_groups(contents);
    let mut sums: Vec<u32> = groups.iter().map(|x| x.iter().sum()).collect::<Vec<u32>>();
    sums.sort_by(|x, y| y.cmp(x));

    println!("Max: {}", sums[0]);
    println!("Sum of top 3: {}", sums.iter().take(3).sum::<u32>())
}

fn split_into_groups(text: String) -> Vec<Vec<u32>> {
    text.split("\n\n")
        .map(|group| {
            group
                .split("\n")
                .map(|int| int.parse::<u32>().unwrap_or(0))
                .collect()
        })
        .collect()
}
