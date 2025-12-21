use std::fs;

fn main() {
    let ranges = load_ranges();
    let sum: u64 = ranges
        .iter()
        .flat_map(|arg: &(u64, u64)| invalid_ids(*arg, is_invalid_id))
        .sum();
    println!("{}", sum);
    let sum: u64 = ranges
        .iter()
        .flat_map(|arg: &(u64, u64)| invalid_ids(*arg, is_invalid_id2))
        .sum();
    println!("{}", sum);
}

fn invalid_ids<P>((low, high): (u64, u64), pred: P) -> Vec<u64>
where
    P: FnMut(&u64) -> bool,
{
    (low..=high).filter(pred).collect()
}

fn is_invalid_id(i: &u64) -> bool {
    even_num_digits(*i) && repeated_twice(*i)
}

fn even_num_digits(i: u64) -> bool {
    num_digits(i) % 2 == 0
}

fn repeated_twice(i: u64) -> bool {
    let halfway_exp = num_digits(i) / 2;
    let upper_half = i / 10_u64.pow(halfway_exp);
    let lower_half = i % 10_u64.pow(halfway_exp);

    upper_half == lower_half
}

fn num_digits(i: u64) -> u32 {
    i.ilog10() + 1
}

fn is_invalid_id2(i: &u64) -> bool {
    let first = i % 10;
    let mut seq = vec![first];
    let mut rest = i / 10;

    while rest > 0 {
        if is_invalid_id2_with(rest, &seq) {
            return true;
        }

        seq.push(rest % 10);
        rest /= 10;
    }

    false
}

fn is_invalid_id2_with(i: u64, seq: &[u64]) -> bool {
    let mut i_copy = i;

    for digit in seq {
        if i_copy == 0 || i_copy % 10 != *digit {
            return false;
        }
        i_copy /= 10;
    }

    i_copy == 0 || is_invalid_id2_with(i_copy, seq)
}

fn load_ranges() -> Vec<(u64, u64)> {
    let contents = fs::read_to_string("input").unwrap();

    contents
        .split(",")
        .map(|seg| {
            let (low, high) = seg.split_once("-").unwrap();
            let low_val: u64 = low.parse().unwrap();
            let high_val: u64 = high.trim().parse().unwrap();
            (low_val, high_val)
        })
        .collect()
}
