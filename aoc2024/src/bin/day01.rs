#![feature(iterator_try_collect)]

use clap::Parser;
use itertools::Itertools;

#[derive(clap::Parser)]
struct Args {
    path: String
}

fn main() {
    // Load input
    let Args {path} = Args::parse();
    println!("Reading {path}");
    let input = std::fs::read_to_string(path).unwrap();

    // Parse input
    let (a1, b1): (Vec<i64>, Vec<i64>) = input.lines().map(|l| l.split_whitespace().map(|w| w.parse::<i64>().unwrap()).next_tuple().unwrap()).unzip();

    fn dedup_sort(x: Vec<i64>) -> Vec<i64> {
        x.into_iter().sorted().dedup().collect()
    }
    let a2 = dedup_sort(a1);
    let b2 = dedup_sort(b1);

    let sum: i64 = a2.into_iter().zip(b2).map(|(ax, bx)| (ax - bx).abs()).sum();
    println!("Sum: {sum}");
}
