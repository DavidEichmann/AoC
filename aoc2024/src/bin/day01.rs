#![feature(duration_millis_float)]
#![feature(iterator_try_collect)]

use std::time::Instant;

use clap::Parser;
use itertools::Itertools;

#[derive(clap::Parser)]
struct Args {
    path: String
}

fn main() {
    // Load input
    let t_load_start = Instant::now();
    let Args {path} = Args::parse();
    println!("Reading {path}");
    let input = std::fs::read_to_string(path).unwrap();
    let t_load_end = Instant::now();

    // Parse input
    let t_parse_start = Instant::now();
    let (a1, b1): (Vec<i64>, Vec<i64>) = input.lines().map(|l| l.split_whitespace().map(|w| w.parse::<i64>().unwrap()).next_tuple().unwrap()).unzip();
    let t_parse_end = Instant::now();

    // Part 1
    let t_part_1_start = Instant::now();
    fn sort(x: &[i64]) -> Vec<i64> {
        x.iter().cloned().sorted().collect()
    }
    let a2 = sort(&a1);
    let b2 = sort(&b1);
    let distance: i64 = a2.into_iter().zip(b2).map(|(ax, bx)| (ax - bx).abs()).sum();
    let t_part_1_end = Instant::now();
    
    // Part 2
    let t_part_2_start = Instant::now();
    let b_counts = b1.iter().cloned().counts();
    let similarity: i64 = a1.iter().cloned().map(|x| x * b_counts.get(&x).cloned().unwrap_or(0) as i64).sum();
    let t_part_2_end = Instant::now();
    
    println!("Distance: {distance}");
    println!("Similarity: {similarity}");
    println!("");
    println!("load: {:.3} ms", t_load_end.duration_since(t_load_start).as_millis_f64());
    println!("parse: {:.3} ms", t_parse_end.duration_since(t_parse_start).as_millis_f64());
    println!("part 1: {:.3} ms", t_part_1_end.duration_since(t_part_1_start).as_millis_f64());
    println!("part 2: {:.3} ms", t_part_2_end.duration_since(t_part_2_start).as_millis_f64());
}
