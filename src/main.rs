mod parse;

use std::io::{self};

use parse::exp;

fn main() {
    println!("Hi! ∃⊥∨→↔");
    println!("Type expression: ");

    let mut buffer = String::new();
    let stdin = io::stdin();
    match stdin.read_line(&mut buffer) {
        Ok(_) => match exp(&buffer) {
            Ok((rem, e)) => {
                println!("Parsed: {:?}", e);
                println!("Reamining: {}", rem);
            }
            _ => println!("Parse error."),
        },
        _ => println!("Input error."),
    }
}
