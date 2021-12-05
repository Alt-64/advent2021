use std::{
    fs::{read_to_string, write},
    io::Error,
};

fn main() {
    let input: Vec<i32> = read_input_file("input.txt").unwrap();

    let mut depth_counter = 0;

    for i in 1..input.len() - 2 {
        let prev = input[i - 1] + input[i] + input[i + 1];
        let curr = input[i] + input[i + 1] + input[i + 2];
        if prev < curr {
            depth_counter += 1;
        }
    }

    write("output.txt", depth_counter.to_string()).unwrap();
}

fn read_input_file(path: &str) -> Result<Vec<i32>, Error> {
    let input = read_to_string(path)?
        .split("\n")
        .map(str::parse::<i32>)
        .flatten()
        .collect::<Vec<i32>>();
    Ok(input)
}
