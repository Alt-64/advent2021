use std::{
    fs::{read_to_string, write},
    io::Error,
};

fn main() {
    let input: Vec<i32> = read_input_file("input.txt").unwrap();

    // let mut larger_measurements: Vec<i32> = Vec::with_capacity(7);
    let mut depth_counter = 0;

    for i in 1..input.len() - 2 {
        let prev = input[i - 1] + input[i] + input[i + 1];
        let curr = input[i] + input[i + 1] + input[i + 2];
        if prev < curr {
            depth_counter += 1;
            // larger_measurements.push(input[i + 1])
        }
    }

    // write_output_file("output.txt", larger_measurements).unwrap();
    // write("output.txt", larger_measurements.len().to_string()).unwrap();
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

fn write_output_file(path: &str, data: Vec<i32>) -> Result<(), Error> {
    let output = data
        .iter()
        .map(i32::to_string)
        .collect::<Vec<String>>()
        .join("\n");
    return write(path, output);
}
