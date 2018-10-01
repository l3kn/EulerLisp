use std::fs::File;
use std::io::{Write, BufReader, BufRead};
use std::fmt::Write as FmtWrite;
use std::process::{Command, Stdio};
use std::error::Error;

pub fn process_file(path: &str) {
    let input = File::open(path).expect("Failed to open file");
    let buffered = BufReader::new(input);

    let mut buffer: Vec<String> = Vec::new();
    let mut output = String::new();

    writeln!(output, "# {}", path);

    for line in buffered.lines() {
        let line = line.expect("Failed to read line");
        let processed = line.trim_left_matches(';');

        if line.starts_with(";;") {
            if buffer.len() > 0 {
                writeln!(output, "\n``` clojure").unwrap();
                for code_line in buffer.iter() {
                    writeln!(output, "{}", code_line).unwrap();
                }
                buffer.clear();
                writeln!(output, "```\n").unwrap();
            }

            writeln!(output, "{}", processed).expect("Failed to write to output");
        } else {
            // Skip normal line comments
            if !line.starts_with(";") {
                buffer.push(line.clone());
            }
        }
    }

    if buffer.len() > 0 {
        writeln!(output, "``` clojure").unwrap();
        for code_line in buffer.iter() {
            writeln!(output, "{}", code_line).unwrap();
        }
        writeln!(output, "```").unwrap();
    }

    let pdf_path = format!("{}.pdf", path); 
    let process = match Command::new("pandoc")
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .arg("-o")
                                .arg(pdf_path)
                                .spawn() {
        Err(why) => panic!("couldn't spawn pandoc: {}", why.description()),
        Ok(process) => process,
    };

    match process.stdin.unwrap().write_all(output.as_bytes()) {
        Err(why) => panic!("couldn't write to pandoc stdin: {}",
                           why.description()),
        Ok(_) => (),
    }
}
