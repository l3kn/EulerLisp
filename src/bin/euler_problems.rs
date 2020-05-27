extern crate colored;
extern crate euler_lisp;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{env, process};

use colored::*;

use euler_lisp::vm::VM;

fn find_file_for_problem(problem: isize, include_all: bool) -> Option<PathBuf> {
    let mut paths = Vec::new();

    let subfolder = (problem - 1) / 50;
    let solved_path = format!(
        "/home/leon/src/euler_solutions/{:03}-{:03}/{:02}.scm",
        subfolder * 50 + 1,
        subfolder * 50 + 50,
        problem
    );
    paths.push(Path::new(&solved_path).to_path_buf());

    if include_all {
        let wip_path = format!("/home/leon/src/euler_solutions/wip/{:02}.scm", problem);
        let slow_path = format!("/home/leon/src/euler_solutions/slow/{:02}.scm", problem);
        paths.push(Path::new(&wip_path).to_path_buf());
        paths.push(Path::new(&slow_path).to_path_buf());
    }

    for path in paths {
        if path.exists() {
            return Some(path);
        }
    }

    None
}

enum RunResult {
    Correct {
        problem: isize,
        duration: Duration,
    },
    Wrong {
        problem: isize,
        duration: Duration,
        expected: String,
        got: String,
    },
    Missing {
        problem: isize,
    },
}

fn format_duration(duration: Duration) -> String {
    let micros = duration.subsec_micros();
    format!("{}.{:06}s", duration.as_secs(), micros).to_string()
}

// TODO: Collect errors
// TODO: Mark slow problems
// TODO: For some reason this takes much longer than the normal run function,
// maybe because more processes use the same amount of cache?
fn run_problem(solutions: &HashMap<isize, String>, problem: isize, verbose: bool) -> RunResult {
    // TODO: Make solution return the value,
    // use this instead of comparing output.
    if verbose {
        println!("Running problem {}", problem);
    }

    if let Some(path) = find_file_for_problem(problem, false) {
        let output = Rc::new(RefCell::new(Vec::new()));
        let mut vm = VM::with_output(output.clone());

        vm.load_stdlib();
        let s = path.to_str().unwrap();

        let now = Instant::now();
        vm.load_file(s, true);
        // TODO: handle errors
        let duration = now.elapsed();

        let solution = String::from_utf8(output.borrow().clone()).unwrap();
        let got = solution.trim_start_matches("Solution: ").trim().to_string();

        match solutions.get(&problem) {
            Some(expected) if expected == &got => RunResult::Correct { problem, duration },
            Some(expected) => RunResult::Wrong {
                problem,
                duration,
                got,
                expected: expected.clone(),
            },
            None => panic!(format!("No reference solution for {}", problem)),
        }
    } else {
        RunResult::Missing { problem }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage:");
        println!("  {} <from> [to] [options]", args[0]);
        println!("Available Options:");
        println!("  --csv, generate CSV output");
        process::exit(1);
    }

    let mut solutions: HashMap<isize, String> = HashMap::new();
    let use_csv = args.iter().any(|x| *x == "--csv");

    let solutions_file = File::open("/home/leon/src/euler_solutions/solutions.csv")
        .expect("Could not open solutions file");
    let mut rdr = csv::Reader::from_reader(solutions_file);

    for result in rdr.records() {
        let record = result.expect("Failed to parse solution entry");
        if record.len() != 2 {
            panic!(format!("Invalid solution entry: {:?}", record));
        }

        let problem = record.get(0).unwrap().parse::<isize>().unwrap();
        let solution = record.get(1).unwrap().to_string();

        if solution != "" {
            solutions.insert(problem, solution);
        }
    }

    let from_str = args.get(1).unwrap();
    let to_str = args.get(2).unwrap_or(from_str);
    let from = from_str
        .parse::<isize>()
        .expect(&format!("{} is not a valid number", from_str));
    let to = to_str
        .parse::<isize>()
        .expect(&format!("{} is not a valid number", to_str));

    // TODO: Add flag to switch between iter & par_iter
    let problems: Vec<isize> = (from..=to).collect();

    if use_csv {
        for problem in problems {
            match run_problem(&solutions, problem, false) {
                RunResult::Correct { problem, duration } => {
                    println!("{},{}", problem, format_duration(duration));
                }
                RunResult::Wrong { problem, .. } => {
                    println!("{},wrong", problem);
                }
                RunResult::Missing { problem } => {
                    println!("{},missing", problem);
                }
            }
        }
    } else {
        let results: Vec<RunResult> = problems
            // .par_iter()
            .iter()
            .map(|p| run_problem(&solutions, *p, true))
            .collect();

        let mut full = Duration::new(0, 0);
        let mut missing = Vec::new();
        let mut correct = Vec::new();
        let mut wrong = Vec::new();

        for res in results {
            match res {
                RunResult::Correct { problem, duration } => {
                    full += duration;
                    correct.push((problem, duration))
                }
                RunResult::Wrong {
                    problem,
                    duration,
                    expected,
                    got,
                } => {
                    full += duration;
                    wrong.push((problem, got, expected))
                }
                RunResult::Missing { problem } => missing.push(problem),
            }
        }

        if !correct.is_empty() {
            println!("{}", "Correct".green().bold());
            for (problem, duration) in correct {
                let time = format_duration(duration);
                println!(" {} {}", problem.to_string().green(), time);
            }
            println!();
        }

        if !missing.is_empty() {
            println!("{}", "Missing".yellow().bold());
            for problem in missing {
                println!("{}", problem.to_string().yellow());
            }
            println!();
        }

        if !wrong.is_empty() {
            println!("{}", "Wrong".red().bold());
            for (problem, got, expected) in wrong {
                println!(" {}", problem.to_string().red());
                println!("   Expected: {}", expected.green());
                println!("   Got:      {}", got.red());
            }
            println!();
        }

        let millis = full.subsec_millis();
        println!("Time: {}.{}s", full.as_secs(), millis);
    }
}
