use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{env, io};

use colored::*;

use euler_lisp::code_formatter::{Formatter, PrettyPrinter};
use euler_lisp::debugger::Debugger;
use euler_lisp::evaluator::Evaluator;
use euler_lisp::{doc, repl};

fn find_file_for_problem(problem: isize, include_all: bool) -> Option<PathBuf> {
    let mut paths = Vec::new();

    let subfolder = (problem - 1) / 50;
    let solved_path = format!(
        "../EulerSolutions/{:03}-{:03}/{:02}.scm",
        subfolder * 50 + 1,
        subfolder * 50 + 50,
        problem
    );
    paths.push(Path::new(&solved_path).to_path_buf());

    if include_all {
        let wip_path = format!("../EulerSolutions/wip/{:02}.scm", problem);
        let slow_path = format!("../EulerSolutions/slow/{:02}.scm", problem);
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

fn format_duration(duration: Duration) -> String {
    let millis = duration.subsec_millis();
    format!("{}.{}s", duration.as_secs(), millis).to_string()
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

// TODO: Collect errors
// TODO: Mark slow problems
// TODO: For some reason this takes much longer than the normal run function,
// maybe because more processes use the same amount of cache?
fn run_problem(solutions: &HashMap<isize, String>, problem: isize) -> RunResult {
    println!("Running problem {}", problem);
    if let Some(path) = find_file_for_problem(problem, false) {
        let output = Rc::new(RefCell::new(Vec::new()));
        let mut eval = Evaluator::new(output.clone(), true);

        let s = path.to_str().unwrap();
        eval.load_file(s, true);

        let now = Instant::now();
        // TODO: handle errors
        eval.run();
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
    let mut args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Usage:");
        println!("  $prog_name repl");
        println!("  $prog_name run <filename>");
        println!();
        println!("Flags:");
        println!("  --no-stdlib, don't load the stdlib on startup");
    } else {
        args.remove(0); // skip program name
        let command = args.remove(0);
        let use_stdlib = !args.iter().any(|x| *x == "--no-stdlib");
        match &command[..] {
            "fmt" => {
                let filename = args.get(0).expect("No filename provided").clone();

                let mut file = File::open(filename.clone()).expect("Could not open file");
                let mut input = String::new();
                file.read_to_string(&mut input)
                    .expect("Could not read file");

                let mut formatter = Formatter::from_string(&input, Some(filename));
                let mut printer = PrettyPrinter::new();

                for el in &formatter.all().unwrap() {
                    printer.print(el, true, true);
                    println!()
                }
            }
            "repl" => repl::run(use_stdlib),
            v @ "run" | v @ "doc" => {
                let mut filename = args.get(0).expect("No filename provided").clone();

                if !filename.ends_with(".scm") {
                    let problem = filename.parse::<isize>().unwrap();
                    if let Some(problem_path) = find_file_for_problem(problem, true) {
                        filename = problem_path.to_str().unwrap().to_string();
                    } else {
                        panic!(format!("Could not find file for problem {}", problem));
                    }
                }

                if v == "run" {
                    let mut eval = Evaluator::new(Rc::new(RefCell::new(io::stdout())), use_stdlib);
                    eval.load_file(&filename, true);
                    // TODO: Handle errors
                    eval.run();
                } else {
                    doc::process_file(&filename);
                }
            }
            "debug" => {
                let mut filename = args.get(0).expect("No filename provided").clone();

                if !filename.ends_with(".scm") {
                    let problem = filename.parse::<isize>().unwrap();
                    if let Some(problem_path) = find_file_for_problem(problem, true) {
                        filename = problem_path.to_str().unwrap().to_string();
                    } else {
                        panic!(format!("Could not find file for problem {}", problem));
                    }
                }

                let mut debugger = Debugger::new(use_stdlib);
                debugger.debug_file(&filename);
            }
            "test" => {
                let mut solutions: HashMap<isize, String> = HashMap::new();

                let solutions_file = File::open("../EulerSolutions/solutions.csv")
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

                let from_str = args.get(0).expect("Usage: lisp test <from> [<to>]");
                let to_str = args.get(1).unwrap_or(from_str);
                let from = from_str
                    .parse::<isize>()
                    .expect("<from> is not a valid number");
                let to = to_str.parse::<isize>().expect("<to> is not a valid number");

                // TODO: Add flag to switch between iter & par_iter
                let problems: Vec<isize> = (from..=to).collect();
                let results: Vec<RunResult> = problems
                    // .par_iter()
                    .iter()
                    .map(|p| run_problem(&solutions, *p))
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
            other => println!("Unknown command \"{}\"", other),
        }
    }
}
