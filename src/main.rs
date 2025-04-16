use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use clap::Subcommand;
use env_logger::Builder;
use log::{debug, info};

use codecrafters_interpreter as lox;

use lox::ast::Ast;
use lox::interpreter::Interpreter;
use lox::parser::Parser;
use lox::scanner::Scanner;

#[derive(ClapParser, Debug)]
#[command(version, about = "Lox language interpreter", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    commands: Commands,

    /// Enable logging to app.log
    #[arg(long, global = true)]
    log: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Tokenizes input from a file, printing each token
    Tokenize { filename: Option<PathBuf> },

    /// Parses input from a file as a single expression and prints its AST
    Parse { filename: Option<PathBuf> },

    /// Evaluates input from a file as a single expression and prints the result
    Evaluate { filename: Option<PathBuf> },

    /// Runs input from a file as a Lox program
    Run { filename: Option<PathBuf> },
}

/// Reads the contents of a file into a Vec<u8>
fn read_file(filename: PathBuf) -> Result<Vec<u8>> {
    info!("Reading file: {:?}", filename);
    let file = File::open(&filename).context(format!("Failed to open file {:?}", filename))?;
    let mut reader = BufReader::new(file);
    let mut buf = Vec::new();

    let bytes = reader
        .read_to_end(&mut buf)
        .context(format!("Failed to read file {:?}", filename))?;

    info!("Read {} bytes from {:?}", bytes, filename);

    Ok(buf)
}

fn init_logger() -> Result<()> {
    // Create or open the log file
    let log_file = File::create("app.log").context("Failed to create app.log")?;

    // Configure env_logger to write to file with statement number and source line
    Builder::new()
        .format(|buf, record| {
            // Strip 'codecrafters_interpreter::' from module path
            let module = record
                .module_path()
                .unwrap_or("<unnamed>")
                .strip_prefix("codecrafters_interpreter::")
                .unwrap_or(record.module_path().unwrap_or("<unnamed>"));
            writeln!(
                buf,
                "[{}:{}] - {}",
                module,
                record.line().unwrap_or(0),
                record.args()
            )
        })
        .target(env_logger::Target::Pipe(Box::new(log_file)))
        .filter(None, log::LevelFilter::Debug) // Default to Debug, override with RUST_LOG
        .init();

    info!("Logger initialized, writing to app.log");
    Ok(())
}

fn main() -> Result<()> {
    // Initialize logger before parsing CLI args
    let args: Cli = Cli::parse();

    // Initialize logger only if --log flag is provided
    if args.log {
        init_logger()?;
    } else {
        // Initialize a minimal logger to avoid "no logger" errors
        env_logger::Builder::new()
            .filter_level(log::LevelFilter::Off)
            .init();
    }

    info!("CLI arguments: {:?}", args);

    match args.commands {
        Commands::Tokenize { filename } => match filename {
            Some(filename) => {
                info!("Running Tokenize subcommand");
                let buf = read_file(filename)?;
                let mut scanner = Scanner::new(buf);
                let mut tokenized = true;

                while let Some(token) = scanner.next() {
                    match token {
                        Ok(token) => {
                            debug!("Scanned token: {}", token);

                            println!("{}", token);
                        }

                        Err(e) => {
                            tokenized = false;

                            debug!("Tokenization debug: {}", e);

                            eprintln!("{}", e);
                        }
                    }
                }

                if !tokenized {
                    debug!("Tokenization failed, exiting with code 65");

                    std::process::exit(65);
                }

                info!("Tokenization completed successfully");
            }
            None => {
                info!("No filepath provided for Tokenize");

                println!("No input filepath was provided. Exiting...");

                std::process::exit(0);
            }
        },

        Commands::Parse { filename } => match filename {
            Some(filename) => {
                info!("Running Parse subcommand");
                let buf = read_file(filename)?;
                let scanner = Scanner::new(buf);
                let mut parser = Parser::new(scanner);

                match parser.parse() {
                    Ok(expr) => {
                        info!("Expression parsed successfully");
                        let printer = Ast; // Assuming Ast is a struct with print method
                        let ast_str = printer.print(&expr);

                        debug!("AST: {}", ast_str);
                        println!("{}", ast_str);
                    }

                    Err(e) => {
                        debug!("Parse debug: {}", e);
                        eprintln!("{}", e);
                        std::process::exit(65);
                    }
                }

                info!("Parse subcommand completed");
            }
            None => {
                info!("No filepath provided for Parse");
                println!("No input filepath was provided. Exiting...");
                std::process::exit(0);
            }
        },

        Commands::Evaluate { filename } => match filename {
            Some(filename) => {
                info!("Running Evaluate subcommand");

                let buf = read_file(filename)?;
                let scanner = Scanner::new(buf);
                let mut parser = Parser::new(scanner);
                let mut interpreter = Interpreter::new();

                match parser.parse() {
                    Ok(expr) => {
                        info!("Expression parsed successfully");

                        match interpreter.evaluate(&expr) {
                            Ok(value) => {
                                debug!("Evaluated to: {}", value);
                                println!("{}", value);
                            }

                            Err(e) => {
                                debug!("Evaluation debug: {}", e);
                                eprintln!("{}", e);
                                std::process::exit(70);
                            }
                        }
                    }

                    Err(e) => {
                        debug!("Parse debug: {}", e);
                        eprintln!("{}", e);
                        std::process::exit(65);
                    }
                }

                info!("Evaluate subcommand completed");
            }

            None => {
                info!("No filepath provided for Evaluate");
                println!("No input filepath was provided. Exiting...");
                std::process::exit(0);
            }
        },

        Commands::Run { filename } => match filename {
            Some(filename) => {
                info!("Running Run subcommand");
                let buf = read_file(filename)?;

                // For logging only
                let file_content: String = unsafe { String::from_utf8_unchecked(buf.clone()) };
                info!("Provided input:\n {}", file_content);

                let scanner = Scanner::new(buf);
                let parser = Parser::new(scanner);
                let mut interpreter = Interpreter::new();

                let mut statements = Vec::new();

                for stmt in parser {
                    match stmt {
                        Ok(stmt) => {
                            debug!("Parsed statement: {:?}", stmt);
                            statements.push(stmt);
                        }
                        Err(e) => {
                            debug!("Parse debug: {}", e);
                            eprintln!("{}", e);
                            std::process::exit(65);
                        }
                    }
                }

                info!("Parsed {} statements", statements.len());

                match interpreter.interpret(&statements) {
                    Ok(()) => {
                        info!("Program executed successfully");
                    }

                    Err(e) => {
                        debug!("Runtime debug: {}", e);
                        eprintln!("{}", e);
                        std::process::exit(70);
                    }
                }
            }

            None => {
                info!("No filepath provided for Run");
                println!("No input filepath was provided. Exiting...");
                std::process::exit(0);
            }
        },
    }

    Ok(())
}
