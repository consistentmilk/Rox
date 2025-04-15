use std::fs::File;
use std::io::BufReader;
use std::io::Read;
use std::path::PathBuf;

use codecrafters_interpreter as lox;

use clap::Parser as ClapParser;
use clap::Subcommand;

use lox::ast::Ast;
use lox::parser::Parser;
use lox::scanner::Scanner;

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    commands: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Tokenizes the provided input from a given valid filepath
    Tokenize { filename: Option<PathBuf> },

    /// Parses the provided input from a given valid filepath
    Parse { filename: Option<PathBuf> },
}

fn main() -> anyhow::Result<()> {
    let args: Cli = Cli::parse();

    match args.commands {
        Commands::Tokenize { filename } => match filename {
            Some(filename) => {
                let mut buf: Vec<u8> = Vec::new();
                let mut reader: BufReader<File> = BufReader::new(File::open(filename)?);
                let _ = reader.read_to_end(&mut buf);

                let mut scanner: Scanner = Scanner::new(&buf);
                let mut tokenized = true;

                while let Some(token) = scanner.next() {
                    match token {
                        Ok(token) => println!("{}", token),

                        Err(e) => {
                            tokenized = false;
                            eprintln!("{}", e);
                        }
                    }
                }

                if !tokenized {
                    std::process::exit(65);
                }
            }

            None => {
                println!("No input filepath was provided. Exiting...");
                std::process::exit(0);
            }
        },

        Commands::Parse { filename } => match filename {
            Some(filename) => {
                let mut buf: Vec<u8> = Vec::new();
                let mut reader: BufReader<File> = BufReader::new(File::open(filename)?);
                let _ = reader.read_to_end(&mut buf);

                let scanner: Scanner = Scanner::new(&buf);
                let mut parser: Parser = Parser::new(scanner);

                match parser.parse() {
                    Ok(expr) => {
                        let printer = Ast;
                        println!("{}", printer.print(&expr));
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                        std::process::exit(65);
                    }
                }
            }

            None => {
                println!("No input filepath was provided. Exiting...");
                std::process::exit(0);
            }
        },
    }

    Ok(())
}
