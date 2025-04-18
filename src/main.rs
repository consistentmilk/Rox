#![allow(unused)]

use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::PathBuf;

use memmap2::Mmap;

use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use clap::Subcommand;
use env_logger::Builder;
use log::{debug, info};

use codecrafters_interpreter as lox;

use lox::ast_printer::AstPrinter;
use lox::interpreter::Interpreter;
use lox::parser::{Parser, Stmt};
use lox::resolver::Resolver;
use lox::scanner::Scanner;
use lox::token::{Token, TokenType};

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

    let file: File =
        File::open(&filename).context(format!("Failed to open file {:?}", filename))?;

    let mut reader: BufReader<File> = BufReader::new(file);
    let mut buf: Vec<u8> = Vec::new();

    let bytes: usize = reader
        .read_to_end(&mut buf)
        .context(format!("Failed to read file {:?}", filename))?;

    info!("Read {} bytes from {:?}", bytes, filename);

    Ok(buf)
}

fn init_logger() -> Result<()> {
    // Create or open the log file
    let log_file: File = File::create("app.log").context("Failed to create app.log")?;

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

                // Open and mmap the file:
                let file =
                    File::open(&filename).context(format!("Failed to open file {:?}", filename))?;
                let mmap = unsafe {
                    Mmap::map(&file).context(format!("Failed to mmap file {:?}", filename))?
                };

                let mut scanner = Scanner::new(&mmap[..]);
                let mut tokenized = true;

                while let Some(result) = scanner.next() {
                    match result {
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
                Ok(())
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

                // ── mmap source file ──────────────────────────────────────
                let file =
                    File::open(&filename).context(format!("Failed to open file {:?}", filename))?;
                let mmap = unsafe {
                    Mmap::map(&file).context(format!("Failed to mmap file {:?}", filename))?
                };

                // ── 1. lex into Vec<Token> ────────────────────────────────
                let mut scanner = Scanner::new(&mmap[..]);
                let mut tokens: Vec<Token<'_>> = Vec::new();
                let mut lex_ok = true;

                while let Some(result) = scanner.next() {
                    match result {
                        Ok(tok) => tokens.push(tok),
                        Err(e) => {
                            lex_ok = false;

                            debug!("Tokenization debug: {}", e);

                            eprintln!("{}", e);
                        }
                    }
                }

                if !lex_ok {
                    debug!("Lexing failed, exiting with code 65");

                    std::process::exit(65);
                }

                // ── 2. ensure terminating semicolon (for single‑expr input)
                if let Some(last) = tokens.last() {
                    if last.token_type == TokenType::EOF {
                        if tokens.len() == 1
                            || tokens[tokens.len() - 2].token_type != TokenType::SEMICOLON
                        {
                            let line: usize = last.line;
                            let semi: Token<'_> = Token::new(TokenType::SEMICOLON, ";", line);
                            tokens.insert(tokens.len() - 1, semi);
                        }
                    }
                }

                // ── 3. parse ───────────────────────────────────────────────
                let mut parser: Parser<'_> = Parser::new(&tokens);

                let ast: Vec<Stmt<'_>> = match parser.parse() {
                    Ok(ast) => ast,
                    Err(e) => {
                        debug!("Parse error: {}", e);

                        eprintln!("{}", e);
                        std::process::exit(65);
                    }
                };

                for stmt in &ast {
                    if let lox::parser::Stmt::Expression(expr) = stmt {
                        println!("{}", AstPrinter::print(expr));
                    } else {
                        // For now ignore non‑expression statements in parse mode
                    }
                }

                info!("Parsing completed successfully");

                Ok(())
            }

            None => {
                info!("No filepath provided for Parse");

                println!("No input filepath was provided. Exiting…");

                std::process::exit(0);
            }
        },

        // ────────────────────────── EVALUATE ────────────────────────────
        Commands::Evaluate { filename } => match filename {
            Some(filename) => {
                info!("Running Evaluate subcommand");

                // mmap source file
                let file =
                    File::open(&filename).context(format!("Failed to open file {:?}", filename))?;
                let mmap = unsafe {
                    Mmap::map(&file).context(format!("Failed to mmap file {:?}", filename))?
                };

                // 1. scan → Vec<Token>
                let mut scanner = Scanner::new(&mmap[..]);
                let mut tokens: Vec<Token<'_>> = Vec::new();
                let mut lex_ok = true;

                while let Some(res) = scanner.next() {
                    match res {
                        Ok(t) => tokens.push(t),
                        Err(e) => {
                            lex_ok = false;

                            debug!("Tokenization debug: {}", e);

                            eprintln!("{}", e);
                        }
                    }
                }

                if !lex_ok {
                    debug!("Lexing failed, exiting with code 65");

                    std::process::exit(65);
                }

                // Ensure a trailing ‘;’ so we treat the input as a single stmt.
                if let Some(last) = tokens.last() {
                    if last.token_type == TokenType::EOF
                        && (tokens.len() == 1
                            || tokens[tokens.len() - 2].token_type != TokenType::SEMICOLON)
                    {
                        let line = last.line;
                        let semi = Token::new(TokenType::SEMICOLON, ";", line);
                        tokens.insert(tokens.len() - 1, semi);
                    }
                }

                // 2. parse
                let mut parser = Parser::new(&tokens);
                let ast: Vec<Stmt<'_>> = match parser.parse() {
                    Ok(a) => a,
                    Err(e) => {
                        debug!("Parse error: {}", e);

                        eprintln!("{}", e);
                        std::process::exit(65);
                    }
                };

                // Expect exactly one `Stmt::Expression`
                let expr = match &ast[..] {
                    [Stmt::Expression(e)] => e.clone(),
                    _ => {
                        eprintln!("Evaluate expects exactly one stand‑alone expression.");
                        std::process::exit(70);
                    }
                };

                // Wrap it in a print stmt so the interpreter will emit the value
                let wrapped_ast = vec![Stmt::Print(expr)];

                // 3. resolve (static analysis)
                let mut interpreter = Interpreter::new();

                let mut resolver = Resolver::new(&mut interpreter);
                if let Err(e) = resolver.resolve(&wrapped_ast) {
                    debug!("Resolve error: {}", e);

                    eprintln!("{}", e);
                    std::process::exit(65);
                }

                // 4. interpret
                if let Err(e) = interpreter.interpret(&wrapped_ast) {
                    debug!("Runtime error: {}", e);

                    eprintln!("{}", e);
                    std::process::exit(70);
                }

                Ok(())
            }
            None => {
                info!("No filepath provided for Evaluate");

                println!("No input filepath was provided. Exiting…");

                std::process::exit(0);
            }
        },

        // ──────────────────────────── RUN ───────────────────────────────
        Commands::Run { filename } => match filename {
            Some(filename) => {
                info!("Running Run subcommand");

                // mmap source file
                let file =
                    File::open(&filename).context(format!("Failed to open file {:?}", filename))?;
                let mmap = unsafe {
                    Mmap::map(&file).context(format!("Failed to mmap file {:?}", filename))?
                };

                // 1. scan
                let mut scanner = Scanner::new(&mmap[..]);
                let mut tokens: Vec<Token<'_>> = Vec::new();
                let mut lex_ok = true;

                while let Some(res) = scanner.next() {
                    match res {
                        Ok(t) => tokens.push(t),
                        Err(e) => {
                            lex_ok = false;

                            debug!("Tokenization debug: {}", e);

                            eprintln!("{}", e);
                        }
                    }
                }

                if !lex_ok {
                    debug!("Lexing failed, exiting with code 65");

                    std::process::exit(65);
                }

                // 2. parse
                let mut parser: Parser<'_> = Parser::new(&tokens);

                let ast: Vec<Stmt<'_>> = match parser.parse() {
                    Ok(a) => a,
                    Err(e) => {
                        debug!("Parse error: {}", e);

                        eprintln!("{}", e);

                        std::process::exit(65);
                    }
                };

                // 3. resolve
                let mut interpreter: Interpreter<'_> = Interpreter::new();

                let mut resolver: Resolver<'_, '_> = Resolver::new(&mut interpreter);

                if let Err(e) = resolver.resolve(&ast) {
                    debug!("Resolve error: {}", e);

                    eprintln!("{}", e);

                    std::process::exit(65);
                }

                // 4. interpret
                if let Err(e) = interpreter.interpret(&ast) {
                    debug!("Runtime error: {}", e);

                    eprintln!("{}", e);
                    std::process::exit(70);
                }

                info!("Program executed successfully");

                Ok(())
            }
            None => {
                info!("No filepath provided for Run");

                println!("No input filepath was provided. Exiting…");

                std::process::exit(0);
            }
        },
    }
}
