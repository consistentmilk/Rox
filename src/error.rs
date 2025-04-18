//! Centralised error hierarchy for the **Lox interpreter**.
//!
//! All subsystems (scanner, parser, resolver, runtime, CLI) must convert their
//! internal failure modes into one of the variants defined here.  This enables a
//! uniform `Result<T>` alias throughout the crate and ergonomic inter‑operation
//! with `anyhow`, while still preserving rich diagnostic detail.
//!
//! The module **does not** print diagnostics itself

use std::io;
use thiserror::Error;

use log::info;

/// Canonical error type used throughout the interpreter.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LoxError {
    /// Lexical (scanner) error with source line information.
    #[error("[line {line}] Error: {message}")]
    Lex {
        /// Human‑readable description.
        message: String,

        /// 1‑based line where the error occurred.
        line: usize,
    },

    /// Syntactic (parser) error.
    #[error("[line {line}] Error: {message}")]
    Parse { message: String, line: usize },

    /// Static‑analysis or resolution failure (e.g. early‑binding errors).
    #[error("[line {line}] Error: {message}")]
    Resolve { message: String, line: usize },

    /// Runtime evaluation error.
    #[error("Runtime error: {0}")]
    Runtime(String),

    /// Wrapper around `std::io::Error` (transparent).  Enables `?` on I/O ops.
    #[error(transparent)]
    Io(#[from] io::Error),

    /// UTF‑8 decoding failure when ingesting external text.
    #[error(transparent)]
    Utf8(#[from] std::string::FromUtf8Error),
}

impl LoxError {
    /// Helper constructor for the **scanner**.
    pub fn lex<S: Into<String>>(line: usize, msg: S) -> Self {
        let message: String = msg.into();

        info!("Creating Lex error: line={}, msg={}", line, message);

        LoxError::Lex { message, line }
    }

    /// Helper constructor for the **parser**.
    pub fn parse<S: Into<String>>(line: usize, msg: S) -> Self {
        let message: String = msg.into();

        info!("Creating Parse error: line={}, msg={}", line, message);

        LoxError::Parse { message, line }
    }

    /// Helper constructor for the **resolver**.
    pub fn resolve<S: Into<String>>(line: usize, msg: S) -> Self {
        let message: String = msg.into();

        info!("Creating Resolve error: line={}, msg={}", line, message);

        LoxError::Resolve { message, line }
    }
}

/// Crate‑wide `Result` alias.
pub type Result<T> = std::result::Result<T, LoxError>;
