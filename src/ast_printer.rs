//! Module `ast_printer` provides functionality to convert Lox AST expressions
//! into a human-readable prefix notation string (CodeCrafters style). This is
//! primarily used for debugging and testing the expression parser.
//!
//! # Overview
//!
//! - Defines the `AstPrinter` struct with a single `print` method.
//! - Recursively traverses the `Expr` AST, formatting nodes in prefix form.
//! - Allocates only for the final output string; intermediate operations avoid heap allocations where possible.
//!
//! # Example
//!
//! ```rust
//! use lox::ast_printer::AstPrinter;
//! use lox::parser::Expr;
//!
//! // Given an expression AST `expr`, obtain its prefix notation:
//! let s = AstPrinter::print(&expr);
//! println!("{}", s);  // e.g. "(* 1 (group 2))"
//! ```

use crate::parser::{Expr, LiteralValue};

/// `AstPrinter` traverses an expression AST and produces a prefix notation string.
/// Only final output uses heap allocation for `String` joining; inner recursion uses stack.
pub struct AstPrinter;

impl AstPrinter {
    /// Print the given expression in prefix notation.
    ///
    /// # Parameters
    /// - `expr`: Reference to the AST node to format.
    ///
    /// # Returns
    /// A `String` containing the prefix-formatted representation.
    pub fn print(expr: &Expr<'_>) -> String {
        match expr {
            Expr::Literal(lit) => match lit {
                LiteralValue::True => "true".into(),
                LiteralValue::False => "false".into(),
                LiteralValue::Nil => "nil".into(),
                LiteralValue::Str(s) => s.clone(),
                LiteralValue::Number(n) => {
                    // Format numeric literals, preserving one decimal for integers
                    if n.fract() == 0.0 {
                        // e.g., 3.0 → "3.0"
                        format!("{:.1}", n)
                    } else {
                        n.to_string()
                    }
                }
            },

            Expr::Grouping(inner) => {
                // Parenthesized sub-expression formatted in prefix form
                format!("(group {})", Self::print(inner))
            }

            Expr::Unary { operator, right } => {
                // Prefix unary operator: (! expr) or (- expr)
                format!("({} {})", operator.lexeme, Self::print(right))
            }

            Expr::Binary {
                left,
                operator,
                right,
            } => {
                // Infix binary operator rendered as prefix: (op left right)
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    Self::print(left),
                    Self::print(right)
                )
            }

            Expr::Logical {
                left,
                operator,
                right,
            } => {
                // Logical operators 'and'/'or' in prefix form
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    Self::print(left),
                    Self::print(right)
                )
            }

            Expr::Variable(name) => {
                // Variable access prints the identifier lexeme
                name.lexeme.into()
            }

            Expr::Assign { name, value } => {
                // Assignment formatted as (= name value)
                format!("(= {} {})", name.lexeme, Self::print(value))
            }

            Expr::Call {
                callee, arguments, ..
            } => {
                // Function or method call: (call callee arg1 arg2 ...)
                let mut result = format!("(call {}", Self::print(callee));
                for arg in arguments {
                    result.push(' ');
                    result.push_str(&Self::print(arg));
                }
                result.push(')');
                result
            }

            Expr::Get { object, name } => {
                // Property access formatted as (get object propertyName)
                format!("(get {} {})", Self::print(object), name.lexeme)
            }

            Expr::Set {
                object,
                name,
                value,
            } => {
                // Property assignment formatted as (set object propertyName value)
                format!(
                    "(set {} {} {})",
                    Self::print(object),
                    name.lexeme,
                    Self::print(value)
                )
            }

            Expr::This(keyword) => {
                // 'this' keyword prints as-is
                keyword.lexeme.into()
            }

            Expr::Super { .. } => todo!(),
        }
    }
}
