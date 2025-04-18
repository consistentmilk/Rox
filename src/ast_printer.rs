use crate::parser::{Expr, LiteralValue};

/// Converts an expression to the CodeCrafters/Crafting‑Interpreters
/// prefix form (no heap allocations except `String` joins for output).
pub struct AstPrinter;

impl AstPrinter {
    pub fn print(expr: &Expr<'_>) -> String {
        match expr {
            // ── literals ────────────────────────────────────────────────
            Expr::Literal(lit) => match lit {
                LiteralValue::True => "true".into(),

                LiteralValue::False => "false".into(),

                LiteralValue::Nil => "nil".into(),

                LiteralValue::Str(s) => format!("{}", s),

                LiteralValue::Number(n) => {
                    if n.fract() == 0.0 {
                        // 3.0 → 3
                        format!("{:.1}", n)
                    } else {
                        n.to_string()
                    }
                }
            },

            // ── grouping ────────────────────────────────────────────────
            Expr::Grouping(inner) => format!("(group {})", Self::print(inner)),

            // ── unary operator ──────────────────────────────────────────
            Expr::Unary { operator, right } => {
                format!("({} {})", operator.lexeme, Self::print(right))
            }

            // ── binary operator ─────────────────────────────────────────
            Expr::Binary {
                left,
                operator,
                right,
            } => format!(
                "({} {} {})",
                operator.lexeme,
                Self::print(left),
                Self::print(right)
            ),

            // ── logical operator ───────────────────────────────────────
            Expr::Logical {
                left,
                operator,
                right,
            } => format!(
                "({} {} {})",
                operator.lexeme,
                Self::print(left),
                Self::print(right)
            ),

            // ── variable / assign / call (future stages) ───────────────
            Expr::Variable(name) => name.lexeme.into(),

            Expr::Assign { name, value } => format!("(= {} {})", name.lexeme, Self::print(value)),

            Expr::Call {
                callee, arguments, ..
            } => {
                let mut s = format!("(call {}", Self::print(callee));
                for arg in arguments {
                    s.push(' ');
                    s.push_str(&Self::print(arg));
                }
                s.push(')');
                s
            }
        }
    }
}
