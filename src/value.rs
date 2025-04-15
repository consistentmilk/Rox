#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    NativeFunction {
        name: String,
        arity: usize,
        func: fn(&[Value]) -> Result<Value, String>,
    },
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::NativeFunction { name, .. } => write!(f, "<native fn {}>", name),

            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{:.0}", n)
                } else {
                    write!(f, "{}", n)
                }
            }

            Value::String(s) => write!(f, "{}", s),

            Value::Bool(b) => write!(f, "{}", b),

            Value::Nil => write!(f, "nil"),
        }
    }
}
