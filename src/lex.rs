use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Lex {
    Left,
    Right,
    Symbol(String),
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Quote,
}

pub fn lex(src: String) -> Result<Vec<Lex>, String> {
    let mut result: Vec<Lex> = vec![];
    let mut chars = src.chars();
    let mut next_char = chars.next();
    loop {
        if let Some(ch) = next_char {
            match ch {
                ' ' | '\n' => { next_char = chars.next(); }
                '(' | '[' => {
                    result.push(Lex::Left);
                    next_char = chars.next();
                }
                ')' | ']' => {
                    result.push(Lex::Right);
                    next_char = chars.next();
                }
                '\'' => {
                    result.push(Lex::Quote);
                    next_char = chars.next();
                }
                '<' => {
                    next_char = chars.next();
                    if let Some('=') = next_char {
                        result.push(Lex::Symbol("<=".to_string()));
                        next_char = chars.next();
                    } else {
                        result.push(Lex::Symbol("<".to_string()));
                    }
                }
                '>' => {
                    next_char = chars.next();
                    if let Some('=') = next_char {
                        result.push(Lex::Symbol(">=".to_string()));
                        next_char = chars.next();
                    } else {
                        result.push(Lex::Symbol(">".to_string()));
                    }
                }
                '"' => {
                    let mut string = String::new();
                    next_char = chars.next();
                    while let Some(ch) = next_char {
                        if ch == '"' {
                            break;
                        } else {
                            string.push(ch);
                            next_char = chars.next();
                        }
                    }
                    result.push(Lex::String(string));
                    next_char = chars.next();
                }
                x if x.is_ascii_digit() || x == '.' => {
                    let mut digit_string = String::new();
                    digit_string.push(x);
                    next_char = chars.next();
                    while let Some(ch) = next_char {
                        if ch.is_ascii_digit() || ch == '.' {
                            digit_string.push(ch);
                            next_char = chars.next();
                        } else {
                            break;
                        }
                    }
                    result.push(parse_number(digit_string)?);
                }
                y => {
                    let mut symbol = String::new();
                    symbol.push(y);
                    next_char = chars.next();
                    while let Some(ch) = next_char {
                        if ch.is_alphanumeric() || ch == '-' || ch == '?' || ch == '!' || ch == '_' {
                            symbol.push(ch);
                            next_char = chars.next();
                        } else {
                            break;
                        }
                    }
                    if symbol.to_lowercase() == "true" {
                        result.push(Lex::Boolean(true));
                    } else if symbol.to_lowercase() == "false" {
                        result.push(Lex::Boolean(false));
                    } else {
                        result.push(Lex::Symbol(symbol));
                    }
                }
            }
        } else {
            break;
        }
    }
    Ok(result)
}

impl fmt::Display for Lex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lex::Left => write!(f, "("),
            Lex::Right => write!(f, ")"),
            Lex::Quote => write!(f, "'"),
            Lex::Symbol(s) => write!(f, "{}", s),
            Lex::Integer(i) => write!(f, "{}", i),
            Lex::Float(x) => write!(f, "{}", x),
            Lex::Boolean(b) => write!(f, "{}", b),
            Lex::String(s) => write!(f, "{}", s),
        }
    }
}

fn parse_number(digit_string: String) -> Result<Lex, String> {
    if digit_string.contains('.') {
        if let Ok(x) = digit_string.parse::<f64>() {
            Ok(Lex::Float(x))
        } else {
            Err("Invalid float".to_string())
        }
    } else if let Ok(i) = digit_string.parse::<isize>() {
        Ok(Lex::Integer(i))
    } else {
        Err("Invalid int".to_string())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn mixed() {
        assert_eq!(lex("(x? (y-y! 12) 0.4 true <= >= ')".to_string()), Ok(vec![
            Lex::Left,
            Lex::Symbol("x?".to_string()),
            Lex::Left,
            Lex::Symbol("y-y!".to_string()),
            Lex::Integer(12),
            Lex::Right,
            Lex::Float(0.4),
            Lex::Boolean(true),
            Lex::Symbol("<=".to_string()),
            Lex::Symbol(">=".to_string()),
            Lex::Quote,
            Lex::Right,
        ]));
    }

    #[test]
    fn double_quote() {
        assert_eq!(lex("\"abc\"".to_string()), Ok(vec![Lex::String("abc".to_string())]));
        assert_eq!(lex("\"a --- c\"".to_string()), Ok(vec![Lex::String("a --- c".to_string())]));
    }
}