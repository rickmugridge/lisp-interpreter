use std::slice::Iter;
use crate::expr::Expr;
use crate::lex::{lex, Lex};

pub fn parser(src: &str) -> Result<Expr, String> {
    let tokens = lex(src.to_string())?;
    let mut tokens = tokens.iter();
    if let Some(result) = parse(&mut tokens, src)? {
        let remainder: Vec<_> = tokens.collect();
        if remainder.len() > 0 {
            let left = remainder.iter()
                .map(|r| r.to_string())
                .collect::<Vec<String>>()
                .join(" ");
            Err(format!("result is {result} but remaining tokens: {:?}", left))
        } else {
            Ok(result)
        }
    } else {
        Err("Unexpected eof".to_string())
    }
}

fn parse(tokens: &mut Iter<Lex>, src: &str) -> Result<Option<Expr>, String> {
    while let Some(token) = tokens.next() {
        match token {
            Lex::Left => {
                let mut elements: Vec<Expr> = vec![];
                loop {
                    if let Some(exp) = parse(tokens, src)? {
                        elements.push(exp);
                    } else {
                        let expr = Expr::SExpr(elements);
                        return Ok(Some(expr));
                    }
                }
            }
            Lex::Right => {
                return Ok(None);
            }
            Lex::Symbol(s) => {
                let expr = Expr::Symbol(s.to_string());
                return Ok(Some(expr));
            }
            Lex::Integer(j) => {
                let expr = Expr::IntAtom(*j);
                return Ok(Some(expr));
            }
            Lex::Float(x) => {
                let expr = Expr::FloatAtom(*x);
                return Ok(Some(expr));
            }
            Lex::Boolean(x) => {
                let expr = Expr::BoolAtom(*x);
                return Ok(Some(expr));
            }
           Lex::String(s) => {
                let expr = Expr::StringAtom(s.clone());
                return Ok(Some(expr));
            }
            Lex::Quote => {
                if let Some(quoted) = parse(tokens, src)? {
                    return Ok(Some(Expr::Quote(Box::new(quoted))));
                }
            }
        }
    }
    Err(format!("Did not expect to end so soon in: '{src}'"))
}


#[cfg(test)]
pub mod tests {
    use crate::builder::build;
    use super::*;

    #[test]
    fn simple_value() {
        assert_eq!(parser("x"), Ok(build::symbol("x")));
        assert_eq!(parser("12"), Ok(Expr::IntAtom(12)));
        assert_eq!(parser("1.2"), Ok(Expr::FloatAtom(1.2)));
        assert_eq!(parser("true"), Ok(Expr::BoolAtom(true)));
    }

    #[test]
    fn s_expr() {
        assert_eq!(parser("()"), Ok(build::s_expr(vec![])));
        assert_eq!(parser("(x)"), Ok(
            build::s_expr(vec![build::symbol("x")])
        ));
        assert_eq!(parser("(x (y))"), Ok(
            build::s_expr(vec![
                build::symbol("x"),
                build::s_expr(vec![build::symbol("y")]),
            ])
        ));
    }

    #[test]
    fn quote() {
        assert_eq!(parser("'a"), Ok(build::quote(build::symbol("a"))));
    }

    //todo Test error cases
}