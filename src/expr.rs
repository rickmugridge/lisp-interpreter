use std::fmt;
use std::ops::Deref;
use std::string::ToString;
use crate::environment::Environment;
use crate::builder::build;
use crate::builtin_functions::functions;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IntAtom(isize),
    FloatAtom(f64),
    BoolAtom(bool),
    StringAtom(String),
    Symbol(String),
    SExpr(Vec<Expr>),
    Lambda(Vec<String>, Box<Expr>, Environment),
    Cons(Box<Expr>, Box<Expr>),
    // Cons is only ever created from builtin (cons ...)
    Nil,
    Sequence(Vec<Expr>),
    Quote(Box<Expr>),
}

impl Expr {
    pub fn debugging(_s: String) {
        // println!("{_s}");
    }

    fn debugging_with_env(_s: String, _env: &Environment) {
        // println!("{_s}");
        // println!("   with closure {_env}")
    }

    pub fn eval(&self, env: &Environment) -> Result<Expr, String> {
        match self {
            Expr::IntAtom(i) => Ok(Expr::IntAtom(*i)),
            Expr::FloatAtom(i) => Ok(Expr::FloatAtom(*i)),
            Expr::BoolAtom(i) => Ok(Expr::BoolAtom(*i)),
            Expr::StringAtom(_) => Ok(self.clone()),
            Expr::Symbol(s) => {
                match env.get(&s.clone()) {
                    Some(e) => Ok(e),
                    None => Ok(Expr::Symbol(s.clone()))
                }
            }
            Expr::Quote(v) => Ok(*v.clone()),
            Expr::SExpr(args) => {
                if args.is_empty() {
                    return Ok(Expr::SExpr(args.clone())); // todo Is this correct semantics?
                }
                let expr = args[0].eval(env)?;
                match expr {
                    Expr::IntAtom(_) => Err("Cannot use int as function".to_string()),
                    Expr::FloatAtom(_) => Err("Cannot use float as function".to_string()),
                    Expr::BoolAtom(_) => Err(format!("Cannot use bool as function in '{self}'")),
                    Expr::StringAtom(_) => Err(format!("Cannot use string as function in '{self}'")),
                    Expr::Symbol(symbol) => functions::eval_builtin(symbol, args, env),
                    Expr::SExpr(_prefix) => Err("Cannot use SExpr as the name of a function".to_string()),
                    Expr::Lambda(parameters, body, closure) =>
                        Self::eval_lambda(env, &args, parameters, body, closure),
                    Expr::Cons(_left, _right) => Err("Cannot use Cons as function".to_string()),
                    Expr::Sequence(_) => Err("Cannot use Sequence as function".to_string()),
                    Expr::Nil => Err("Cannot use Nil as function".to_string()),
                    Expr::Quote(_) => Err("Cannot use Quote as function".to_string()),
                }
            }
            Expr::Lambda(_args, _body, _closure) => Err("Cannot use Lambda as function".to_string()),
            Expr::Cons(_left, _right) => Ok(self.clone()),
            Expr::Nil => Ok(self.clone()),
            Expr::Sequence(seq) => Expr::eval_sequence(seq, env),
        }
    }

    pub fn eval_sequence(seq: &[Expr], env: &Environment) -> Result<Expr, String> {
        assert!(seq.len() > 0);
        for e in &seq[0..seq.len() - 1] {
            e.eval(env)?;
        }
        seq.last().unwrap().eval(env)
    }

    pub fn make_primitive_functions(env: &Environment) { // todo Move these utilities into mod builder
        env.set("else".to_string(), Expr::BoolAtom(true));
        env.set("<=".to_string(), build::not_lambda(">", env.clone()));
        env.set(">=".to_string(), build::not_lambda("<", env.clone()));
        env.set("abs".to_string(), build::abs(env.clone()));
        env.set("nil".to_string(), Expr::Nil);
    }

    pub(crate) fn eval_let(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("let", args, 2)?;
        if let Expr::SExpr(lets) = &args[0] {
            let new_env = &env.push();
            for var_expr_pair in lets {
                if let Expr::SExpr(var_definition) = var_expr_pair {
                    if let Expr::Symbol(name) = &var_definition[0] {
                        new_env.set(name.to_string(), var_definition[1].eval(new_env)?)
                    } else {
                        return Err(format!("Let: Expected name for (var expr) pair, but got {}", var_expr_pair));
                    }
                } else {
                    return Err(format!("Let: Expected SExpr of (var expr), but got {}", var_expr_pair));
                }
            }
            args[1].eval(new_env)
        } else {
            Err(format!("Let: Expected SExpr of (var expr) pairs, but got {}", &args[0]))
        }
    }

    pub(crate) fn eval_literal_lambda(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() < 2 {
            return Err(format!("lambda requires at least 2 arguments, but got: {}",
                               Expr::display_expressions(args)));
        }
        if let Expr::SExpr(ref parameters) = args[0] {
            let parameter_names = Expr::gather_parameters(&parameters[0..], env)?;
            let body = Expr::make_body(&args[1..]);
            let lambda = Expr::Lambda(parameter_names, body, env.clone());
            Expr::debugging(format!("    eval_literal_lambda gives: {lambda}"));
            Ok(lambda)
        } else {
            Err(format!("Literal lambda needs SExpr for parameters, but got {}", args[0]))
        }
    }

    fn make_body(bodies: &[Expr]) -> Box<Expr> {
        let body = if bodies.len() == 1 {
            bodies[0].clone()
        } else {
            let seq: Vec<Expr> = bodies.iter().map(|e| e.clone()).collect();
            Expr::Sequence(seq)
        };
        Box::new(body)
    }

    fn eval_lambda(env: &Environment,
                   args: &&Vec<Expr>,
                   parameters: Vec<String>,
                   body: Box<Expr>,
                   closure: Environment) -> Result<Expr, String> {
        Expr::debugging_with_env(format!("75 eval_lambda args: ({}), parameters: ({:?}), body: {}",
                                         Expr::display_expressions(&args[1..]), parameters, body), &closure);
        if parameters.len() != args.len() - 1 {
            return Err(format!("Cannot call Lambda as not right number of args: {}",
                               Expr::Lambda(parameters, body, closure)));
        }
        let new_env = &closure.push();
        for name_value_pair in parameters.iter().zip(&args[1..]) {
            let (name, value) = name_value_pair;
            let value = value.eval(env)?.clone();
            new_env.set(name.to_string(), value);
        }
        Expr::debugging(format!("   with new_env {new_env}"));
        body.eval(new_env)
    }

    pub(crate) fn eval_define_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::debugging(format!("  222 eval_define_expr args: ({})", Self::display_expressions(args)));
        if args.len() < 2 {
            Err(format!("define needs at least 2 arguments, but got {}", args.len())) // todo Reconsider this
        } else {
            match &args[0] {
                Expr::Symbol(s) => Self::define_var(s.to_string(), args, env),
                Expr::SExpr(header) => Self::define_function(header, &args[1..], env),
                _ => Err("Define: first argument must evaluate to a symbol".to_string()),
            }
        }
    }

    fn define_var(s: String, args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let value = args[1].eval(env)?;
        env.set(s, value.clone());
        Ok(value)
    }

    pub fn define_function(header: &Vec<Expr>, bodies: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::debugging(format!("    define_function: header: ({}) body:{} env: {env}",
                                Self::display_expressions(header), Self::display_expressions(bodies)));
        if header.is_empty() {
            Err(format!("define header needs at least 1 argument, but got {}", header.len())) // todo Reconsider this
        } else {
            match header[0].eval(env)? {
                Expr::Symbol(name) => {
                    let parameters = Expr::gather_parameters(&header[1..], env)?;
                    let body = Expr::make_body(bodies);
                    let lambda = Expr::Lambda(parameters, body, env.clone());
                    Expr::debugging(format!("       adding to env: {} -> {}", &name, &lambda));
                    env.set(name, lambda.clone());
                    Ok(lambda)
                }
                _ => Err("Function parameter must be a Symbol (or eval to a Symbol)".to_string())
            }
        }
    }

    pub fn check_args_count(fun: &str, args: &[Expr], expected_count: usize) -> Result<(), String> {
        if args.len() != expected_count {
            Err(format!("{fun} requires {expected_count} arguments, but got: {}",
                        Expr::display_expressions(args)))
        } else {
            Ok(())
        }
    }

    pub fn display_expressions(args: &[Expr]) -> String {
        args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(" ")
    }

    fn gather_parameters(header: &[Expr], env: &Environment) -> Result<Vec<String>, String> {
        let mut parameters: Vec<String> = Vec::new();
        for arg in header {
            if let Expr::Symbol(parameter) = arg.eval(env)? {
                parameters.push(parameter);
            } else {
                return Err(format!("Function parameter has to be a Symbol, but got '{arg}'"));
            }
        };
        Ok(parameters)
    }

    fn format_expressions(f: &mut fmt::Formatter, expressions: &Vec<Expr>, bracketed: bool) -> fmt::Result {
        if !expressions.is_empty() {
            if bracketed { write!(f, "(")?; }
            write!(f, "{}", expressions[0])?;
            for arg in &expressions[1..] {
                write!(f, " {}", arg)?;
            }
            if bracketed { write!(f, ")")?; }
        } else if bracketed {
            write!(f, "()")?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    // todo Change to pretty print
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::IntAtom(i) => write!(f, "{}", i),
            Expr::FloatAtom(x) => write!(f, "{}", x),
            Expr::BoolAtom(b) => write!(f, "{}", b),
            Expr::StringAtom(b) => write!(f, "\"{}\"", b),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::SExpr(args) =>
                Expr::format_expressions(f, args, true),
            Expr::Lambda(parameters, body, _) => {
                write!(f, "(_lambda_ ({}) {})", parameters.join(" "), body)
            }
            Expr::Cons(left, right) => fmt_cons(f, left, right),
            Expr::Nil => write!(f, "nil"),
            Expr::Sequence(seq) =>
                Expr::format_expressions(f, seq, false),
            Expr::Quote(expr) => write!(f, "'{}", expr),
        }
    }
}

fn fmt_cons(f: &mut fmt::Formatter, left: &Box<Expr>, right: &Box<Expr>) -> fmt::Result {
    let mut list = vec![left.deref()];
    let mut remainder = right;
    loop {
        match remainder.deref() {
            Expr::Cons(car, rest) => {
                list.push(car.deref());
                remainder = rest;
            }
            Expr::Nil => {
                write!(f, "(list")?;
                for element in list {
                    write!(f, " {}", element)?;
                }
                return write!(f, ")");
            }
            _ => break,
        }
    }
    write!(f, "(cons {left} {right})")
}

#[cfg(test)]
pub mod tests {
    use crate::test_evaluator::Evaluator;
    use super::*;

    #[test]
    fn display() {
        assert_eq!(&Expr::IntAtom(23).to_string(), "23");
        assert_eq!(&build::symbol("abc").to_string(), "abc");
        assert_eq!(&build::s_expr(vec![]).to_string(), "()");
        assert_eq!(&build::s_expr(vec![Expr::IntAtom(23), Expr::IntAtom(23)]).to_string(), "(23 23)");
        assert_eq!(&build::s_expr(vec![
            Expr::Symbol("+".to_string()),
            Expr::IntAtom(23),
            Expr::IntAtom(23)]
        ).to_string(), "(+ 23 23)");
        let x = "x".to_string();
        let y = "y".to_string();
        let body = build::s_expr(vec![
            Expr::Symbol("+".to_string()),
            Expr::Symbol(x.clone()),
            Expr::Symbol(y.clone())]
        );
        let fun = &Expr::Lambda(vec![x, y], Box::new(body),
                                Environment::new());
        assert_eq!(fun.to_string(), "(_lambda_ (x y) (+ x y))");
        let seq = &Expr::Sequence(vec![Expr::IntAtom(23), Expr::IntAtom(24)]);
        assert_eq!(seq.to_string(), "23 24");
        let quote = build::quote(build::symbol("a"));
        assert_eq!(quote.to_string(), "'a");
        assert_eq!(Expr::StringAtom("abc".to_string()).to_string(), "\"abc\"");
    }

    #[test]
    fn primitive_values() {
        Evaluator::new()
            .eval_to_self("23")
            .eval_to_self("23.4")
            .eval_to_self("true")
            .eval_to_self("+");
    }

    #[test]
    fn define() {
        Evaluator::new()
            .eval_to("(define x 22)", "22")
            .eval_to("x", "22");
    }

    #[test]
    fn lambda() {
        Evaluator::new()
            .eval_to("( (lambda (x) (+ x 1)) 2)", "3");
    }

    #[test]
    fn lambda_multiple_body() {
        Evaluator::new()
            .eval_to("( (lambda (x) (new-line) (+ x 1)) 2)", "3");
    }

    #[test]
    fn define_multiple_body() {
        Evaluator::new()
            .eval_to("( (define (f x) (new-line) (+ x 1)) 2)", "3");
    }

    #[test]
    fn lambda_nested() {
        Evaluator::new()
            .eval("(define (f x) (lambda (y) (+ x y)))")
            .eval_to("( (f 5) 2)", "7");
    }

    #[test]
    fn lambda_nested_twice() {
        Evaluator::new()
            .eval_to("(((lambda (y) (lambda (x) (+ x y))) 5) 2)", "7");
    }

    #[test]
    fn lambda_as_parameter() {
        Evaluator::new()
            .eval("(define (f g) (g 9))")
            .eval("(define (g x) (+ x 1))")
            .eval_to("(f g)", "10");
    }

    #[test]
    fn closure1() {
        Evaluator::new()
            .eval("(define v 5)")
            .eval("(define (f) v)")
            .eval("(define (g fn) (fn))")
            .eval_to("(g f)", "5");
    }

    #[test]
    fn closure2() {
        Evaluator::new()
            .eval("(define v 5)")
            .eval("(define (f x) (+ x v))")
            .eval("(define (g fn w) (fn w))")
            .eval_to("(g f 1)", "6");
    }

    #[test]
    fn let_() {
        Evaluator::new()
            .eval_to("(let ((x 1) (y 2)) (+ x y))", "3");
    }

    #[test]
    fn let_with_dependencies() {
        Evaluator::new()
            .eval_to("(let ((x 1) (y x)) (+ x y))", "2");
    }

    #[test]
    fn quote() {
        Evaluator::new()
            .eval("(define a 32)")
            .eval_to("(cons 'a a)", "(cons a 32)")
            .eval_to("(list 'a a)", "(list a 32)")
            .eval_to("'(list a a a)", "(list a a a)")
            .eval_to("'(list 'a a)", "(list 'a a)")
            .eval_to("'(cons 'a '(a nil))", "(cons 'a '(a nil))"); // todo check this
    }
}
