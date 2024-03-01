pub mod functions {
    use crate::bool_expression::bool_expr;
    use crate::builtin_functions::functions;
    use crate::cond::conditional;
    use crate::cons_expression::cons_expression;
    use crate::environment::Environment;
    use crate::expr::Expr;
    use crate::number_expression::number_expr;
    use crate::mutate_expression::mutate_expression;

    pub fn eval_builtin(symbol: String, args: &[Expr], env: &Environment) -> Result<Expr, String> {
        match symbol.to_lowercase().as_str() {
            "and" => bool_expr::eval_and_expr(&args[1..], env),
            "or" => bool_expr::eval_or_expr(&args[1..], env),
            "not" => bool_expr::eval_not_expr(&args[1..], env),
            "+" => number_expr::eval_plus(&args[1..], env),
            "-" => number_expr::eval_minus(&args[1..], env),
            "*" => number_expr::eval_multiply(&args[1..], env),
            "/" => number_expr::eval_divide(&args[1..], env),
            "<" => number_expr::eval_less_than(&args[1..], env),
            ">" => number_expr::eval_greater_than(&args[1..], env),
            "=" => number_expr::eval_equals(&args[1..], env),
            "min" => number_expr::eval_min(&args[1..], env),
            "max" => number_expr::eval_max(&args[1..], env),
            "remainder" => number_expr::eval_remainder(&args[1..], env),
            "number?" => number_expr::eval_is_number(&args[1..], env),

            "cond" => conditional::eval_cond_expr(&args[1..], env),
            "if" => conditional::eval_if_expr(&args[1..], env),

            "define" => Expr::eval_define_expr(&args[1..], env),
            "lambda" => Expr::eval_literal_lambda(&args[1..], env),
            "let" => Expr::eval_let(&args[1..], env),

            "cons" => cons_expression::eval_cons(&args[1..], env),
            "car" => cons_expression::eval_car(&args[1..], env),
            "cdr" => cons_expression::eval_cdr(&args[1..], env),
            "list" => cons_expression::eval_list(&args[1..], env),
            "null?" => cons_expression::eval_is_null(&args[1..], env),
            "pair?" => cons_expression::eval_is_pair(&args[1..], env),

            "quote" => functions::eval_quote(&args[1..], env),
            "display" => functions::eval_display(&args[1..], env),
            "new-line" => functions::eval_new_line(&args[1..]),
            "eq?" => functions::eval_symbol_equals(&args[1..], env),
            "equal?" => functions::eval_equal(&args[1..], env),
            "symbol?" => functions::eval_is_symbol(&args[1..], env),
            "error" => functions::eval_error(&args[1..], env),

            "set!" => mutate_expression::eval_set(&args[1..], env),
            "set-car!" => mutate_expression::eval_set_car(&args[1..], env),
            "set-cdr!" => mutate_expression::eval_set_cdr(&args[1..], env),

            other => {
                match env.get(other) {
                    Some(e) => Ok(e),
                    None => Err(format!("Unknown symbol: '{}'", other)),
                }
            }
        }
    }

    pub fn eval_quote(args: &[Expr], _env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("quote", args, 1)?;
        Ok(Expr::Quote(Box::new(args[0].clone())))
    }

    pub fn eval_display(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("display", args, 1)?;
        let value = args[0].eval(env)?;
        print!("--display------> {}", &value);
        Ok(value.clone())
    }

    pub fn eval_new_line(args: &[Expr]) -> Result<Expr, String> {
        Expr::check_args_count("new-line", args, 0)?;
        println!();
        Ok(Expr::BoolAtom(true))
    }

    pub fn eval_symbol_equals(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("eq?", args, 2)?;
        let value1 = &args[0].eval(env)?;
        let value2 = &args[1].eval(env)?;
        match (value1, value2) {
            (Expr::Symbol(s1), Expr::Symbol(s2)) =>
                Ok(Expr::BoolAtom(s1 == s2)),
            _ => Err(format!("eq? requires 2 Symbols, but got: {value1} and {value2}"))
        }
    }

    pub fn eval_is_symbol(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("symbol?", args, 1)?;
        let value = &args[0].eval(env)?;
        let sym = match value {
            Expr::Symbol(_) => true,
            Expr::Quote(q) => match **q {
                Expr::Symbol(_) => true,
                _ => false,
            },
            _ => false,
        };
        Ok(Expr::BoolAtom(sym))
    }

    pub fn eval_equal(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("symbol?", args, 2)?;
        Ok(Expr::BoolAtom(pair_is_equal(&args[0].eval(env)?, &args[1].eval(env)?)))
    }

    fn pair_is_equal(value1: &Expr, value2: &Expr) -> bool {
        match (value1, value2) {
            (Expr::IntAtom(s1), Expr::IntAtom(s2)) => s1 == s2,
            (Expr::FloatAtom(s1), Expr::FloatAtom(s2)) => s1 == s2,
            (Expr::BoolAtom(s1), Expr::BoolAtom(s2)) => s1 == s2,
            (Expr::Symbol(s1), Expr::Symbol(s2)) => s1 == s2,
            (Expr::SExpr(s1), Expr::SExpr(s2)) => {
                s1.len() == s2.len() && s1.iter().zip(s2.iter()).all(|(a, b)|
                    functions::pair_is_equal(a, b))
            }
            (Expr::Cons(ref h1, ref t1),
                Expr::Cons(ref h2, ref t2)) =>
                pair_is_equal(h1, h2) & pair_is_equal(t1, t2),
            (Expr::Nil, Expr::Nil) => true,
            (Expr::Sequence(s1), Expr::Sequence(s2)) => {
                s1.len() == s2.len() && s1.iter().zip(s2.iter()).all(|(a, b)|
                    functions::pair_is_equal(a, b))
            }
            (Expr::Quote(ref s1), Expr::Quote(ref s2)) =>
                pair_is_equal(s1, s2),
            _ => false
        }
    }

    pub fn eval_error(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("error", args, 1)?;
        let expr = args[0].eval(env)?;
        Err(format!("Error thrown: '{expr}'"))
    }
}

#[cfg(test)]
mod built_in_tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn quote_1() {
        Evaluator::new()
            .eval_to("'a", "a")
            .eval_to("(list 'let)", "(list let)");
    }

    #[test]
    fn quote() {
        Evaluator::new()
            .eval_to("'a", "a")
            .eval_to("'()", "()") // todo Should be Nil
            .eval_to("'(let x (3))", "(let x (3))")
            .eval_to("(list 'display '\"hello\")", "(display \"hello\")")
            .eval_to("(list 'let)", "(list let)")
            .eval_to("(list 'let (list (list 'x '1)) (list '* '2 'x))",
                     "(let ((x 1)) (* 2 x))");
        // todo Above is not right according to http://www.phyast.pitt.edu/~micheles/scheme/scheme8.html
         Evaluator::new()
            .eval("(define a (quote a))")
            .eval_to("a", "'a");
        Evaluator::new()
            .eval("(define a (quote (1 2)))")
            .eval_to("a", "'(1 2)");
    }

    #[test]
    fn display_() {
        Evaluator::new()
            .eval("(display a)")
            .eval("(new-line)")
            .eval("(display (/ 1.0 2))");
    }

    #[test]
    fn symbol_equals() {
        Evaluator::new()
            .eval_to("(eq? a a)", "true")
            .eval_to("(eq? a b)", "false");
    }

    #[test]
    fn is_symbol() {
        Evaluator::new()
            .eval_to("(symbol? a)", "true")
            .eval_to("(symbol? 1)", "false");
    }

    #[test]
    fn equal() {
        Evaluator::new()
            .eval_to("(equal? a a)", "true")
            .eval_to("(equal? 'a 'a)", "true")
            .eval_to("(equal? 1 1)", "true")
            .eval_to("(equal? 1.4 1.4)", "true")
            .eval_to("(equal? false false)", "true")
            .eval_to("(equal? nil nil)", "true")
            .eval_to("(equal? (list 1 2 3) (list 1 2 3))", "true")
            .eval_to("(equal? '(list 1 2 (list 3 4)) '(list 1 2 (list 3 4)))", "true")

            .eval_to("(equal? a b)", "false")
            .eval_to("(equal? 'a 'b)", "false")
            .eval_to("(equal? 1 2)", "false")
            .eval_to("(equal? 1.4 2.4)", "false")
            .eval_to("(equal? false true)", "false")
            .eval_to("(equal? nil 3)", "false")
            .eval_to("(equal? (list 1 2 3) (list 1 2 4))", "false")
            .eval_to("(equal? '(list 1 2 (list 3 4)) '(list 1 2 (list 3 5)))", "false")
            .eval_to("(equal? (lambda (a) a) (lambda (a) a))", "false");
    }

    #[test]
    fn error() {
        assert_eq!(Evaluator::new().try_eval("(error problem)"), Err("Error thrown: 'problem'".to_string()));
    }
}