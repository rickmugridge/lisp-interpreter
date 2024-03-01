pub mod mutate_expression {
    use crate::environment::Environment;
    use crate::expr::Expr;

    pub fn eval_set(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("set!", args, 2)?;
        if let Expr::Symbol(name) = &args[0] {
            let value = args[1].eval(env)?;
            env.set_replace(name.clone(), value)?;
            Ok(Expr::Nil)
        } else {
            Err(format!("Expected a symbol as first argument of set! but got {}", &args[0]))
        }
    }

    pub fn eval_set_car(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("set-car!", args, 2)?;
        if let Expr::Symbol(name) = &args[0] {
            let cons = args[0].eval(env)?;
            if let Expr::Cons(_car, cdr) = cons {
                let value = args[1].eval(env)?;
                env.set_replace(name.clone(),
                                Expr::Cons(Box::new(value), cdr))?;
                Ok(Expr::Nil)
            } else {
                Err(format!("Expected a Cons in set-car! but got {}", cons))
            }
        } else {
            Err(format!("Expected a symbol as first argument of set-car! but got {}", &args[0]))
        }
    }

    pub fn eval_set_cdr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("set-cdr!", args, 2)?;
        if let Expr::Symbol(name) = &args[0] {
            let cons = args[0].eval(env)?;
            if let Expr::Cons(car, _cdr) = cons {
                let value = args[1].eval(env)?;
                env.set_replace(name.clone(),
                                Expr::Cons(car, Box::new(value)))?;
                Ok(Expr::Nil)
            } else {
                Err(format!("Expected a Cons in set-cdr! but got {}", cons))
            }
        } else {
            Err(format!("Expected a symbol as first argument of set-cdr! but got {}", &args[0]))
        }
    }
}

#[cfg(test)]
mod mutation_tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn set() {
        Evaluator::new()
            .eval("(define a 0)")
            .eval("(set! a 1)")
            .eval("(set! a 2)")
            .eval_to("a", "2");
    }

    #[test]
    fn set_car() {
        Evaluator::new()
            .eval("(define a (cons 1 2))")
            .eval("(set-car! a 10)")
            .eval_to("a", "(cons 10 2)");
    }

    #[test]
    fn set_cdr() {
        Evaluator::new()
            .eval("(define a (cons 1 2))")
            .eval("(set-cdr! a 10)")
            .eval_to("a", "(cons 1 10)");
    }
}