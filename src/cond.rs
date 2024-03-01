pub mod conditional {
    use crate::environment::Environment;
    use crate::expr::Expr;

    pub fn eval_cond_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::debugging(format!("cond({})", Expr::display_expressions(args)));
        if args.is_empty() {
            Err("cond needs > 0 arguments".to_string()) // todo Reconsider this
        } else {
            for condition in args {
                match condition {
                    Expr::SExpr(pair) => {
                        if pair.len() < 2 {
                            return Err(format!("condition needs 2 arguments, but got {}", pair.len())); // todo Reconsider this
                        }
                        let boolean_result = pair[0].eval(env)?;
                        if let Expr::BoolAtom(passed) = boolean_result {
                            if passed { return Expr::eval_sequence(&pair[1..], env); }
                            // if passed { return pair[1].eval(env); }
                        } else {
                            return Err(format!("condition needs to evaluate to a boolean: '{boolean_result}'")); // todo Reconsider this
                        }
                    }
                    _ => return Err("cond: first argument of a condition must evaluate to a boolean".to_string()),
                }
            }
            Err("cond: returned no value".to_string())
        }
    }

    pub fn eval_if_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("if", args, 3)?;
        Expr::debugging(format!("(if {})", Expr::display_expressions(args)));
        let cond = args[0].eval(env)?;
        if let Expr::BoolAtom(passed) = cond {
            if passed { args[1].eval(env) } else { args[2].eval(env) }
        } else {
            Err(format!("if needs to evaluate to a boolean: '{cond}'"))
        }
    }
}

#[cfg(test)]
mod number_tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn cond() {
        Evaluator::new()
            .eval_to("(cond (true 2))", "2")
            .eval_to("(cond (false 1) (true 2))", "2");
    }

    #[test]
    fn cond_else() {
        Evaluator::new()
            .eval_to("(cond (false 1) (else 2))", "2");
    }

    #[test]
    fn if_expression() {
        Evaluator::new()
            .eval("( define (forever) (forever))")
            .eval_to("(if false (forever) 2)", "2")
            .eval_to("(if true 1 (forever))", "1");
    }
}