pub mod bool_expr {
    use crate::expr::Expr;
    use crate::environment::Environment;

    pub fn eval_and_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        for arg in args {
            match arg.eval(env)? {
                Expr::BoolAtom(true) => {}
                Expr::BoolAtom(false) => { return Ok(Expr::BoolAtom(false)); }
                non_bool => return Err(format!("and: Cannot use a non-boolean value: '{non_bool}'"))
            }
        }
        Ok(Expr::BoolAtom(true))
    }

    pub fn eval_or_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        for arg in args {
            match arg.eval(env)? {
                Expr::BoolAtom(true) => { return Ok(Expr::BoolAtom(true)); }
                Expr::BoolAtom(false) => {}
                non_bool => return Err(format!("or: Cannot use a non-boolean value: '{non_bool}'"))
            }
        }
        Ok(Expr::BoolAtom(false))
    }

    pub fn eval_not_expr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("not: Only 1 argument is allowed".to_string());
        }
        match args[0].eval(env)? {
            Expr::BoolAtom(b) => Ok(Expr::BoolAtom(!b)),
            non_bool => return Err(format!("Cannot use a non-boolean value: '{non_bool}'"))
        }
    }
}

#[cfg(test)]
mod bool_tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn and_expression() {
        Evaluator::new()
            .eval_to("(and false false)", "false")
            .eval_to("(and true false)", "false")
            .eval_to("(and false true)", "false")
            .eval_to("(and true true)", "true");
    }

    #[test]
    fn or_expression() {
        Evaluator::new()
            .eval_to("(or false false)", "false")
            .eval_to("(or true false)", "true")
            .eval_to("(or false true)", "true")
            .eval_to("(or true true)", "true");
    }

    #[test]
    fn not_expression() {
        Evaluator::new()
            .eval_to("(not false)", "true")
            .eval_to("(not true)", "false");
    }
}