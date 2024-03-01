pub mod cons_expression {
    use crate::environment::Environment;
    use crate::expr::Expr;

    pub fn eval_cons(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err(format!("cons requires 2 arguments, but got: '{}'",
                               Expr::display_expressions(args)));
        }
        Ok(cons(args[0].eval(env)?, args[1].eval(env)?))
    }

    fn cons(arg1: Expr, arg2: Expr) -> Expr {
        Expr::Cons(Box::new(arg1), Box::new(arg2))
    }

    pub fn eval_car(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("car requires 1 argument, but got: '{}'",
                               Expr::display_expressions(args)));
        }
        match args[0].eval(env)? {
            Expr::Cons(left, _right) => Ok(*left.clone()),
            _ => Err(format!("car requires a Cons, but got: '{}'", args[0]))
        }
    }

    pub fn eval_cdr(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("cdr requires 1 argument, but got: '{}'",
                               Expr::display_expressions(args)));
        }
        match args[0].eval(env)? {
            Expr::Cons(_left, right) => Ok(*right.clone()),
            _ => Err(format!("cdr requires a Cons, but got: '{}'", args[0]))
        }
    }

    pub fn eval_list(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let mut last = Expr::Nil;
        for arg in args.iter().rev() {
            last = cons(arg.eval(env)?, last);
        }
        Ok(last)
    }

    pub fn eval_is_null(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("null? requires 1 argument, but got: '{}'",
                               Expr::display_expressions(args)));
        }
        match args[0].eval(env)? {
            Expr::Nil => Ok(Expr::BoolAtom(true)),
            Expr::Cons(_, _) => Ok(Expr::BoolAtom(false)),
            _ => Err(format!("null? requires a Cons, but got: '{}'", args[0]))
        }
    }

    pub fn eval_is_pair(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("pair? requires 1 argument, but got: '{}'",
                               Expr::display_expressions(args)));
        }
        match args[0].eval(env)? {
            Expr::Cons(_, _) => Ok(Expr::BoolAtom(true)),
            _ => Ok(Expr::BoolAtom(false)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn cons() {
        Evaluator::new()
            .eval("(define x (cons 1 2))")
            .eval("(define y (cons 3 x))")
            .eval_to("(car x)", "1")
            .eval_to("(cdr x)", "2")
            .eval_to("(car y)", "3")
            .eval_to("(car (cdr y))", "1")
            .eval_to("(cdr (cdr y))", "2")
            .eval_to("(cons 1 (cons 2 (cons 3 nil)))", "(list 1 2 3)")
            .eval_to("(list 1 2 3)", "(list 1 2 3)");
    }

    #[test]
    fn null() {
        Evaluator::new()
            .eval_to("(null? (list 1))", "false")
            .eval_to("(null? (list))", "true");
    }

    #[test]
    fn pair() {
        Evaluator::new()
            .eval_to("(pair? (list 1))", "true")
            .eval_to("(pair? (list))", "false")
            .eval_to("(pair? 1)", "false");
    }
}
