pub mod build {
    use crate::environment::Environment;
    use crate::expr::Expr;

    pub fn s_expr(args: Vec<Expr>) -> Expr {
        Expr::SExpr(args)
    }

    pub fn symbol(s: &str) -> Expr {
        Expr::Symbol(s.to_string())
    }

    pub fn quote(expr: Expr) -> Expr {
        Expr::Quote(Box::new(expr))
    }

    pub fn not_lambda(relation: &str, closure: Environment) -> Expr {
        Expr::Lambda(vec!["x".to_string(), "y".to_string()], Box::new(
            s_expr(vec![
                symbol("not"),
                s_expr(vec![
                    symbol(relation),
                    symbol("x"),
                    symbol("y"),
                ]),
            ])), closure)
    }

    pub fn abs(closure: Environment) -> Expr {
        Expr::Lambda(vec!["x".to_string()], Box::new(
            s_expr(vec![
                symbol("if"),
                s_expr(vec![
                    symbol("<"),
                    symbol("x"),
                    Expr::IntAtom(0),
                ]),
                s_expr(vec![
                    symbol("-"),
                    Expr::IntAtom(0),
                    symbol("x"),
                ]),
                symbol("x"),
            ])), closure)
    }
}
