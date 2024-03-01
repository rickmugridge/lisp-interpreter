pub mod number_expr {
    use crate::expr::Expr;
    use crate::environment::Environment;
    use crate::numbers::Numbers;

    pub fn eval_plus(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 2..100, env)? {
            Numbers::Integers(ints) => Expr::IntAtom(ints.iter().sum()),
            Numbers::Floats(floats) => Expr::FloatAtom(floats.iter().sum()),
        };
        Ok(result)
    }

    pub fn eval_minus(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 1..100, env)? {
            Numbers::Integers(ints) => {
                if ints.len() == 1 { return Ok(Expr::IntAtom(-ints[0])); }
                Expr::IntAtom(ints.iter().skip(1).fold(ints[0], |i, j| i - j))
            }
            Numbers::Floats(floats) => {
                if floats.len() == 1 { return Ok(Expr::FloatAtom(-floats[0])); }
                Expr::FloatAtom(floats.iter().skip(1).fold(floats[0], |i, j| i - j))
            }
        };
        Ok(result)
    }

    pub fn eval_multiply(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 2..100, env)? {
            Numbers::Integers(ints) => Expr::IntAtom(ints.iter().product()),
            Numbers::Floats(floats) => Expr::FloatAtom(floats.iter().product()),
        };
        Ok(result)
    }

    pub fn eval_divide(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let expr = match Numbers::eval(args, 2..3, env)? {
            Numbers::Integers(ints) =>
                Expr::IntAtom(ints.iter().skip(1).fold(ints[0], |i, j| i / j)),
            Numbers::Floats(floats) =>
                Expr::FloatAtom(floats.iter().skip(1).fold(floats[0], |i, j| i / j)),
        };
        Ok(expr)
    }

    pub fn eval_less_than(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 2..3, env)? {
            Numbers::Integers(ints) => ints[0] < ints[1],
            Numbers::Floats(floats) => floats[0] < floats[1],
        };
        Ok(Expr::BoolAtom(result))
    }

    pub fn eval_greater_than(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 2..3, env)? {
            Numbers::Integers(ints) => ints[0] > ints[1],
            Numbers::Floats(floats) => floats[0] > floats[1],
        };
        Ok(Expr::BoolAtom(result))
    }

    pub fn eval_equals(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let result = match Numbers::eval(args, 2..3, env)? {
            Numbers::Integers(ints) => {
                let result = ints[0] == ints[1];
                println!("= {} -> {}", ints_to_string(ints), result);
                result
            }
            Numbers::Floats(floats) => floats[0] == floats[1],
        };
        Ok(Expr::BoolAtom(result))
    }

    pub fn eval_remainder(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let expr = match Numbers::eval(args, 2..3, env)? {
            Numbers::Integers(ints) =>
                {
                    let result = Expr::IntAtom(ints.iter().skip(1).fold(ints[0], |i, j| i % j));
                    println!("remainder {} -> {}", ints_to_string(ints), result);
                    result
                }
            Numbers::Floats(floats) =>
                Expr::FloatAtom(floats.iter().skip(1).fold(floats[0], |i, j| i % j)),
        };
        Ok(expr)
    }

    pub fn eval_min(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let expr = match Numbers::eval(args, 2..50, env)? {
            Numbers::Integers(ints) =>
                Expr::IntAtom(ints.iter()
                    .skip(1)
                    .fold(ints[0], |i, j| i.min(*j))),
            Numbers::Floats(floats) =>
                Expr::FloatAtom(floats.iter()
                    .skip(1)
                    .fold(floats[0], |i, j| i.min(*j))),
        };
        Ok(expr)
    }

    pub fn eval_max(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        let expr = match Numbers::eval(args, 2..50, env)? {
            Numbers::Integers(ints) =>
                Expr::IntAtom(ints.iter()
                    .skip(1)
                    .fold(ints[0], |i, j| i.max(*j))),
            Numbers::Floats(floats) =>
                Expr::FloatAtom(floats.iter()
                    .skip(1)
                    .fold(floats[0], |i, j| i.max(*j))),
        };
        Ok(expr)
    }

    pub fn eval_is_number(args: &[Expr], env: &Environment) -> Result<Expr, String> {
        Expr::check_args_count("number?", args, 1)?;
        let value = &args[0].eval(env)?;
        let num = match value {
            Expr::IntAtom(_) | Expr::FloatAtom(_) => true,
            _ => false,
        };
        Ok(Expr::BoolAtom(num))
    }

    fn ints_to_string(ints: Vec<isize>) -> String {
        ints.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" ")
    }
}


#[cfg(test)]
mod number_tests {
    use crate::test_evaluator::Evaluator;

    #[test]
    fn plus_int() {
        Evaluator::new().eval_to("(+ 2 3)", "5");
    }

    #[test]
    fn plus_float() {
        Evaluator::new().eval_to("(+ 2 3.1)", "5.1");
    }

    #[test]
    fn minus_int_1() {
        Evaluator::new().eval_to("(- 2)", "-2");
    }

    #[test]
    fn minus_int_2() {
        Evaluator::new().eval_to("(- 2 3)", "-1");
    }

    #[test]
    fn minus_int_3() {
        Evaluator::new().eval_to("(- (- 2 3) 3)", "-4");
    }

    #[test]
    fn minus_float() {
        Evaluator::new().eval_to("(- (- 2 3.1) 3)", "-4.1");
    }

    #[test]
    fn multiply_int() {
        Evaluator::new().eval_to("(* (* 2 3) 3)", "18");
    }

    #[test]
    fn multiply_floats() {
        Evaluator::new().eval_to("(* (* 2 3) 3.1)", "18.6");
    }

    #[test]
    fn divide_int() {
        Evaluator::new().eval_to("(/ 7 3)", "2");
    }

    #[test]
    fn divide_floats() {
        Evaluator::new().eval_to("(/ (/ 20.1 4) 2)", "2.5125");
    }

    #[test]
    fn remainder_int() {
        Evaluator::new().eval_to("(remainder 7 3)", "1");
        Evaluator::new().eval_to("(remainder 7 7)", "0");
    }

    #[test]
    fn remainder_floats() {
        Evaluator::new().eval_to("(remainder 20.5 5.0)", "0.5");
    }

    #[test]
    fn min_int() {
        Evaluator::new().eval_to("(min 27 3 13)", "3");
    }

    #[test]
    fn min_floats() {
        Evaluator::new().eval_to("(min 27.3 3.1 13)", "3.1");
    }

    #[test]
    fn max_int() {
        Evaluator::new().eval_to("(max 27 3 130)", "130");
    }

    #[test]
    fn max_floats() {
        Evaluator::new().eval_to("(max 27.3 3.1 133.4)", "133.4");
    }

    #[test]
    fn less_than() {
        Evaluator::new()
            .eval_to("(< 3 4)", "true")
            .eval_to("(< 3 2)", "false");
    }

    #[test]
    fn greater_than() {
        Evaluator::new()
            .eval_to("(> 3 4)", "false")
            .eval_to("(> 3 2)", "true");
    }

    #[test]
    fn equals() {
        Evaluator::new()
            .eval_to("(= 3 4)", "false")
            .eval_to("(= 3 3)", "true");
    }

    #[test]
    fn less_than_or_equals() {
        Evaluator::new()
            .eval_to("(<= 3 4)", "true")
            .eval_to("(<= 3 3)", "true")
            .eval_to("(<= 4 3)", "false");
    }

    #[test]
    fn greater_than_or_equals() {
        Evaluator::new()
            .eval_to("(>= 3 4)", "false")
            .eval_to("(>= 3 3)", "true")
            .eval_to("(>= 4 3)", "true");
    }

    #[test]
    fn abs_int() {
        Evaluator::new()
            .eval_to("(abs 1)", "1")
            .eval_to("(abs (- 1))", "1");
    }

    #[test]
    fn abs_float() {
        Evaluator::new()
            .eval_to("(abs 1.1)", "1.1")
            .eval_to("(abs (- 1.1))", "1.1");
    }

    #[test]
    fn is_number() {
        Evaluator::new()
            .eval_to("(number? 1)", "true")
            .eval_to("(number? a)", "false");
    }
}