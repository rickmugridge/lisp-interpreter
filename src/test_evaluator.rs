use crate::environment::Environment;
use crate::expr::Expr;
use crate::parse;

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Self { env: Environment::new() }
    }

    pub fn eval(&mut self, src: &str) -> &mut Self {
        Evaluator::evaluates(src, &self.env);
        self
    }

    pub fn eval_all(&mut self, srcs: Vec<&str>) -> &mut Self {
        srcs.iter().for_each(|src| Evaluator::evaluates(src, &self.env));
        self
    }

    pub fn eval_to(&mut self, src: &str, expected: &str) -> &mut Self {
        let env = &self.env;
        let expr = &parse::parser(src).unwrap();
        match expr.eval(env) {
            Ok(given) => assert_eq!(given.to_string(), expected),
            Err(message) => panic!("{message}")
        }
        self
    }

    pub fn eval_to_self(&mut self, src: &str) -> &mut Self {
        Evaluator::eval_to(self, src, src)
    }

    pub fn evaluates(src: &str, env: &Environment) {
        parse::parser(src).unwrap().eval(env).unwrap();
    }

    pub fn try_eval(&mut self, src: &str) -> Result<Expr, String>{
        parse::parser(src).unwrap().eval(&self.env)
    }
}
