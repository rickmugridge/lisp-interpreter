mod environment;
mod expr;
mod numbers;
mod builder;
mod bool_expression;
mod number_expression;
mod mutate_expression;
mod lex;
mod parse;
mod test_evaluator;
mod lisp_exercises;
mod cons_expression;
mod builtin_functions;
mod cond;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(true);
    }
}
