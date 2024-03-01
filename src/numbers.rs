use std::ops::Range;
use crate::expr::Expr;
use crate::environment::Environment;

pub enum Numbers {
    Integers(Vec<isize>),
    Floats(Vec<f64>),
}

impl Numbers {
    pub fn eval(args: &[Expr], valid_range: Range<usize>, env: &Environment) -> Result<Self, String> {
        let mut eval_args: Vec<Expr> = Vec::new();
        let mut contains_float = false;
        for arg in args.iter() {
            let value = arg.eval(env)?;
            match value {
                Expr::FloatAtom(_) => { contains_float = true; }
                Expr::IntAtom(_) => {}
                not_numeric => return Err(format!("Cannot use a non-numeric value: '{not_numeric}'"))
            };
            eval_args.push(value);
        }
        if !valid_range.contains(&eval_args.len()) {
            return Err(format!("Needed args length: {:?} but was {}", valid_range, eval_args.len()));
        }
        if contains_float {
            let floats = eval_args.into_iter().map(Numbers::select_float).collect();
            Ok(Numbers::Floats(floats))
        } else {
            let ints = eval_args.into_iter().map(Numbers::select_int).collect();
            Ok(Numbers::Integers(ints))
        }
    }

    fn select_int(e: Expr) -> isize {
        if let Expr::IntAtom(i) = e { i } else { unreachable!() }
    }

    fn select_float(e: Expr) -> f64 {
        match e {
            Expr::IntAtom(i) => i as f64,
            Expr::FloatAtom(f) => f,
            _ => unreachable!()
        }
    }
}