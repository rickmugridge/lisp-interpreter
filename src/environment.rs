use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    env: Rc<Env>,
}

// Unfortunately, we need to use a RefCell here because rust
// can't handle lifetimes through the recursion without it.
#[derive(Debug, Clone, PartialEq)]
struct Env {
    previous: Option<Environment>,
    map: RefCell<HashMap<String, Expr>>,
}

impl Environment {
    pub fn new() -> Self {
        let result = Self {
            env: Rc::new(Env {
                previous: None,
                map: RefCell::new(HashMap::new()),
            })
        };
        Expr::make_primitive_functions(&result);
        result
    }

    pub fn push(&self) -> Self {
        Self {
            env: Rc::new(Env {
                previous: Some(self.clone()),
                map: RefCell::new(HashMap::new()),
            })
        }
    }

    pub fn set_replace(&self, symbol: String, e: Expr) -> Result<(), String> {
        let mut map = self.env.map.borrow_mut();
        match map.get(&symbol) {
            Some(_) => {
                map.insert(symbol, e);
                Ok(())
            }
            None => match &(self.env).previous {
                None => Err(format!("Cannot set! {} as not defined in any environment", symbol)),
                Some(p) => p.set_replace(symbol, e)
            }
        }
    }

    pub fn set(&self, symbol: String, e: Expr) {
        self.env.map.borrow_mut().insert(symbol, e);
    }

    pub fn get(&self, key: &str) -> Option<Expr> {
        match self.env.map.borrow().get(key) {
            Some(e) => Some(e.clone()),
            None => match &(self.env).previous {
                None => None,
                Some(p) => p.get(key)
            }
        }
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Environment(\n")?;
        for (key, expr) in self.env.map.borrow().iter() {
            write!(f, "  \"{}\" -> {}\n", key, expr)?;
        }
        if let Some(p) = &self.env.previous {
            <dyn fmt::Display>::fmt(&p, f)?;
        }
        write!(f, ")\n")
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_state() {
        let env = &mut Environment::new();
        assert_eq!(env.get("a"), None);
    }

    #[test]
    fn add() {
        let env = &mut Environment::new();
        let expr = Expr::IntAtom(23);
        env.set("a".to_string(), expr.clone());
        assert_eq!(env.get("a"), Some(expr));
    }

    #[test]
    fn push_and_pop() {
        let env = &mut Environment::new();
        let expr23 = Expr::IntAtom(23);
        let expr24 = Expr::IntAtom(24);
        let a = "a".to_string();
        env.set(a.clone(), expr23.clone());

        let new_env = env.push();
        new_env.set(a.clone(), expr24.clone());
        assert_eq!(new_env.get(&a), Some(expr24));

        assert_eq!(env.get(&a), Some(expr23));
        // println!("env: {env}\n");
        println!("new_env: {new_env}");
    }

    #[test]
    fn env() {
        let env = &Environment::new();
        env.set("key".to_string(), Expr::IntAtom(23));
        assert_eq!(env.get("key"), Some(Expr::IntAtom(23)));
        assert_eq!(env.get("key2"), None);
        let new_env = env.push();
        assert_eq!(new_env.get("key"), Some(Expr::IntAtom(23)));
        assert_eq!(new_env.get("key2"), None);
        new_env.set("key2".to_string(), Expr::IntAtom(50));
        assert_eq!(new_env.get("key"), Some(Expr::IntAtom(23)));
        assert_eq!(new_env.get("key2"), Some(Expr::IntAtom(50)));
        assert_eq!(env.get("key2"), None);

        let new_env2 = env.push();
        assert_eq!(new_env2.get("key"), Some(Expr::IntAtom(23)));
        assert_eq!(new_env2.get("key2"), None);
        new_env2.set("key2".to_string(), Expr::IntAtom(100));
        assert_eq!(new_env2.get("key"), Some(Expr::IntAtom(23)));
        assert_eq!(new_env2.get("key2"), Some(Expr::IntAtom(100)));
        assert_eq!(new_env.get("key2"), Some(Expr::IntAtom(50)));
        assert_eq!(env.get("key2"), None);
    }
}