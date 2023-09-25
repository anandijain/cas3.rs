extern crate peg;
use ordered_float;
use rustyline::{DefaultEditor, Result};
use std::fmt;

peg::parser! {
    grammar expr_parser() for str {
        rule integer() -> Expr
            = n:$("-"? ['0'..='9']+ ) {? n.parse().map(Expr::Int).or(Err("integer")) }

        rule real() -> Expr
            = n:$("-"? ['0'..='9']* "." ['0'..='9']+ ) {? n.parse().map(Expr::Real).or(Err("real")) }

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule atom() -> Expr
            = real() / integer() / symbol()

        rule list() -> Expr
            = "(" l:Expr() ** " " ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = atom() / list()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Int(i64),
    Real(ordered_float::NotNan<f64>),
    Sym(String),
    List(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Real(r) => write!(f, "{}", r),
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::List(lst) => {
                let str_list: Vec<String> = lst.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", str_list.join(" "))
            }
        }
    }
}

fn head(expr: &Expr) -> String {
    match expr {
        Expr::Int(_) => "Int".to_string(),
        Expr::Real(_) => "Real".to_string(),
        Expr::Sym(_) => "Sym".to_string(),
        Expr::List(lst) => {
            if let Some(first) = lst.first() {
                match first {
                    Expr::Sym(s) => s.clone(),
                    _ => "List".to_string(),
                }
            } else {
                panic!("empty list isnt allowed")
            }
        }
    }
}

pub fn is_atom(expr: &Expr) -> bool {
    match expr {
        Expr::List(_) => false,
        _ => true,
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context {
    vars: std::collections::HashMap<Expr, Expr>,
}

pub fn evaluate(ctx: &mut Context, expr: &Expr) -> Expr {
    println!("evaluating {}", expr);
    match expr {
        Expr::Sym(_) => {
            if let Some(val) = ctx.vars.get(expr) {
                val.clone()
            } else {
                expr.clone()
            }
        }
        Expr::List(l) => {
            let mut prev_ne = None; // Store the previous ne here

            // this returns when we have reached fixed point 
            loop {
                let mut res = vec![];
                for e in l {
                    res.push(evaluate(ctx, e));
                }

                let ne = Expr::List(res.clone());

                if let Some(prev) = &prev_ne {
                    if *prev == ne {
                        if head(&ne) == "set" {
                            ctx.vars.insert(res[1].clone(), res[2].clone());
                        }
                        return ne;
                    }
                }

                prev_ne = Some(ne); // Update prev_ne to the current ne
            }
        }
        Expr::Int(_) | Expr::Real(_) => expr.clone(),
    }
}

fn main() -> Result<()> {
    let result = expr_parser::Expr("(42 foo (1.0 -2) (nested list))");
    match result {
        Ok(val) => println!("Parsed: {}", val),
        Err(err) => println!("Failed to parse: {}", err),
    }
    let mut rl = DefaultEditor::new()?;
    let mut ctx = Context {
        vars: std::collections::HashMap::new(),
    };

    let mut i = 0;

    loop {
        let prompt = format!("(in {})> ", i);
        let line = rl.readline(&prompt)?; // read
        rl.add_history_entry(line.as_str()).unwrap(); // history
        println!("Line: {line}"); // eval / print
        let ex = expr_parser::Expr(&line);
        match ex {
            Ok(expr) => {
                i += 1;
                // println!("Parsed: {}", val);
                println!("head: {}", head(&expr));
                let res = evaluate(&mut ctx, &expr);
                println!("Result: {}", res);
                println!("ctx: {:?}", ctx);
            }
            Err(err) => println!("Failed to parse: {}", err),
        }
    } // loop
}

/*
exprs/programs to make work
1.

(set x 1)
x
(set y 2)
(+ x y) => (+ 1 2). I don't think i want/need to implement arithmetic yet
2.
k[x_][y_] := x
(SetDelayed (k (pattern x (blank)) (pattern y (blank))) x)

f[x_] := {x, x^2, x^3}
f[y] # gives {x, x^2, x^3}
(SetDelayed (f (pattern x (blank)) (list x (pow x 2) (pow x 3)))

3.
(matchq x x) # true
(matchq x y) # false
(matchq x (pattern (blank))) # true
(matchq (list a) (pattern (blank))) # true


*/
