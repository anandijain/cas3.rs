extern crate peg;
use ordered_float;
use rustyline::{DefaultEditor, Result};
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::{fmt, path::Path};

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

pub fn sym(s: &str) -> Expr {
    Expr::Sym(s.to_string())
}

fn head(expr: &Expr) -> Expr {
    match expr {
        Expr::Int(_) => Expr::Sym("Int".to_string()),
        Expr::Real(_) => Expr::Sym("Real".to_string()),
        Expr::Sym(_) => Expr::Sym("Sym".to_string()),
        Expr::List(lst) => {
            if let Some(first) = lst.first() {
                first.clone()
            } else {
                println!("[ERROR]: empty list isnt allowed");
                Expr::Sym("GET_FUCKED".to_string())
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

// pub fn args(expr: &Expr) -> Vec<Expr> {
//     match expr {
//         Expr::List(lst) => lst[1..].to_vec(),
//         _ => vec![]
//     }
// }

// pub fn part(expr: &Expr, n: usize) -> Expr {
//     match expr {
//         Expr::List(lst) => lst[n].clone(),
//         _ => expr.clone() // this is wrong
//     }
// }

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context {
    vars: std::collections::HashMap<Expr, Expr>,
}

pub fn evaluate(ctx: &mut Context, expr: &Expr) -> Expr {
    // println!("evaluating {}", expr);
    match expr {
        Expr::Sym(_) => {
            // in the  below case we need precedence for locally bound
            // (set ((k a"_") b_) a)
            if let Some(val) = ctx.vars.get(expr) {
                val.clone()
            } else {
                expr.clone()
            }
        }
        Expr::List(l) => {
            let mut prev_ne = None; // Store the previous ne here

            if head(expr) == Expr::Sym("hold".to_string()) {
                return expr.clone();
            }

            // this returns when we have reached fixed point
            loop {
                let mut res = vec![];
                for e in l {
                    res.push(evaluate(ctx, e));
                }

                let ne = Expr::List(res.clone());

                if let Some(prev) = &prev_ne {
                    if *prev == ne {
                        // fixpoint condition
                        let h = head(&ne);
                        if h == Expr::Sym("set".to_string()) {
                            // we do this so (set (x) (x)), (x) doesnt crash
                            if res[1] != res[2] {
                                ctx.vars.insert(res[1].clone(), res[2].clone());
                            }
                            return res[2].clone();
                        } else if h == Expr::Sym("SameQ".to_string()) {
                            if res[1] == res[2] {
                                return Expr::Sym("True".to_string());
                            } else {
                                return Expr::Sym("False".to_string());
                            }
                        } else if h == Expr::Sym("head".to_string()) {
                            println!("so no head??");
                            return head(&res[1]);
                        }

                        // todo figure out how SetDelayed works
                        // else if head(&ne) == "setd" {

                        // }
                        // i think this is the part that needs to be smarter about its lookup. for instance
                        // if we fib[n_] := fib[n - 1] + fib[n - 2]
                        // SetDelayed[fib[Pattern[n, Blank[]]], Plus[fib[Plus[n, -1]], fib[Plus[n, -2]]]]]
                        // so i think simple Eq/PartialEq is not enough here
                        // fib[n] gives TerminatedEvaluation[RecursionLimit]
                        //
                        let def = ctx.vars.get(&ne);
                        if let Some(val) = def {
                            println!("we out here : {}", val);
                            return evaluate(ctx, &val.clone());
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

pub fn startup(ctx: &mut Context, startup_path: &Path) -> Result<()> {
    let file = File::open(startup_path)?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        match line {
            Ok(content) => {
                if let Ok(ex) = &expr_parser::Expr(&content) {
                    evaluate(ctx, ex);
                }
                // println!("{}", content);
            }
            Err(error) => {
                eprintln!("Error reading a line: {:?}", error);
            }
        }
    }

    Ok(())
}

pub fn run() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut ctx = Context {
        vars: std::collections::HashMap::new(),
    };

    startup(&mut ctx, Path::new("startup.sexp")).unwrap();

    let mut i = 0;

    loop {
        let prompt = format!("(In {})> ", i);
        let line = rl.readline(&prompt)?; // read
        rl.add_history_entry(line.as_str()).unwrap(); // history
        let ex = expr_parser::Expr(&line);
        match ex {
            Ok(expr) => {
                let res = evaluate(&mut ctx, &expr);
                // println!("head: {}", head(&expr));

                // ins and outs (works but makes ctx printing too verbose, and its just not that useful rn )

                // let in_i = expr_parser::Expr(format!("(set (In {i}) {})", expr).as_str()).unwrap();
                // evaluate(&mut ctx, &in_i);
                // let out_i = expr_parser::Expr(format!("(set (Out {i}) {})", expr).as_str()).unwrap();
                // evaluate(&mut ctx, &out_i);

                // println!("ctx: {:?}", ctx);

                println!("(Out {i}): {}", res);
                i += 1;
            }
            Err(err) => println!("Failed to parse: {}", err),
        }
    } // loop
}

fn main() -> Result<()> {
    let result = expr_parser::Expr("(42 foo (1.0 -2) (nested list))");
    let result2 = expr_parser::Expr("(42 foo (1.0 -2) (nested list))");

    assert_eq!(result, result2);

    run()?;
    Ok(())
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

4. (most important right now)
SetDelayed[fib[Pattern[n, Blank[]]], Plus[fib[Plus[n, -1]], fib[Plus[n, -2]]]]]

(set (fib 1) (fib 0))
(set (fib 0) 1)
(set_delayed (fib (pattern n (blank))) (plus (fib (minus n 1)) (fib (minus n 2))))
(set_delayed (fib (pattern n (blank Int))) (plus (fib (minus n 1)) (fib (minus n 2))))
(fib 5)

okay so we make a new hashmap called DownValues that is a HashMap of symbol to list of exprs
this list of expr is all the downvalues.
so
f[x_][y_] := x
f[x_] := 1

f[x][y] # 1

k[x_][y_] := x
k[x][y] # x

so it does the recursive thing, doesn't find any pattern matching (k x), which it looks to find first, shown by the f example
then goes out to see if there is a more nested pattern that matches, which is the k example


one thing that mathematica does is it actually stores (fib 2, 3,... ) in the evaluation of fib(5)


notes:
currently this crashes the interpreter because it goes into an infinite loop (no fixed point)
(set (f) (f f)
(set (f f) (f))

# bug
(and (SameQ (plus x y) (plus x y)) False) gives (and True False) (did not eval to fixed point)


f[x_] := x
f[1]

(setd (f (pattern x (blank))) (f x))
(f 1) so basically wh

(f (list 1))


*/
