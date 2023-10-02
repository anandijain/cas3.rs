extern crate peg;
use ordered_float;
use rustyline::{DefaultEditor, Result};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::ops::{Deref, DerefMut};
use std::{fmt, path::Path};

peg::parser! {
    grammar expr_parser() for str {
        rule integer() -> Expr
            = n:$("-"? ['0'..='9']+ ) {? n.parse().map(Expr::Int).or(Err("integer")) }

        rule real() -> Expr
            = n:$("-"? ['0'..='9']* "." ['0'..='9']+ ) {? n.parse().map(Expr::Real).or(Err("real")) }

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

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

impl Deref for Expr {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Self::Target {
        match self {
            Expr::List(vec) => vec,
            _ => panic!("Can only deref Expr::List"),
        }
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Expr::List(vec) => vec,
            _ => panic!("Can only deref Expr::List"),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context {
    vars: HashMap<Expr, Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context2 {
    vars: HashMap<Expr, TableEntry>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TableEntry {
    own: Expr,
    down: Expr,
    sub: Expr,
}

impl TableEntry {
    pub fn new() -> Self {
        Self {
            own: Expr::List(vec![sym("list")]),
            down: Expr::List(vec![sym("list")]),
            sub: Expr::List(vec![sym("list")]),
        }
    }
}

fn setd(ctx: &mut Context2, lhs: &Expr, rhs: &Expr) -> Expr {
    match lhs {
        Expr::Int(_) | Expr::Real(_) => sym("$Failed"),
        Expr::Sym(_) => {
            let mut te = ctx.vars.get_mut(lhs);
            if let Some(te) = te {
                println!("setting {:?} to {:?}", lhs, rhs);
                te.own = rhs.clone();
                println!("ctx: {:?}", ctx.vars.get_mut(lhs).unwrap().own);
            } else {
                let mut te = TableEntry::new();
                te.own = rhs.clone();
                ctx.vars.insert(lhs.clone(), te);
            }
            sym("Null")
        }
        Expr::List(ls) => {
            println!(
                "lhs of setd must be a symbol. this is Todo to set downvalues. ie (setd (f x) 1)"
            );
            sym("$Failed")
        }
    }
}

pub fn evaluate(stack: &mut Expr, ctx: &mut Context2, expr: &Expr) -> Expr {
    let mut prev = None;
    let mut ex = expr.clone();
    loop {
        if let Some(prev_ex) = &prev {
            if prev_ex == &ex {
                break;
            }
        }
        match &ex {
            Expr::Int(_) | Expr::Real(_) => {
                prev = Some(ex.clone());
                ex = ex.clone();
            }
            Expr::Sym(_) => {
                if let Some(te) = ctx.vars.get(&ex) {
                    let own = &te.own;
                    // own.len()
                    // println!("own: {:?}, len: {}", own, own.len());
                    match own {
                        
                    }
                    if own.len() == 1 {
                        prev = Some(ex.clone());
                        // ex = own[0].clone();
                    } else {
                        prev = Some(ex.clone());
                        ex = te.own.clone();
                        println!("ex: {:?}", ex);
                    }
                    // println!("ctx: {:?}", ctx);
                } else {
                    // we can create the tableentry here and not initialize.
                    // but we want to leave the OwnValues as empty list, which means that we return the symbol itself

                    ctx.vars.insert(ex.clone(), TableEntry::new());
                    prev = Some(ex.clone());
                    ex = ex.clone();
                }
            }
            Expr::List(l) => {
                if head(&ex) == sym("setd") {
                    let args = &l[1..];
                    if args.len() != 2 {
                        println!("setd needs 2 args");
                        prev = Some(ex.clone());
                        ex = sym("$Failed");
                    } else {
                        let lhs = &args[0];
                        let rhs = &args[1];
                        prev = Some(ex.clone());
                        ex = setd(ctx, lhs, rhs);
                    }
                } else if head(&ex) == sym("set") {
                    let args = &l[1..];
                    if args.len() != 2 {
                        println!("set needs 2 args");
                        prev = Some(ex.clone());
                        ex = sym("$Failed");
                    } else {
                        let lhs = &args[0];
                        let rhs = &args[1];
                        // Call setd with the original rhs
                        setd(ctx, lhs, rhs);
                        // Evaluate lhs
                        let lhs_evaluated = evaluate(stack, ctx, lhs);
                        // Call setd again with the evaluated lhs
                        prev = Some(ex.clone());
                        setd(ctx, lhs, &lhs_evaluated);
                        ex = lhs_evaluated
                    }
                } else if head(&ex) == sym("hold") {
                    break;
                } else {
                    let mut evaluated_args = vec![];
                    for ex in l {
                        println!("ex: {:?}", ex);
                        let evaluated_ex = evaluate(stack, ctx, ex);
                        evaluated_args.push(evaluated_ex);
                    }
                    prev = Some(ex.clone());
                    ex = Expr::List(evaluated_args);
                }
            }
        }
    }
    ex
}

// pub fn startup(ctx: &mut Context, startup_path: &Path) -> Result<()> {
//     let file = File::open(startup_path)?;
//     let reader = BufReader::new(file);

//     for line in reader.lines() {
//         match line {
//             Ok(content) => {
//                 if let Ok(ex) = &expr_parser::Expr(&content) {
//                     let mut stack = Expr::List(vec![]);
//                     evaluate(&mut stack, ctx, ex);
//                 }
//             }
//             Err(error) => {
//                 eprintln!("Error reading a line: {:?}", error);
//             }
//         }
//     }

//     Ok(())
// }

pub fn run() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut ctx = Context2 {
        vars: HashMap::new(),
    };

    // startup(&mut ctx, Path::new("startup.sexp")).unwrap();

    let mut i = 0;

    loop {
        let prompt = format!("(In {})> ", i);
        let line = rl.readline(&prompt)?; // read
        rl.add_history_entry(line.as_str()).unwrap(); // history
        let ex = expr_parser::Expr(&line);
        match ex {
            Ok(expr) => {
                let mut stack = Expr::List(vec![]);
                let res = evaluate(&mut stack, &mut ctx, &expr);
                // println!("head: {}", head(&expr));

                // ins and outs (works but makes ctx printing too verbose, and its just not that useful rn )
                // let in_i = expr_parser::Expr(format!("(setd (In {i}) {})", expr).as_str()).unwrap();
                // evaluate(&mut ctx, &in_i);
                // let out_i =
                //     expr_parser::Expr(format!("(set (Out {i}) {})", expr).as_str()).unwrap();
                // evaluate(&mut ctx, &out_i);

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

f[x_] := x
f[1]

(setd (f (pattern x (blank))) (f x))
(f 1) so basically wh

(f (list 1))


------
TODO actually make testing

(set (a b) c)
(a b) == c
(set b 1)
(a b) == (a 1)

------
need to make
(set x (plus x 1))
crash the program
and
(setd x (plus x 1)) not
but (setd x (plus x 1)), (x) should crash the program



f[x_]:=g[y_]:=y
f[1] === Null # True
but note that if you try to call g before f, then g is undefined
so
ff[x_]:=gg[y_]:=y
gg[1] # gives gg[1]
but then
ff[1]
gg[1] # now gives 1

also note that
x=1
x=2
works because Set is HoldFirst


https://mathematica.stackexchange.com/questions/176732/can-a-symbol-have-more-than-one-ownvalue
i will keep TableEntry.own as a list expr, but since I am not going to do conditional evaluation, it will only have one element
if set manually by the user, through OwnValues[x] = ..., i panic if more than one

one interesting thing is how to set Set attributes to HoldFirst and Setd before calling Set and Setd.
maybe have to manually put in those DownValues of Attributes manually in rust and not in startup
can also just hardcode it in evaluate to never evaluate the first argument of Set and the rest


apply just replaces list with arg[1]
apply[f, {a, b, c}] # f[a, b, c]


*/
