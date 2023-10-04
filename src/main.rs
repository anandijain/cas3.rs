extern crate peg;
use std::borrow::Cow::{self, Borrowed, Owned};

use ordered_float;
use rustyline::{
    error::ReadlineError,
    highlight::{Highlighter, MatchingBracketHighlighter},
    history::FileHistory,
    validate::MatchingBracketValidator,
    Completer, DefaultEditor, Editor, Helper, Hinter, Result, Validator,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::ops::{Deref, DerefMut};
use std::{fmt, path::Path};

peg::parser! {
    grammar expr_parser() for str {
        rule whitespace() = [' ' | '\t' | '\n' | '\r']*

        rule integer() -> Expr
            = n:$("-"? ['0'..='9']+ ) {? n.parse().map(Expr::Int).or(Err("integer")) }

        rule real() -> Expr
            = n:$("-"? ['0'..='9']* "." ['0'..='9']+ ) {? n.parse().map(Expr::Real).or(Err("real")) }

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule string() -> Expr
            = "\"" s:$((!['"'][_])* ) "\"" { Expr::Str(s.into()) }

        rule atom() -> Expr
            = real() / integer() / symbol() / string()

        rule list() -> Expr
            = "(" l:Expr() ** whitespace() ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = atom() / list()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Int(i64),
    Real(ordered_float::NotNan<f64>),
    Sym(String),
    Str(String),
    List(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Real(r) => write!(f, "{}", r),
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::Str(s) => write!(f, "\"{}\"", s),
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
            e => panic!("Can only deref Expr::List. ex:{}", e),
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
        Expr::Str(_) => Expr::Sym("Str".to_string()),
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
pub struct Context2 {
    vars: HashMap<Expr, TableEntry>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TableEntry {
    own: Option<Expr>,
    down: Expr,
    sub: Expr,
}

impl TableEntry {
    pub fn new() -> Self {
        Self {
            own: None,
            down: Expr::List(vec![sym("list")]),
            sub: Expr::List(vec![sym("list")]),
        }
    }
}

pub fn get_ownvalue(ctx: &Context2, sym: Expr) -> Option<Expr> {
    // println!("ctx: {:?}. sym: {}", ctx, sym);
    let te = ctx.vars.get(&sym);
    if let Some(te) = te {
        let rule = te.own.clone();
        return rule;
        // for now, since I'm only allowing a single ownvalue maybe im not going to do the whole handling of HoldPattern[lhs] :> rhs
        // where i actually take sym and do sym /. OwnValues[sym]
        // apply_rule()
    } else {
        None
    }
}

// pub fn get_downvalues(ctx: &Context2, sym: Expr) -> Option<Expr> {
//     let te = ctx.vars.get(&sym);
//     if let Some(te) = te {
//         let rule = te.down.clone();
//         if let Expr::List(rule) = rule {
//             Some(Expr::List(rule.to_vec()))
//         } else {
//             None
//         }
//     } else {
//         None
//     }
// }

// pub fn get_subvalues(ctx: &Context2, sym: Expr) -> Option<Expr> {
//     let te = ctx.vars.get(&sym);
//     if let Some(te) = te {
//         let rule = te.sub.clone();
//         if let Expr::List(rule) = rule {
//             Some(Expr::List(rule.to_vec()))
//         } else {
//             None
//         }
//     } else {
//         None
//     }
// }

pub fn evaluate(stack: &mut Expr, ctx: &mut Context2, expr: &Expr) -> Expr {
    let mut ex = expr.clone();
    let mut last_ex = None;

    loop {
        if Some(&ex) == last_ex.as_ref() {
            // If the expression hasn't changed, break the loop.
            break;
        }

        last_ex = Some(ex.clone());

        match &ex {
            Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => {
                break;
            }
            Expr::Sym(ref s) => {
                if let Some(rule) = get_ownvalue(ctx, sym(s)) {
                    ex = rule;
                } else {
                    break;
                }
            }
            Expr::List(ref ls) => {
                let h = ls.first().unwrap();
                let nh = evaluate(stack, ctx, h);
                let mut evaluated_args = vec![];

                for p in &ls[1..] {
                    evaluated_args.push(evaluate(stack, ctx, p));
                }

                // ex = match nh {
                //     // we dont need to panic here "abc"[foo] doesn't
                //     Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => panic!("head must be a symbol"),
                //     Expr::Sym(ref s) => apply_downvalues(stack, ctx, nh, &evaluated_args),
                //     Expr::List(ref head_args) => apply_subvalues(stack, ctx, nh, &evaluated_args),
                // };

                // this corresponds to step 15 in Wagner's main eval loop section
                // where we apply internal/builtin down and subvalues
                if nh == sym("matchq") {
                    assert!(evaluated_args.len() == 2);
                    ex = Expr::Sym(format!(
                        "{}",
                        is_match(&evaluated_args[0], &evaluated_args[1], &mut HashMap::new())
                    ));
                } else if nh == sym("sameq") {
                    let first_arg = &evaluated_args[0];
                    let all_same = evaluated_args.iter().all(|arg| arg == first_arg);
                    ex = Expr::Sym(format!("{}", all_same));
                } else if nh == sym("replace") {
                    ex = replace(&evaluated_args[0], &evaluated_args[1]);
                } else if nh == sym("replace_all") {
                    ex = replace_all(&evaluated_args[0], &evaluated_args[1]);
                } else if nh == sym("rr") || nh == sym("replace_repeated") {
                    ex = replace_repeated(&evaluated_args[0], &evaluated_args[1]);
                } else if nh == sym("head") {
                    ex = head(&evaluated_args[0]);
                } else if nh == sym("parse") {
                    match evaluated_args[0] {
                        Expr::Str(ref s) => {
                            let pex = expr_parser::Expr(s);
                            match pex {
                                Ok(expr) => ex = expr,
                                Err(err) => {
                                    println!("Failed to parse: {}", err);
                                    return sym("$Failed");
                                }
                            }
                        }
                        _ => println!("parse takes a string"),
                    }
                } else if nh == sym("set") {
                    match &evaluated_args[0] {
                        Expr::Sym(ref s) => {
                            let mut te = TableEntry::new();
                            te.own = Some(evaluated_args[1].clone());
                            ctx.vars.insert(sym(s), te);
                            ex = evaluated_args[1].clone();
                        }
                        _ => println!("set takes a symbol"),
                    }
                    // assert_eq!(evalparse"))
                } else {
                    ex = Expr::List(
                        std::iter::once(nh.clone())
                            .chain(evaluated_args.clone())
                            .collect(),
                    );
                }
            }
        }
    }

    ex
}

fn is_match(expr: &Expr, pattern_expr: &Expr, bindings: &mut HashMap<String, Expr>) -> bool {
    match (expr, pattern_expr) {
        (Expr::List(e_list), Expr::List(p_list)) => {
            if p_list.len() == 1 {
                if let Expr::Sym(ref head) = p_list[0] {
                    if head == "blank" {
                        return true;
                    }
                }
            }

            if p_list.len() > 0 {
                if let Expr::Sym(ref head) = p_list[0] {
                    if head == "pattern" {
                        let name = p_list[1].clone().to_string();
                        let pattern = &p_list[2];
                        if let Some(existing_binding) = bindings.get(&name) {
                            return expr == existing_binding;
                        }
                        if is_match(expr, pattern, bindings) {
                            bindings.insert(name, expr.clone());
                            return true;
                        }
                    }
                }
            }

            if p_list.len() == 0 || e_list.len() != p_list.len() {
                return false;
            }

            for (e, p) in e_list.iter().zip(p_list.iter()) {
                if !is_match(e, p, bindings) {
                    return false;
                }
            }
            true
        }
        (_, Expr::List(p_list)) => {
            if let Expr::Sym(ref p_head) = p_list[0] {
                if p_head == "pattern" {
                    let name = p_list[1].clone().to_string();
                    let pattern = &p_list[2];
                    if let Some(existing_binding) = bindings.get(&name) {
                        return expr == existing_binding;
                    }
                    if is_match(expr, pattern, bindings) {
                        bindings.insert(name, expr.clone());
                        return true;
                    }
                } else if p_head == "blank" {
                    if p_list.len() == 2 {
                        // println!("p_list: {:?}", p_list);
                        let required_head = &p_list[1];
                        if head(expr) == *required_head {
                            return true;
                        }
                    } else if p_list.len() == 1 {
                        return true;
                    }
                }
            }
            false
        }
        (Expr::Sym(e), Expr::Sym(p)) => e == p,
        (Expr::Int(e), Expr::Int(p)) => e == p,
        (Expr::Real(e), Expr::Real(p)) => e == p,
        (Expr::Str(e), Expr::Str(p)) => e == p,
        _ => false,
    }
}

pub fn bindings_to_rules(bindings: &HashMap<String, Expr>) -> Expr {
    let mut rules = Expr::List(vec![sym("list")]);
    for (name, binding) in bindings.clone() {
        rules.push(Expr::List(vec![sym("rule"), sym(&name), binding.clone()]));
    }
    rules
}

pub fn norm_rules(rules:&Expr)-> Vec<Expr> {
    if head(rules) == sym("rule") {
        return vec![rules.clone()];
    } else {
        assert_eq!(head(rules), sym("list"));
        return rules.clone()[1..].to_vec();
    };
}

pub fn replace(expr: &Expr, rules: &Expr) -> Expr {

    let rules_list = norm_rules(rules);

    for rule in rules_list {
        let mut bindings = HashMap::new();
        assert!(head(&rule) == sym("rule"));
        if is_match(expr, &rule[1], &mut bindings) {
            let mut new_expr = rule[2].clone();
            new_expr = replace_all(&new_expr, &bindings_to_rules(&bindings));
            println!("bindings: {:?} expr: {}", bindings, new_expr);
            // for (name, binding) in bindings.clone() {
            //     new_expr = replace(
            //         &new_expr,
            //         &Expr::List(vec![sym("rule"), sym(&name), binding]),
            //     );
            // }

            return new_expr;
        }
    }
    expr.clone()
}

pub fn replace_all(expr: &Expr, rules: &Expr) -> Expr {
    // let cexpr = expr.clone()
    // match rules {
    //     Expr::List(rs) => {
    //         for rule in rs {
    //             let mut bindings = HashMap::new();
    //             let (lhs, rhs) = (rule[1].clone(), rule[2].clone());
    //             if is_match(expr, &lhs, &mut bindings) {
    //                 return replace(expr, rule);
    //             }
    //         }
    //         match expr {
    //             Expr::List(ps) => {
    //                 let mut new = vec![];
    //                 for p in ps {
    //                     new.push(replace_all(p, rules));
    //                 }
    //                 return Expr::List(new);
    //             }
    //             Expr::Sym(_) | Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => return replace(expr, rules),
    //         }
    //     }
    //     _ => panic!("rules needs to be a list"),
    // }
    // 
    let rules_list = norm_rules(rules);
    for rule in rules_list {
        let mut bindings = HashMap::new();
        assert!(head(&rule) == sym("rule"));
        if is_match(expr, &rule[1], &mut bindings) {
            return replace(expr, &rule);
        }
    }

    match expr {
        // Base cases: Symbol, Int, Real, and Str types
        Expr::Sym(_) | Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => replace(expr, rules),

        // Recursive case: List type
        Expr::List(list) => {
            // for l in list {

            // }
            let new_list: Vec<Expr> = list
                .iter()
                .map(|sub_expr| replace_all(sub_expr, rules))
                .collect();
            // After replacing all sub-expressions, apply the rule(s) to the new list itself
            // replace(&Expr::List(new_list), rules)
            Expr::List(new_list)
        }
    }
}

pub fn replace_repeated(expr: &Expr, rules: &Expr) -> Expr {
    let mut current_expr = expr.clone();
    let mut i = 0;
    loop {
        let new_expr = replace_all(&current_expr, rules);
        if new_expr == current_expr {
            break;
        }
        current_expr = new_expr;
        i += 1;
        if i > 1 << 16 {
            println!("replace_repeated, iteration limit 1<<16 reached");
            break;
        }
    }
    current_expr
}

#[derive(Helper, Completer, Hinter, Validator)]
pub struct ReplHelper {
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    colored_prompt: String,
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

pub fn startup(ctx: &mut Context2, startup_path: &Path) -> Result<()> {
    let file = File::open(startup_path)?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        match line {
            Ok(content) => {
                if let Ok(ex) = &expr_parser::Expr(&content) {
                    let mut stack = Expr::List(vec![]);
                    evaluate(&mut stack, ctx, ex);
                }
            }
            Err(error) => {
                eprintln!("Error reading a line: {:?}", error);
            }
        }
    }

    Ok(())
}

pub fn run(
    mut rl: rustyline::Editor<ReplHelper, rustyline::history::FileHistory>,
    mut ctx: Context2,
) -> Result<()> {
    let mut i = 1;

    loop {
        let prompt = format!("(In {}) := ", i);
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{prompt}\x1b[0m");

        let line = rl.readline(&prompt); // read
        match line {
            Ok(l) => {
                rl.add_history_entry(l.as_str()).unwrap(); // history
                let ex = expr_parser::Expr(&l);
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

                        println!("(Out {i}) = {}", res);
                        i += 1;
                    }

                    Err(err) => println!("Failed to parse: {}", err),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    } // loop
    rl.save_history("history.txt").unwrap();
    Ok(())
}

fn main() -> Result<()> {
    let h = ReplHelper {
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let config = rustyline::Config::default();
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut ctx = Context2 {
        vars: HashMap::new(),
    };

    // startup(&mut ctx, Path::new("startup.sexp"))?;
    run(rl, ctx)?;
    Ok(())
}

pub fn evalparse(s: &str) -> Expr {
    let ex = expr_parser::Expr(s);
    match ex {
        Ok(expr) => {
            let mut ctx = Context2 {
                vars: HashMap::new(),
            };
            let mut stack = Expr::List(vec![]);
            evaluate(&mut stack, &mut ctx, &expr)
        }
        Err(err) => panic!("Failed to parse: {}", err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_matching() {
        assert_eq!(evalparse("(matchq 1 (blank))"), sym("true"));
        assert_eq!(evalparse("(matchq 1 (blank Int))"), sym("true"));
        assert_eq!(evalparse("(matchq 1 (pattern x (blank)))"), sym("true"));
        assert_eq!(evalparse("(matchq 1 (pattern x (blank Int)))"), sym("true"));
        assert_eq!(
            evalparse("(matchq 1 (pattern x (blank Sym)))"),
            sym("false")
        );
        assert_eq!(
            evalparse("(matchq ((k a) b) ((k (pattern x (blank Sym))) (pattern y (blank Sym))))"),
            sym("true")
        );
        assert_eq!(
            evalparse("(matchq ((k a) b) ((k (pattern x (blank))) (pattern y (blank))))"),
            sym("true")
        );
        assert_eq!(
            evalparse("(matchq (plus 1 2) (plus (blank) (blank)))"),
            sym("true")
        );

        assert_eq!(evalparse("(matchq (list a b c) (blank))"), sym("true"));
        assert_eq!(
            evalparse("(matchq (list a b c) (pattern x (blank)))"),
            sym("true")
        );

        assert_eq!(evalparse("(matchq (f (g 1)) (f (g (blank))))"), sym("true"));
        assert_eq!(
            evalparse("(matchq (f a a) (f (pattern x (blank)) (pattern x (blank))))"),
            sym("true")
        );
        assert_eq!(
            evalparse("(matchq (f a b) (f (pattern x (blank)) (pattern x (blank))))"),
            sym("false")
        );

        // nested patterns, head is pattern
        assert_eq!(
            evalparse("(matchq (foo x) ((pattern f (blank)) (pattern y (blank))))"),
            sym("true")
        );
    }

    #[test]
    fn test_rules_and_replacement() {
        assert_eq!(
            evalparse("(replace ((k a) b) (rule ((k (pattern x (blank))) (pattern y (blank))) x))"),
            sym("a")
        );

        // list of rules does first one that matches
        assert_eq!(
            evalparse("(replace x (list (rule a b) (rule x y)))"),
            sym("y")
        );
        assert_eq!(
            evalparse("(replace x (list (rule x y) (rule x z)))"),
            sym("y")
        );

        // doesn't keep going
        assert_eq!(
            evalparse("(replace x (list (rule x y) (rule y z)))"),
            sym("y")
        );

        // case where no rules apply
        assert_eq!(
            evalparse("(replace_all x (list (rule y a) (rule z b)))"),
            sym("x")
        );

        // test for blank with head + nested list
        assert_eq!(
            evalparse(r#"(replace_all (list 1 1.5 Pi (list a 2)) (rule (blank Int) "hi"))"#),
            expr_parser::Expr(r#"(list "hi" 1.5 Pi (list a "hi"))"#).unwrap()
        );

        assert_eq!(
            evalparse("(replace_all (list x (power x 2) y z) (list (rule x 1)))"),
            expr_parser::Expr("(list 1 (power 1 2) y z)").unwrap()
        );

        assert_eq!(
            evalparse("(replace_all (list x (power x 2) y z) (list (rule x 1) (rule y 2)))"),
            expr_parser::Expr("(list 1 (power 1 2) 2 z)").unwrap()
        );

        assert_eq!(
            evalparse("(replace_all (plus 1 (pow x 2) (pow x 4)) (rule (pow x (pattern p (blank))) (f p)))"),
            expr_parser::Expr("(plus 1 (f 2) (f 4))").unwrap()
        );

        let s = "(replace_repeated (list (f (f x)) (f x) (g (f x)) (f (g (f x)))) (list (rule (f (pattern x (blank))) x)))";
        // todo test s above to give (list x x (g x) (g x))
        assert_eq!(
            evalparse(s),
            expr_parser::Expr("(list x x (g x) (g x))").unwrap()
        );

        assert_eq!(
            evalparse("(rr ((((s s) k) k) k) (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))"),
            sym("k")
        );

    }
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
