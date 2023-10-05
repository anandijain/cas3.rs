extern crate peg;
use std::borrow::Cow::{self, Borrowed, Owned};

use ordered_float;
use rustyline::{
    error::ReadlineError,
    highlight::{Highlighter, MatchingBracketHighlighter},
    validate::MatchingBracketValidator,
    Completer, Editor, Helper, Hinter, Result, Validator,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
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

// (a b c) -> (b c)
// fn rest(expr: &Expr) -> Expr {}

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

pub fn internal_functions_apply(
    stack: &mut Expr,
    ctx: &mut Context2,
    nh: Expr,
    evaluated_args: Vec<Expr>,
) -> Expr {
    if nh == sym("matchq") {
        // assert!(evaluated_args.len() == 2);
        if evaluated_args.len() != 2 {
            println!("matchq takes 2 arguments");
            return sym("$Failed");
        }
        return Expr::Sym(format!(
            "{}",
            is_match(&evaluated_args[0], &evaluated_args[1], &mut HashMap::new())
        ));
    } else if nh == sym("sameq") {
        let first_arg = &evaluated_args[0];
        let all_same = evaluated_args.iter().all(|arg| arg == first_arg);
        return Expr::Sym(format!("{}", all_same));
    } else if nh == sym("replace") {
        return replace(&evaluated_args[0], &evaluated_args[1]);
    } else if nh == sym("replace_all") {
        return replace_all(&evaluated_args[0], &evaluated_args[1]);
    } else if nh == sym("rr") || nh == sym("replace_repeated") {
        if evaluated_args.len() != 2 {
            println!("replace_repeated takes 2 arguments");
            return sym("$Failed");
        }
        return replace_repeated(&evaluated_args[0], &evaluated_args[1]);
    } else if nh == sym("head") {
        return head(&evaluated_args[0]);
    } else if nh == sym("parse") {
        match evaluated_args[0] {
            Expr::Str(ref s) => {
                let pex = expr_parser::Expr(s);
                match pex {
                    Ok(expr) => return expr,
                    Err(err) => {
                        println!("Failed to parse: {}", err);
                        return sym("$Failed");
                    }
                }
            }
            _ => {
                println!("parse takes a string");
                return sym("$Failed");
            }
        }
    } else if nh == sym("set") {
        let lhs = &evaluated_args[0];
        match lhs {
            Expr::Sym(ref s) => {
                // pretty sure this needs to be fixed to check if te already exists 
                let mut te = TableEntry::new();
                te.own = Some(evaluated_args[1].clone());
                ctx.vars.insert(sym(s), te);
                return evaluated_args[1].clone();
            }
            // this is the down/subvalue case
            Expr::List(ls) => {
                let lhs_h = &ls[0];
                match lhs_h {
                    // lhs_h is the tag in this downvalue assignment
                    Expr::Sym(_) => {
                        //given
                        // (set (f (pattern x (blank))) x)
                        // we end up pushing
                        // (rule_delayed (holdpattern expr[1]) expr[2])
                        // (rule_delayed (holdpattern evaluated_args[0]) evaluated_args[1])
                        let rhs = &evaluated_args[1];
                        // onto the downvalues of h (which is expected to have head list)
                        let te: &mut TableEntry =
                            ctx.vars.entry(lhs_h.clone()).or_insert_with(TableEntry::new);
                        let dv_str = format!("(rule_delayed (hold_pattern {lhs}) {rhs})");
                        let dv = expr_parser::Expr(&dv_str).unwrap();

                        // NOTE! here we aren't inserting in the right order, where we look for more specific 
                        // definitions and insert them first. so user has to do the right order themselves
                        // at the moment 
                        te.down.push(dv);
                        return rhs.clone();
                    }
                    // subvalue
                    Expr::List(ls2) => {
                        todo!()
                    }
                    _ => panic!("hi"),
                }
            }
            _ => {
                println!("set takes a symbol or list");
                return sym("$Failed");
            }
        }
    } else if nh == sym("own_values") {
        ctx.vars.get(&evaluated_args[0]).unwrap().own.clone().unwrap()
        // todo!()
    } else if nh == sym("down_values") {
        ctx.vars.get(&evaluated_args[0]).unwrap().down.clone()
    } else if nh == sym("sub_values") {
        todo!()
    } 
    // need to hold our args first otherwise (set x 1) (clear x) ends up being (clear 1) which makes no sense
    // else if nh == sym("clear") {
    //     match &evaluated_args[0] {
    //         Expr::Sym(ref s) => {
    //             if let Some(te) =  ctx.vars.get_mut(&evaluated_args[0]) {
    //                 te.own = None;
    //             }
    //             return sym("Null");
    //         }
    //         _ => {
    //             println!("set takes a symbol");
    //             return sym("$Failed");
    //         }
    //     }
    // }
    else {
        return Expr::List(
            std::iter::once(nh.clone())
                .chain(evaluated_args.clone().to_owned())
                .collect(),
        );
    }
}

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
                let h;
                if let Some(sh) = ls.first() {
                    h = sh;
                } else {
                    println!("Expr::List needs a head");
                    return sym("$Failed");
                }
                // step 5
                let nh = evaluate(stack, ctx, h);

                // step 6
                // the use of a separate stack here is questionable
                // also to note that when we use "contains" on it, we also do a comparison against the head ("list"), which is wrong
                // but for practical purposes shouldn't cause a problem

                // the most important next step here is making sure that (attrs set) works correctly

                // note that something here causes SO 
                // let nh_attrs = evaluate(
                //     &mut Expr::List(vec![sym("list")]),
                //     ctx,
                //     &expr_parser::Expr(&format!("(attrs {})", nh).to_string()).unwrap(),
                // );
                // println!("nh_attrs: {:?}", nh_attrs);
                // assert!(head(&nh_attrs) == sym("list"));
                // if nh_attrs.contains(&sym("HoldAllComplete")) {
                //     // skip to 14
                //     todo!();
                // }

                // step 7

                let mut evaluated_args = vec![];

                // hold_mask entry with a zero means "don't hold"
                let mut hold_mask = vec![false; ls.len() - 1];

                // idk if it should be else ifs
                // if nh_attrs.contains(&sym("HoldAll")) {
                //     hold_mask.fill(true);
                // }
                // if nh_attrs.contains(&sym("HoldFirst")) {
                //     hold_mask[0] = true;
                // }
                // if nh_attrs.contains(&sym("HoldRest")) {
                //     hold_mask[1..].fill(true);
                // }

                for (i, p) in ls[1..].iter().enumerate() {
                    if hold_mask[i] {
                        evaluated_args.push(p.clone());
                    } else {
                        evaluated_args.push(evaluate(stack, ctx, p));
                    }
                }

                // step 14
                // ex = match nh {
                //     // we dont need to panic here "abc"[foo] doesn't
                //     Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => panic!("head must be a symbol"),
                //     Expr::Sym(ref s) => apply_downvalues(stack, ctx, nh, &evaluated_args),
                //     Expr::List(ref head_args) => apply_subvalues(stack, ctx, nh, &evaluated_args),
                // };

                // this corresponds to step 15 in Wagner's main eval loop section
                // where we apply internal/builtin down and subvalues
                ex = internal_functions_apply(stack, ctx, nh, evaluated_args);
            }
        }
    }

    ex
}

// this whole thing needs to be rewritten
// just a total mess
fn is_match(expr: &Expr, pattern_ex: &Expr, bindings: &mut HashMap<String, Expr>) -> bool {
    let mut pattern_expr = pattern_ex.clone();
    if head(&pattern_expr) == sym("hold_pattern") {
        pattern_expr = pattern_expr[1].clone();
    }
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
                            // not sure if this branch is needed. this whole function needs to be rewritten
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
        (Expr::Sym(e), Expr::Sym(p)) => e == &p,
        (Expr::Int(e), Expr::Int(p)) => e == &p,
        (Expr::Real(e), Expr::Real(p)) => e == &p,
        (Expr::Str(e), Expr::Str(p)) => e == &p,
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

pub fn norm_rules(rules: &Expr) -> Vec<Expr> {
    if head(rules) == sym("rule") || head(rules) == sym("rule_delayed") {
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
        println!("rule: {}", rule);
        assert!(head(&rule) == sym("rule") || head(&rule) == sym("rule_delayed"));
        if is_match(expr, &rule[1], &mut bindings) {
            let mut new_expr = rule[2].clone();
            new_expr = replace_all(&new_expr, &bindings_to_rules(&bindings));
            // println!("bindings: {:?} expr: {}", bindings, new_expr);
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
    let rules_list = norm_rules(rules);
    for rule in rules_list {
        let mut bindings = HashMap::new();
        assert!(head(&rule) == sym("rule") || head(&rule) == sym("rule_delayed"));
        if is_match(expr, &rule[1], &mut bindings) {
            return replace(expr, &rule);
        }
    }

    match expr {
        Expr::Sym(_) | Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => replace(expr, rules),
        Expr::List(list) => {
            let new_list: Vec<Expr> = list
                .iter()
                .map(|sub_expr| replace_all(sub_expr, rules))
                .collect();
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
                // saving every line (even if slow, just until its more stable)
                rl.save_history("history.txt").unwrap();

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
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    } // loop
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
    let ctx = Context2 {
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

        // head matching
        assert_eq!(
            evalparse("(matchq (list x) (pattern x (blank list)))"),
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
        // (s k) is false, and k is true
        // Combinator reduction of And for Tuples[{True, False}]
        // in boolean logic, you need 3 things to "do everything"
        // true, false, nand
        // whats cool about combinators, is you only need 2 things
        // s and k combinators. everything else is up to interpretation

        let test_cases = vec![
            ("((((s s) k) (s k)) (s k))", "(s k)"),
            ("((((s s) k) (s k)) k)", "(s k)"),
            ("((((s s) k) k) (s k))", "(s k)"),
            ("((((s s) k) k) k)", "k"),
        ];
        let crules_str = "(list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x))";
        for (input, res) in test_cases.iter() {
            assert_eq!(
                evalparse(&format!("(rr {} {crules_str})", input)),
                expr_parser::Expr(res).unwrap()
            );
        }

        // let nand = "s[s[k[s[s[s][s[k[k[k]]]]]]]][s]";
        // (s (s (k (s (s s) (s (k (k k)))))))

        let nand = "(((s s) (s (k (k k)))) s)";
        let nand = "(((s s) (s (k (k k)))) s)";
        let nand = "(s (s (k (s (((s s) (s (k (k k)))))))) s)";
        let nand = "(((s (s (k (((s (s s)) (s (k (k k)))))))) s)";
        let a = "k";
        let b = "k";
        let s = format!("(({nand} {a}) {b})");
        println!("{}", s);
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
