extern crate peg;
use std::{
    borrow::Cow::{self, Borrowed, Owned},
    ops::{Add, Mul},
};

use ordered_float;
use rug::Integer;
use rustyline::{
    config::Configurer,
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
            = whitespace() e:(atom() / list()) whitespace() { e }
    }
}

fn parse(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    // Int(num_bigint::BigInt),
    Int(Integer),
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

fn list(strs: Vec<&str>) -> Expr {
    Expr::List(strs.iter().map(|s| sym(s)).collect::<Vec<_>>())
}

fn liste(es: Vec<Expr>) -> Expr {
    Expr::List(es)
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

fn length(expr: &Expr) -> Expr {
    match expr {
        Expr::List(es) => Expr::Int((es.len() - 1).into()),
        _ => Expr::Int(0.into()),
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

// are we guaranteed that we have a list here?
// can evaluated_args be empty
// nh can be List too
pub fn internal_functions_apply(
    _stack: &mut Expr,
    ctx: &mut Context2,
    nh: Expr,
    evaluated_args: Vec<Expr>,
) -> Expr {
    let reconstructed_ex = Expr::List(
        std::iter::once(nh.clone())
            .chain(evaluated_args.clone().to_owned())
            .collect(),
    );

    if nh == sym("matchq") {
        if evaluated_args.len() != 2 {
            println!("matchq takes 2 arguments");
            return sym("$Failed");
        }
        return Expr::Sym(format!(
            "{}",
            my_match(
                evaluated_args[0].clone(),
                evaluated_args[1].clone(),
                &vec![],
                &mut HashMap::new(),
                &mut HashMap::new()
            )
        ));
    } else if nh == sym("sameq") {
        println!("in sameq: evaluated_args: {:?}", evaluated_args);
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
        // array part setting notes:
        // (set l (Table false 9))
        // simple single assignment case
        // (set (Part l 1) 3)

        // this is the more advanced case setting multiple at the same time
        // (set (Part l (list 1 2 3)) (Table true 3))

        // println!("evaluated_args: {:?}", evaluated_args);
        let lhs = &evaluated_args[0];
        let rhs = &evaluated_args[1];

        match lhs {
            // ownvalue
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
                        // todo: implementing list inplace modification
                        // if lhs_h == &sym("Part") {
                        //     let part_lhs = &ls[1];
                        //     let part_rhs = &ls[2];
                        //     assert_eq!(length(rhs), length(part_rhs));
                        // }
                        //given
                        // (set (f (pattern x (blank))) x)
                        // we end up pushing
                        // (rule_delayed (holdpattern expr[1]) expr[2])
                        // (rule_delayed (holdpattern evaluated_args[0]) evaluated_args[1])
                        // onto the downvalues of h (which is expected to have head list)
                        let te: &mut TableEntry = ctx
                            .vars
                            .entry(lhs_h.clone())
                            .or_insert_with(TableEntry::new);
                        let dv_str = format!("(rule_delayed (hold_pattern {lhs}) {rhs})");
                        let dv = expr_parser::Expr(&dv_str).unwrap();

                        // NOTE! here we aren't inserting in the right order, where we look for more specific
                        // definitions and insert them first. so user has to do the right order themselves
                        // at the moment
                        te.down.push(dv);
                        return rhs.clone();
                    }
                    // subvalue
                    Expr::List(_) => {
                        todo!("subvalues")
                    }
                    _ => {
                        let h = head(lhs_h);
                        println!("Tag {h} in {lhs} is Protected");
                        return rhs.clone();
                    }
                }
            }
            _ => {
                println!("set takes a symbol or list, got {}", lhs);
                return sym("$Failed");
            }
        }
    } else if nh == sym("setd") {
        // println!("evaluated_args: {:?}", evaluated_args);
        let lhs = &evaluated_args[0];
        match lhs {
            Expr::Sym(ref s) => {
                // pretty sure this needs to be fixed to check if te already exists
                let mut te = TableEntry::new();
                te.own = Some(evaluated_args[1].clone());
                ctx.vars.insert(sym(s), te);
                return sym("Null");
            }
            // this is the down/subvalue case
            Expr::List(ls) => {
                let lhs_h = &ls[0];
                match lhs_h {
                    // lhs_h is the tag in this downvalue assignment
                    Expr::Sym(_) => {
                        let rhs = &evaluated_args[1];
                        // onto the downvalues of h (which is expected to have head list)
                        let te: &mut TableEntry = ctx
                            .vars
                            .entry(lhs_h.clone())
                            .or_insert_with(TableEntry::new);
                        let dv_str = format!("(rule_delayed (hold_pattern {lhs}) {rhs})");
                        let dv = expr_parser::Expr(&dv_str).unwrap();

                        // NOTE! here we aren't inserting in the right order, where we look for more specific
                        // definitions and insert them first. so user has to do the right order themselves
                        // at the moment
                        te.down.push(dv);
                        return sym("Null");
                    }
                    // subvalue
                    Expr::List(_) => {
                        todo!("subvalues")
                    }
                    _ => panic!("hi"),
                }
            }
            _ => {
                println!("set takes a symbol or list, got {}", lhs);
                return sym("$Failed");
            }
        }
    } else if nh == sym("own_values") {
        ctx.vars
            .get(&evaluated_args[0])
            .unwrap() // there is a bug here
            .own
            .clone()
            .unwrap()
        // todo!()
    } else if nh == sym("down_values") {
        ctx.vars.get(&evaluated_args[0]).unwrap().down.clone()
    } else if nh == sym("sub_values") {
        todo!()
    }
    else if nh == sym("clear") {
        match &evaluated_args[0] {
            Expr::Sym(_) => {
                if let Some(te) = ctx.vars.get_mut(&evaluated_args[0]) {
                    te.own = None;
                    te.down = Expr::List(vec![sym("list")]);
                    te.sub = Expr::List(vec![sym("list")]);
                }
                return sym("Null");
            }
            _ => {
                println!("set takes a symbol");
                return sym("$Failed");
            }
        }
    } else if nh == sym("Plus") {
        match (&evaluated_args[0], &evaluated_args[1]) {
            (Expr::Int(a), Expr::Int(b)) => Expr::Int(a.add(b).into()),
            // see issue about 3.0 printing as `3`
            // (Expr::Real(a), Expr::Real(b)) => Expr::Real(a + b),
            _ => {
                let reconstructed_ex = Expr::List(
                    std::iter::once(nh.clone())
                        .chain(evaluated_args.clone().to_owned())
                        .collect(),
                );
                return reconstructed_ex;
            }
        }
    } else if nh == sym("Times") {
        match (&evaluated_args[0], &evaluated_args[1]) {
            (Expr::Int(a), Expr::Int(b)) => Expr::Int(a.mul(b).into()),
            _ => {
                return reconstructed_ex;
            }
        }
    } else if nh == sym("Part") {
        match &evaluated_args[0] {
            Expr::List(ls) => match &evaluated_args[1] {
                Expr::Int(i) => {
                    let i = i.to_isize().unwrap();
                    if i < 0 || i >= ls.len() as isize {
                        println!("Part: index {} out of range", i);
                        return reconstructed_ex;
                    }
                    return ls[i as usize].clone();
                }
                Expr::List(indices) => {
                    let mut results = vec![sym("list")];
                    for index in indices[1..].iter() {
                        match index {
                            Expr::Int(i) => {
                                let i = i.to_isize().unwrap();
                                if i < 0 || i >= ls.len() as isize {
                                    println!("Part: index {} out of range", i);
                                    return reconstructed_ex;
                                }
                                results.push(ls[i as usize].clone());
                            }
                            _ => {
                                println!("Part: each index in the list must be an integer");
                                return reconstructed_ex;
                            }
                        }
                    }
                    return Expr::List(results);
                }
                _ => {
                    println!("Part: index must be an integer or a list of integers");
                    return reconstructed_ex;
                }
            },
            _ => return reconstructed_ex,
        }
    } else if nh == sym("Length") {
        return length(&evaluated_args[0]);
    } else if nh == sym("Get") {
        if let Expr::Str(p) = &evaluated_args[0] {
            let res = run_file(ctx, Path::new(&p));
            if let Ok(res) = res {
                return res;
            } else {
                return sym("$Failed");
            }
        } else {
            println!("Get takes an Expr::String");
            return sym("$Failed");
        }
    }else {
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
        // println!("evaluating: {}", ex);

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
                let mut nh = evaluate(stack, ctx, h);

                // step 6
                // the use of a separate stack here is questionable
                // also to note that when we use "contains" on it, we also do a comparison against the head ("list"), which is wrong
                // but for practical purposes shouldn't cause a problem

                // the most important next step here is making sure that (attrs set) works correctly
                // note that something here causes SO

                // a potential problem here is that nh is not necesarily an atom
                // nh can be Expr::List

                // 3a (we might be able to just replace_all((attrs h) (down_values attrs)))
                // nvm 3a is less easy to see that a match wastn found

                // println!("hi");

                // 1. assume h::Sym
                // 2. get dvs of attr
                // 3. find a matching rule in dvs to (attrs h)
                // if no matching rule found, return "(list)"

                let mut nh_attrs = Expr::List(vec![sym("list")]);

                if let Expr::Sym(_) = nh.clone() {
                    let te = ctx.vars.entry(sym("attrs")).or_insert_with(TableEntry::new);
                    // (down_values attrs)
                    let dvs = &te.down;
                    let attr_expr =
                        expr_parser::Expr(&format!("(attrs {})", nh).to_string()).unwrap();
                    // println!("attr_expr: {}", attr_expr);
                    if let Expr::List(_ls) = dvs {
                        // dv expected to be (rule_delayed (hold_pattern lhs) rhs)
                        for dv in &_ls[1..] {
                            let mut pos_map = HashMap::new();
                            let mut named_map = HashMap::new();
                            let pos = vec![];
                            if my_match(
                                attr_expr.clone(),
                                dv[1].clone(),
                                &pos,
                                &mut pos_map,
                                &mut named_map,
                            ) {
                                // println!("found attributes match for {} -> {}", nh, dv);
                                nh_attrs = replace(&attr_expr, dv);
                                break; // Exit the loop once a match is found
                            }
                        }
                    } else {
                        panic!("down_values must be a list");
                    }
                }

                // println!("nh_attrs: {:?}", nh_attrs);
                // assert!(head(&nh_attrs) == sym("list"));
                if nh_attrs.contains(&sym("HoldAllComplete")) {
                    // skip to 14
                    todo!();
                }

                // step 7
                let mut evaluated_args = vec![];

                // hold_mask entry with a zero means "don't hold"
                let mut hold_mask = vec![false; ls.len() - 1];

                // idk if it should be else ifs
                if nh_attrs.contains(&sym("HoldAll")) {
                    hold_mask.fill(true);
                }
                if nh_attrs.contains(&sym("HoldFirst")) {
                    hold_mask[0] = true;
                }
                if nh_attrs.contains(&sym("HoldRest")) {
                    hold_mask[1..].fill(true);
                }
                // println!("hold_mask: {:?}", hold_mask);

                for (i, p) in ls[1..].iter().enumerate() {
                    if hold_mask[i] {
                        evaluated_args.push(p.clone());
                    } else {
                        let ev = evaluate(stack, ctx, p);

                        evaluated_args.push(ev);
                    }
                }

                if !nh_attrs.contains(&sym("SequenceHold")) {
                    let mut arg_idx = 0;
                    while arg_idx < evaluated_args.len() {
                        if head(&evaluated_args[arg_idx]) == sym("Sequence") {
                            let seq_args: Vec<_> = evaluated_args[arg_idx][1..].to_vec();
                            evaluated_args.splice(arg_idx..arg_idx + 1, seq_args);
                        } else {
                            arg_idx += 1;
                        }
                    }
                }
                let reconstructed_ex = Expr::List(
                    std::iter::once(nh.clone())
                        .chain(evaluated_args.clone().to_owned())
                        .collect(),
                );
                // println!("reconstructed_ex: {}", reconstructed_ex);

                // step 14: apply user defined downvalues and subvalues
                let exprime = match nh.clone() {
                    // we dont need to panic here "abc"[foo] doesn't
                    Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => {
                        panic!("head must be a symbol, got {nh}")
                    }
                    // this is the down_value case, bcause the head
                    Expr::Sym(_) => {
                        let te = ctx.vars.entry(nh.clone()).or_insert_with(TableEntry::new);
                        let dvs = &te.down;
                        // println!("looking for user defined down_values for {} -> {}", nh, dvs);

                        // should this be replace_all? or replace_repeated?

                        let exprime = replace_all(&reconstructed_ex, dvs);
                        // println!("before: {}", reconstructed_ex);
                        // println!("after: {}", exprime);
                        exprime
                    }
                    // subvalue
                    Expr::List(_) => reconstructed_ex.clone(),
                };

                // im not sure if this is correct, but it seems necesary,
                // if we found a matching downvalue rule, then we need to re-evaluate the expression after replacement
                if ex != exprime {
                    ex = exprime;
                    continue;
                }

                // note now that ex is not necesarily a List anymore
                // so if we still have a list, then we do step 15, and apply internal down/subvalues

                match ex {
                    Expr::List(_) => {}
                    _ => continue,
                }
                nh = head(&ex);
                evaluated_args = ex[1..].to_vec();
                // this corresponds to step 15 in Wagner's main eval loop section
                // where we apply internal/builtin down and subvalues
                ex = internal_functions_apply(stack, ctx, nh, evaluated_args);
            }
        }
    }
    // println!("exiting evaluate: {}", ex);
    ex
}

fn named_rebuild_all(expr: Expr, map: &HashMap<Expr, Expr>) -> Expr {
    // First, check if the entire expression exists in the map and replace it if it does
    if let Some(replacement) = map.get(&expr) {
        return replacement.clone();
    }

    // If the expression is not in the map, proceed with the recursion
    match expr {
        Expr::List(list) => {
            // Recursively rebuild all sub-expressions in the list
            let new_list: Vec<Expr> = list
                .into_iter()
                .map(|e| named_rebuild_all(e, map))
                .collect();
            Expr::List(new_list)
        }
        _ => expr,
    }
}

// fn fix_pos_map(offset:isize, pos: &Vec<usize>, pos_map: &HashMap<Vec<usize>, Expr>)->HashMap<Vec<usize>, Expr> {

// }

// )
/// to fix pos_map. as you iterate es, and you keep track of the offset for the current list.
/// then you want to adjust lookups into the posmap based on the position - offset.
fn pos_map_rebuild(pos: Vec<usize>, pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    if let Some(replacement) = pos_map.get(&pos) {
        return replacement.clone();
    }

    match pat {
        Expr::List(es) => {
            let mut new_es = vec![];
            // note in the case of blank_null_seq, this can be negative
            let mut offset: isize = 0;
            for (i, e) in es.iter().enumerate() {
                let mut new_pos = pos.clone();
                new_pos.push(i);
                let new_e = pos_map_rebuild(new_pos, e.clone(), pos_map);
                if head(&new_e) == sym("Sequence") {
                    offset += new_e.len() as isize - 2; // its -2 becasue 1 for the head and 1 for the original (blank_seq) object
                }
                // println!("{i}, new_e: {} offest: {offset}", new_e);
                new_es.push(new_e);
            }
            Expr::List(new_es)
        }
        _ => pat,
    }
}

fn splice_sequences(expr: Expr) -> Expr {
    match expr {
        Expr::List(mut list) => {
            let mut i = 0;
            while i < list.len() {
                list[i] = splice_sequences(list[i].clone());
                i += 1;
            }

            let mut new_list = Vec::new();
            let mut i = 0;
            while i < list.len() {
                let item = list[i].clone();
                if let Expr::List(ref sublist) = item {
                    if let Some(Expr::Sym(head)) = sublist.first() {
                        if head == "Sequence" {
                            new_list.extend_from_slice(&sublist[1..]);
                            i += 1;
                            continue;
                        }
                    }
                }
                new_list.push(item);
                i += 1;
            }
            Expr::List(new_list)
        }
        _ => expr,
    }
}

fn rebuild_and_splice(
    pat: Expr,
    pos: &Vec<usize>,
    pos_map: &HashMap<Vec<usize>, Expr>,
    named_map: &HashMap<Expr, Expr>,
) -> Expr {
    splice_sequences(named_rebuild_all(
        pos_map_rebuild(pos.clone(), pat, pos_map),
        named_map,
    ))
}

// we assume that p has a blank object for a head
fn is_blank_match(e: Expr, p: Expr) -> bool {
    if let Expr::List(ps) = p {
        if ps.len() == 2 {
            let p_head = &ps[1];
            if p_head == &head(&e) {
                true
            } else {
                false
            }
        } else {
            true
        }
    } else {
        panic!("is_blank_match needs a list for p")
    }
}

fn my_match(
    ex: Expr,
    mut pat: Expr,
    pos: &Vec<usize>,
    pos_map: &mut HashMap<Vec<usize>, Expr>,
    named_map: &mut HashMap<Expr, Expr>,
) -> bool {
    let pattern_expr = pat.clone();
    if head(&pattern_expr) == sym("hold_pattern") {
        pat = pattern_expr[1].clone();
    }
    // println!("entering my_match: {pos:?} | {ex} | {pat} | {pos_map:?} | {named_map:?}");
    let pat_syms = vec![sym("blank"), sym("blank_seq")];
    match (ex.clone(), pat.clone()) {
        (Expr::List(es), Expr::List(ps)) => {
            if ps[0] == sym("pattern") {
                if let Some(from_map) = named_map.get(&pat) {
                    return &ex == from_map;
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat.clone(), ex.clone());
                        return true;
                    }
                }
            } else if pat_syms.contains(&ps[0]) {
                if is_blank_match(ex.clone(), pat.clone()) {
                    pos_map.insert(pos.clone(), ex);
                    return true;
                }
            }

            let mut new_pos = pos.clone();
            new_pos.push(0); // we are at the head
            if !my_match(es[0].clone(), ps[0].clone(), &new_pos, pos_map, named_map) {
                return false;
            }

            'outer: for (i, pi) in ps.iter().enumerate().skip(1) {
                let mut new_pos = pos.clone();
                new_pos.push(i); // we are at the head
                if head(pi) == sym("pattern") {
                    // println!("in pattern pi ");
                    if let Some(from_map) = named_map.get(&pi) {
                        // here is an example that contradicts the below i think
                        //  (setd (Times (pattern x (blank Sym)) (pattern x (blank Sym))) (Pow x 2))
                        println!("we should have rebuilt to remove this i think");
                    }
                    let b = &pi[2];
                    let bt = &b[0];
                    let p_name = &pi[1];
                    if bt == &sym("blank_seq") {
                        for j in 1..=es[1..].len() {
                            let mut elts = vec![sym("Sequence")];
                            // im pretty sure this is not needed
                            if i + j > es.len() {
                                // println!("breaking news!");
                                break 'outer;
                            }
                            for seq_e in &es[i..i + j] {
                                if b.len() == 2 {
                                    let b_head = &b[1];
                                    if b_head != &head(seq_e) {
                                        break;
                                    }
                                }
                                elts.push(seq_e.clone());
                            }
                            let seq = liste(elts);
                            named_map.insert(pi.clone(), seq.clone());

                            let new_pat = rebuild_and_splice(pat.clone(), &pos, pos_map, named_map);
                            // println!("new_pat in bs: at iter {j} {new_pat} {seq}");
                            if my_match(ex.clone(), new_pat, pos, pos_map, named_map) {
                                break 'outer;
                            }
                        }
                    } else if bt == &sym("blank_null_seq") {
                        for j in 0..=es[1..].len() {
                            let mut elts = vec![sym("Sequence")];
                            // im pretty sure this is not needed
                            if i + j > es.len() {
                                // println!("breaking news!");
                                break 'outer;
                            }
                            for seq_e in &es[i..i + j] {
                                if b.len() == 2 {
                                    let b_head = &b[1];
                                    if b_head != &head(seq_e) {
                                        break;
                                    }
                                }
                                elts.push(seq_e.clone());
                            }
                            let seq = liste(elts);
                            named_map.insert(pi.clone(), seq.clone());

                            let new_pat = rebuild_and_splice(pat.clone(), &pos, pos_map, named_map);
                            // println!("new_pat in bs: at iter {j} {new_pat} {seq}");
                            if my_match(ex.clone(), new_pat, pos, pos_map, named_map) {
                                break 'outer;
                            }
                        }
                    } else {
                        if i >= es.len() {
                            break 'outer;
                        }
                        // named blank case
                        if !my_match(es[i].clone(), ps[i].clone(), &new_pos, pos_map, named_map) {
                            break 'outer;
                        }
                    }
                } else if head(pi) == sym("blank_seq") {
                    for j in 1..=es[1..].len() {
                        let mut elts = vec![sym("Sequence")];
                        // im pretty sure this is not needed
                        if i + j > es.len() {
                            // println!("breaking news!");
                            break 'outer;
                        }
                        for seq_e in &es[i..i + j] {
                            if pi.len() == 2 {
                                let b_head = &pi[1];
                                if b_head != &head(seq_e) {
                                    break;
                                }
                            }
                            elts.push(seq_e.clone());
                        }
                        let seq = liste(elts);
                        pos_map.insert(new_pos.clone(), seq.clone());

                        let new_pat = rebuild_and_splice(pat.clone(), &pos, pos_map, named_map);
                        // println!("new_pat in bs: at iter {j} {new_pat}");
                        let mut copy = pos_map.clone();
                        // this is to avoid double application of a pos rule
                        copy.remove(&new_pos);
                        // if my_match(ex.clone(), pat.clone(), pos, &mut copy) {
                        if my_match(ex.clone(), new_pat, pos, &mut copy, named_map) {
                            pos_map.clear();
                            pos_map.extend(copy);

                            pos_map.insert(new_pos.clone(), seq.clone());

                            break 'outer;
                        } else {
                            // break 'outer;
                            // i think we need to revert pos_map to whatever it was before this my_match call
                        }
                    }
                } else if head(pi) == sym("blank_null_seq") {
                    for j in 0..=es[1..].len() {
                        let mut elts = vec![sym("Sequence")];
                        // im pretty sure this is not needed
                        if i + j > es.len() {
                            println!("breaking news!");
                            break 'outer;
                        }
                        for seq_e in &es[i..i + j] {
                            if pi.len() == 2 {
                                let b_head = &pi[1];
                                if b_head != &head(seq_e) {
                                    break;
                                }
                            }
                            elts.push(seq_e.clone());
                        }
                        let seq = liste(elts);
                        pos_map.insert(new_pos.clone(), seq.clone());

                        let new_pat = rebuild_and_splice(pat.clone(), &pos, pos_map, named_map);
                        // println!("new_pat in bs: at iter {j} {new_pat}");
                        let mut copy = pos_map.clone();
                        // this is to avoid double application of a pos rule
                        copy.remove(&new_pos);
                        // if my_match(ex.clone(), pat.clone(), pos, &mut copy) {
                        if my_match(ex.clone(), new_pat, pos, &mut copy, named_map) {
                            pos_map.clear();
                            pos_map.extend(copy);

                            pos_map.insert(new_pos.clone(), seq.clone());

                            break 'outer;
                        } else {
                            // break 'outer;
                            // i think we need to revert pos_map to whatever it was before this my_match call
                        }
                    }
                } else {
                    if i >= es.len() {
                        break 'outer;
                    }
                    if !my_match(es[i].clone(), ps[i].clone(), &new_pos, pos_map, named_map) {
                        break 'outer;
                    }
                }
            }
            let final_pat = rebuild_and_splice(pat.clone(), &pos, pos_map, named_map);
            // println!("final comparison: POS: {pos:?} | PAT: {pat} | NEW_PAT: {final_pat} | EX: {ex} || pos {pos_map:?} || named {named_map:?}");
            if final_pat == ex {
                return true;
            }
            false
        }
        (_, Expr::List(ps)) => {
            if ps[0] == sym("pattern") {
                if let Some(from_map) = named_map.get(&pat) {
                    return &ex == from_map;
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat.clone(), ex.clone());
                        true
                    } else {
                        false
                    }
                }
            } else if pat_syms.contains(&ps[0]) {
                if is_blank_match(ex.clone(), pat.clone()) {
                    pos_map.insert(pos.clone(), ex);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        _ => ex == pat,
    }
}

pub fn bindings_to_rules(bindings: &HashMap<String, Expr>) -> Expr {
    let mut rules = Expr::List(vec![sym("list")]);
    for (name, binding) in bindings.clone() {
        rules.push(Expr::List(vec![sym("rule"), sym(&name), binding.clone()]));
    }
    rules
}

pub fn pat_bindings_to_rules(bindings: &HashMap<Expr, Expr>) -> Expr {
    let mut rules = Expr::List(vec![sym("list")]);
    for (pat, binding) in bindings.clone() {
        if let Expr::List(ps) = pat {
            let p_name = &ps[1]; // (pattern x (blank))
            rules.push(Expr::List(vec![
                sym("rule"),
                p_name.clone(),
                binding.clone(),
            ]));
        }
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
        let pos = vec![];
        let mut pos_map = HashMap::new();
        let mut named_map = HashMap::new();
        assert!(head(&rule) == sym("rule") || head(&rule) == sym("rule_delayed"));
        if my_match(
            expr.clone(),
            rule[1].clone(),
            &pos,
            &mut pos_map,
            &mut named_map,
        ) {
            let mut new_expr = rule[2].clone();
            new_expr = replace_all(&new_expr, &pat_bindings_to_rules(&named_map));

            return new_expr;
        }
    }
    expr.clone()
}

pub fn replace_all(expr: &Expr, rules: &Expr) -> Expr {
    let rules_list = norm_rules(rules);
    for rule in rules_list {
        let pos = vec![];
        let mut pos_map = HashMap::new();
        let mut named_map = HashMap::new();
        assert!(head(&rule) == sym("rule") || head(&rule) == sym("rule_delayed"));
        if my_match(
            expr.clone(),
            rule[1].clone(),
            &pos,
            &mut pos_map,
            &mut named_map,
        ) {
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

pub fn startup_attrs(ctx: &mut Context2) {
    let attrs_te = ctx.vars.entry(sym("attrs")).or_insert_with(TableEntry::new);
    let mut exs = vec![
        format!("(rule_delayed (hold_pattern (attrs hold_pattern)) (list HoldAll))"),
        format!("(rule_delayed (hold_pattern (attrs attrs)) (list HoldAll))"),
        format!("(rule_delayed (hold_pattern (attrs rule_delayed)) (list HoldRest SequenceHold))"),
        format!("(rule_delayed (hold_pattern (attrs set)) (list HoldFirst SequenceHold))"),
        format!("(rule_delayed (hold_pattern (attrs down_values)) (list HoldAll))"),
    ]
    .iter_mut()
    .map(|s| expr_parser::Expr(&s).unwrap())
    .collect();
    attrs_te.down.append(&mut exs);
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

pub fn run_file(ctx: &mut Context2, startup_path: &Path) -> Result<Expr> {
    let file = File::open(startup_path)?;
    let reader = BufReader::new(file);

    // i dont love this because it's ambigious whether or not something failed in reading the file or sth
    // or if the last expr in the file was a setd or something that returns a Null
    let mut res = sym("Null");
    for line in reader.lines() {
        match line {
            Ok(content) => {
                if content.starts_with("//") {
                    continue;
                }
                if let Ok(ex) = &expr_parser::Expr(&content) {
                    let mut stack = Expr::List(vec![]);
                    res = evaluate(&mut stack, ctx, ex);
                } else {
                    eprintln!("Error parsing a line: {:?}", content);
                }
            }
            Err(error) => {
                eprintln!("Error reading a line: {:?}", error);
            }
        }
    }

    Ok(res)
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
                        // let in_i = expr_parser::Expr(format!("(setd (In {i}) {})", expr).as_str())
                        //     .unwrap();
                        // evaluate(&mut stack, &mut ctx, &in_i);
                        let out_i =
                            expr_parser::Expr(format!("(set (Out {i}) {})", res).as_str()).unwrap();
                        evaluate(&mut stack, &mut ctx, &out_i);

                        println!("\x1B[1m(Out {i}) = {}\x1B[0m", res);

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
    // println!("{}", sym("hi").len());

    let h = ReplHelper {
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let config = rustyline::Config::default();
    let mut rl = Editor::with_config(config)?;
    rl.set_max_history_size(10000).unwrap();
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut ctx = Context2 {
        vars: HashMap::new(),
    };

    run_file(&mut ctx, Path::new("lang/attrs.sexp"))?;
    run_file(&mut ctx, Path::new("lang/startup.sexp"))?;
    run_file(&mut ctx, Path::new("lang/calculus.sexp"))?;
    startup_attrs(&mut ctx);
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

pub fn ctx_evalparse(ctx: &mut Context2, s: &str) -> Expr {
    let ex = expr_parser::Expr(s);
    match ex {
        Ok(expr) => {
            let mut stack = Expr::List(vec![]);
            evaluate(&mut stack, ctx, &expr)
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

        // testing that patterns with the same name must equal the same matched expr
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

        // blank doesnt match the here, which is correct. triple_blank would match this
        assert_eq!(evalparse("(matchq (f) (f (blank)))"), sym("false"));
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

        // let nand = "s[ s[ k[ s[ s[ s][s[k[k[k]]]]]]]][s]";
        //            (s (s (k (s (s  s) (s (k (k k)))))))

        // let nand = "(((s s) (s (k (k k)))) s)";
        // let nand = "(((s s) (s (k (k k)))) s)";
        // let nand = "(s (s (k (s (((s s) (s (k (k k)))))))) s)";
        // let nand = "(((s (s (k (((s (s s)) (s (k (k k)))))))) s)";
        // let a = "k";
        // let b = "k";
        // let s = format!("(({nand} {a}) {b})");
        // println!("{}", s);
    }

    #[test]
    fn list_ops() {
        assert_eq!(
            evalparse("(sameq (Part (f x y z) (list 1 2 3)) (list x y z))"),
            sym("true")
        );
    }

    #[test]
    fn seqs_and_geeks() {
        assert_eq!(
            evalparse("(sameq (f (Sequence a b) c (Sequence d e)) (f a b c d e))"),
            sym("true")
        );

        let mut ctx = Context2 {
            vars: HashMap::new(),
        };
        run_file(&mut ctx, Path::new("lang/attrs.sexp")).unwrap();
        ctx_evalparse(
            &mut ctx,
            "(setd (listq (pattern x (blank))) (sameq list (head x)))",
        );
        println!("listq ctx {:?}", ctx);
        assert_eq!(ctx_evalparse(&mut ctx, "(listq (list a b c))"), sym("true"));
    }

    #[test]
    fn new_pattern_matcher() {
        let blank = list(vec!["blank"]);

        let lhs = list(vec!["f", "a", "b"]);
        let rhs = list(vec!["f", "blank"]);

        let test_cases = vec![
            (sym("1"), sym("1"), true),      // goes to "1" == "1" Sym, Sym arm
            (sym("1"), blank.clone(), true), // Sym Sym arm with blank
            (sym("1"), Expr::List(vec![sym("1")]), false), // Sym List -> false
            (Expr::List(vec![sym("1")]), sym("1"), false), // List Sym
            // (1) | (blank)
            (Expr::List(vec![sym("1")]), blank.clone(), true), // List, sym, with blank
            (lhs.clone(), rhs.clone(), false),                 // List, sym, with blank
            // (lhs.clone(), list(vec!["f", "blank", "blank"]), true), // List, sym, with blank
            (
                lhs.clone(),
                liste(vec![sym("f"), blank.clone(), blank.clone()]),
                true,
            ), // List, sym, with blank
            (sym("f"), list(vec!["blank", "Sym"]), true),
            (sym("f"), list(vec!["blank", "f"]), false),
            (list(vec!["f", "x"]), list(vec!["blank", "f"]), true),
            (list(vec!["f", "x"]), list(vec!["blank", "g"]), false),
            (parse("(f (a b))"), parse("(f (blank))"), true),
            (parse("(f (a b))"), parse("(f (blank a))"), true),
            (parse("(f x)"), parse("((blank) (blank))"), true),
            (parse("f"), parse("(pattern x (blank))"), true),
            (parse("(f)"), parse("(pattern x (blank))"), true),
            (parse("(f x)"), parse("((pattern x (blank)) (blank))"), true),
            (
                parse("(f a b c)"),
                parse("(f (pattern x (blank_seq)))"),
                true,
            ),
            (
                parse("(f a b c)"),
                parse("(f (pattern x (blank_seq)) (pattern y (blank_seq)))"),
                true,
            ),
            (
                parse("(f a a)"),
                parse("(f (pattern x (blank_seq)) (pattern x (blank_seq)))"),
                true,
            ),
            (
                parse("(f a (g b))"),
                parse("(f (pattern x (blank_seq)))"),
                true,
            ),
            (
                parse("(f a)"),
                parse("(f (pattern x (blank_null_seq)))"),
                true,
            ),
            (
                parse("(f a)"),
                parse("(f (pattern x (blank_null_seq)) a)"),
                true,
            ),
            (
                parse("(f a b c a b)"),
                parse("(f (pattern x (blank_seq)) c (pattern x (blank_seq)))"),
                true,
            ),
            (
                parse("(f (a b) c a b)"),
                parse("(f (pattern x (blank b)) (pattern y (blank_seq)))"),
                false,
            ),
            (
                parse("(f (a b) c a b)"),
                parse("(f (pattern x (blank a)) (pattern y (blank_seq)))"),
                true,
            ),
            (
                parse("(f a b c d)"),
                parse("(f (blank_seq) (pattern y (blank_seq)))"),
                true,
            ),
            // fails todo fix blank_seq with head
            (
                parse("(f (a b) (a c) (b d))"),
                parse("(f (pattern x (blank_seq a)))"),
                false,
            ),
            (
                parse("(f (a b) (a c) (b d))"),
                parse("(f (pattern x (blank_seq a)) (b d))"),
                true,
            ),
            // pos : Vec<usize> where are we in the pattern Expr
            (
                parse("(f (a b) (a c) (b d))"),
                parse("(f (blank_seq a) (b d))"),
                true,
            ),
            (
                parse("(f (a b) (a c) (b d))"),
                parse("(f (blank_seq a))"),
                false,
            ),
        ];

        // list(vec!["f", "a", "b", "c"]), list(vec!["f", sym("blank_sequence")])
        for (i, (ex, pat, expected)) in test_cases.iter().enumerate() {
            println!("testing case {i}: {ex} | {pat} ");
            let pos = vec![];
            let mut pos_map = HashMap::new();
            let mut named_map = HashMap::new();
            let m = my_match(ex.clone(), pat.clone(), &pos, &mut pos_map, &mut named_map);
            let rebuilt_ex = rebuild_and_splice(pat.clone(), &vec![], &pos_map, &named_map);
            println!("rebuilt:{rebuilt_ex:?}\n\npos:\n{pos_map:?}\nnamed:\n{named_map:?}\n\n");

            assert_eq!(m, *expected);

            if *expected {
                assert_eq!(rebuilt_ex, ex.clone());
            }
        }
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
