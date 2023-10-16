extern crate peg;

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::{Deref, DerefMut};
use std::{
    borrow::Cow,
    ops::{},
};
use std::{fmt, path::Path};

use ordered_float;
use rug::Integer;
use rustyline::{
    error::ReadlineError,
    highlight::{Highlighter, MatchingBracketHighlighter},
    validate::MatchingBracketValidator,
    Completer, Editor, Helper, Hinter, Result, Validator,
};

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

// to remove
fn parse(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

impl Expr {
    fn parse(s: &str) -> Expr {
        expr_parser::Expr(s).unwrap()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
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
// (f a b) - Display
// Expr::List(vec![Expr::Sym("f")]) - Debug
// {:?}

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

// impl IntoIterator<Item = &str>
fn list(strs: Vec<&str>) -> Expr {
    Expr::List(strs.iter().map(|s| sym(s)).collect::<Vec<_>>())
}

fn liste(expressions: Vec<Expr>) -> Expr {
    Expr::List(expressions)
}

fn error() -> Expr {
    Expr::Sym("GET_FUCKED".to_string())
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
    // matches!(expr, Expr::List(_))
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
            down: Expr::List(vec![sym("list")]), //Expr::from_expressions(), f[1] = "hello" f[x] = x f[_] = "hi"
            sub: Expr::List(vec![sym("list")]),
        }
    }
}

pub fn get_ownvalue(ctx: &Context2, sym: Expr) -> Option<Expr> {
    // println!("ctx: {:?}. sym: {}", ctx, sym);
    // ctx.vars.get(&sym).map(|u| u.to_owned())
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

    if nh == sym("new_match") {
        if evaluated_args.len() != 2 {
            println!("matchq takes 2 arguments");
            return sym("$Failed");
        }
        return Expr::Sym(format!(
            "{}",
            my_match(
                &evaluated_args[0],
                evaluated_args[1].clone(),
                &vec![],
                &mut HashMap::new(),
                &mut HashMap::new()
            )
        ));
    } else if nh == sym("matchq") {
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
        // println!("evaluated_args: {:?}", evaluated_args);
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
    // need to hold our args first otherwise (set x 1) (clear x) ends up being (clear 1) which makes no sense
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
            (Expr::Int(a), Expr::Int(b)) => Expr::Int((a + b).into()),
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
        // make variadic
        match (&evaluated_args[0], &evaluated_args[1]) {
            (Expr::Int(a), Expr::Int(b)) => Expr::Int(( a * b).into()),
            _ => reconstructed_ex,
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
    } else {
        return Expr::List(
            std::iter::once(nh.clone())
                .chain(evaluated_args.clone().to_owned())
                .collect(),
        );
    }
}

// impl Expr { pub fn evaluate(&mut self, ...) -> Expr {...} }
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
                            let mut bindings = HashMap::new();
                            if is_match(&attr_expr, &dv[1], &mut bindings) {
                                // println!("found attributes match for {} -> {}", nh, dv);
                                nh_attrs = replace(&attr_expr, dv);
                                break; // Exit the loop once a match is found
                            }
                        }
                    } else {
                        panic!("downvalues must be a list");
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

                // step 14
                ex = match nh.clone() {
                    // we dont need to panic here "abc"[foo] doesn't
                    Expr::Int(_) | Expr::Real(_) | Expr::Str(_) => {
                        panic!("head must be a symbol, got {nh}")
                    }
                    // this is the down_value case, bcause the head
                    Expr::Sym(_) => {
                        let te = ctx.vars.entry(nh.clone()).or_insert_with(TableEntry::new);
                        let dvs = &te.down;
                        // println!("looking for user defined down_values for {} -> {}", s, dvs);

                        // should this be replace_all? or replace_repeated?

                        let exprime = replace_all(&reconstructed_ex, dvs);
                        // println!("before: {}", reconstructed_ex);
                        // println!("after: {}", exprime);
                        exprime
                    }
                    Expr::List(_) => ex,
                };
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

    ex
}

fn pos_map_rebuild(pos: Vec<usize>, pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    if let Some(replacement) = pos_map.get(&pos) {
        return replacement.clone();
    }

    match pat {
        Expr::List(es) => {
            let mut new_es = vec![];
            for (i, e) in es.iter().enumerate() {
                let mut new_pos = pos.clone();
                new_pos.push(i);
                let new_e = pos_map_rebuild(new_pos, e.clone(), pos_map);
                new_es.push(new_e);
            }
            Expr::List(new_es)
        }
        _ => pat,
    }
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

fn rebuild_all(
    pat: Expr,
    named_map: &HashMap<Expr, Expr>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    let new_ex = named_rebuild_all(pat, named_map);
    pos_map_rebuild(vec![], new_ex, pos_map)
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
                        if head == "sequence" {
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
    named_map: &HashMap<Expr, Expr>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    splice_sequences(rebuild_all(pat, named_map, pos_map))
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

fn is_match2(ex: &Expr, pat: &Expr) -> bool {
    my_match(
        ex,
        pat.clone(),
        &vec![],
        &mut HashMap::new(),
        &mut HashMap::new(),
    )
}

// remember pos always refers to the place in the pattern, not the ex
fn my_match(
    ex: &Expr,
    pat: Expr,
    pos: &Vec<usize>,
    pos_map: &mut HashMap<Vec<usize>, Expr>,
    named_map: &mut HashMap<Expr, Expr>,
) -> bool {
    println!("{pos:?} | {ex} | {pat}");
    // todo move
    let blank_syms = vec![sym("blank"), sym("blank_seq"), sym("blank_null_seq")];
    match (ex.clone(), pat.clone()) {
        (Expr::List(es), Expr::List(ps)) => {
            // so if we are not in the else branch, we are matching the entire list ex
            let mut new_pos = pos.clone();
            new_pos.push(0); // zero is head position
            if ps[0] == sym("pattern") {
                if let Some(v) = named_map.get(&pat) {
                    // remember it may be the case that we actually recurse here but i dont really think so
                    return v == ex;
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat, ex.clone());
                        return true;
                    } else {
                        return false;
                    }
                }
            } else if blank_syms.contains(&ps[0]) {
                // we can assume that there is no key at our current position (we've never been here before),
                // or if we have, we removed the key appropriately
                assert!(!pos_map.contains_key(pos));
                if is_blank_match(ex.clone(), pat.clone()) {
                    pos_map.insert(pos.to_vec(), ex.clone());
                    println!("inserted {new_pos:?} -> {ex}");
                    return true;
                } else {
                    return false;
                }
            } else {
                let p_head = &ps[0];
                let p_rest = &ps[1..];

                let e_head = &es[0];
                let e_rest = &es[1..];

                // (f x) | (f _)
                if !my_match(e_head, p_head.clone(), &new_pos, pos_map, named_map) {
                    // pos_map.remove()
                    return false;
                }

                // we skip the head, remember blank_seq and blank_null_seq can only match
                // (f a b) | ((blank_seq) (blank)) -> false because we cant "take" the a along with us for blank_seq
                for (i, p_i) in ps.iter().enumerate().skip(1) {
                    let mut new_pos = pos.clone();
                    new_pos.push(i); // zero is head position

                    match p_i {
                        Expr::List(pi_ps) => {
                            if pi_ps[0] == sym("pattern") {
                                let pi_blank = &pi_ps[2];
                                let blank_t = &pi_ps[2][0];
                                // actually this can throw, because its optional arg
                                // let pi_blank_h = &pi_ps[2][1];
                                let p_name = &pi_ps[1];

                                if blank_t == &sym("blank_seq") {
                                    for j in 1..=es[1..].len() {
                                        let mut elts = vec![sym("sequence")];
                                        for seq_e in &es[i..i + j] {
                                            if pi_blank.len() == 2 {
                                                let b_head = &pi_blank[1];
                                                if b_head != &head(seq_e) {
                                                    break;
                                                }
                                            }
                                            elts.push(seq_e.clone());
                                        }
                                        let seq = liste(elts);

                                        println!("seq {p_name:?}: {seq:?}");
                                        if let Some(from_map) = named_map.get(p_i) {
                                        } else {
                                            named_map.insert(p_i.clone(), seq.clone());
                                            println!("here i am ");
                                        } // rebuild all uses the map to rebuild the pattern up to equality with no matching

                                        let new_ex =
                                            rebuild_and_splice(pat.clone(), named_map, pos_map);

                                        let m = my_match(ex, new_ex, pos, pos_map, named_map);
                                        if !m {
                                            named_map.remove(p_i);
                                            // since we are in a named pattern, we dont need this
                                            // pos_map.remove(&new_pos);
                                        } else {
                                            return true;
                                        }
                                    }
                                } else if blank_t == &sym("blank_null_seq") {
                                    for j in 0..=es[1..].len() {
                                        let mut elts = vec![sym("sequence")];
                                        for seq_e in &es[i..i + j] {
                                            if pi_blank.len() == 2 {
                                                let b_head = &pi_blank[1];
                                                if b_head != &head(seq_e) {
                                                    break;
                                                }
                                            }
                                            elts.push(seq_e.clone());
                                        }
                                        let seq = liste(elts);

                                        println!("seq {p_name:?}: {seq:?}");
                                        if let Some(from_map) = named_map.get(p_i) {
                                        } else {
                                            named_map.insert(p_i.clone(), seq.clone());
                                            // println!("here i am ");
                                        } // rebuild all uses the map to rebuild the pattern up to equality with no matching

                                        let new_ex =
                                            rebuild_and_splice(pat.clone(), named_map, pos_map);

                                        let m = my_match(ex, new_ex, pos, pos_map, named_map);
                                        if !m {
                                            named_map.remove(p_i);
                                            // since we are in a named pattern, we dont need this
                                            // pos_map.remove(&new_pos);
                                        } else {
                                            return true;
                                        }
                                    }
                                } else {
                                    // blank case

                                    let m =
                                        my_match(&es[i], p_i.clone(), &new_pos, pos_map, named_map);
                                    if !m {
                                        named_map.remove(p_i);
                                        return false;
                                    }

                                    let new_ex =
                                        rebuild_and_splice(pat.clone(), named_map, pos_map);
                                    let m = my_match(ex, new_ex, pos, pos_map, named_map);
                                    if !m {
                                        named_map.remove(p_i);
                                        return false;
                                    }
                                }
                            } else if pi_ps[0] == sym("blank_seq") {
                                // let blank_h = &pi_ps[1];
                                for j in 1..=es[1..].len() {
                                    let mut elts = vec![sym("sequence")];
                                    for seq_e in &es[i..i + j] {
                                        if pi_ps.len() == 2 {
                                            let b_head = &pi_ps[1];
                                            if b_head != &head(seq_e) {
                                                break;
                                            }
                                        }
                                        elts.push(seq_e.clone());
                                    }
                                    let seq = liste(elts);

                                    println!("positional seq current pos {pos:?} ll pos: {new_pos:?}: {seq:?}");
                                    if let Some(from_map) = pos_map.get(&new_pos) {
                                    } else {
                                        pos_map.insert(new_pos.clone(), seq.clone());
                                        // println!("here i am ");
                                    } // rebuild all uses the map to rebuild the pattern up to equality with no matching

                                    let new_ex =
                                        rebuild_and_splice(pat.clone(), named_map, pos_map);
                                    println!("in positional {new_ex}");

                                    let m = my_match(ex, new_ex, pos, pos_map, named_map);
                                    if !m {
                                        pos_map.remove(&new_pos);
                                    } else {
                                        return true;
                                    }
                                }
                            } else if pi_ps[0] == sym("blank_null_seq") {
                                // let blank_h = &pi_ps[1];
                                for j in 0..=es[1..].len() {
                                    let mut elts = vec![sym("sequence")];
                                    for seq_e in &es[i..i + j] {
                                        if pi_ps.len() == 2 {
                                            let b_head = &pi_ps[1];
                                            if b_head != &head(seq_e) {
                                                break;
                                            }
                                        }
                                        elts.push(seq_e.clone());
                                    }
                                    let seq = liste(elts);

                                    println!("positional seq {new_pos:?}: {seq:?}");
                                    if let Some(from_map) = pos_map.get(&new_pos) {
                                    } else {
                                        pos_map.insert(new_pos.clone(), seq.clone());
                                        // println!("here i am ");
                                    } // rebuild all uses the map to rebuild the pattern up to equality with no matching

                                    let new_ex =
                                        rebuild_and_splice(pat.clone(), named_map, pos_map);
                                    let m = my_match(ex, new_ex, pos, pos_map, named_map);

                                    if !m {
                                        pos_map.remove(&new_pos);
                                    } else {
                                        return true;
                                    }
                                }
                            } else if pi_ps[0] == sym("blank") {
                                if !my_match(&es[i], p_i.clone(), &new_pos, pos_map, named_map) {
                                    return false;
                                }
                                let new_ex = rebuild_and_splice(pat.clone(), named_map, pos_map);
                                let m = my_match(ex, new_ex, pos, pos_map, named_map);

                                if !m {
                                    pos_map.remove(&new_pos);
                                } else {
                                    return true;
                                }
                            } else {
                                // this is the case where the p_i is a list but not a blank/pattern
                                if !my_match(&es[i], p_i.clone(), &new_pos, pos_map, named_map) {
                                    return false;
                                }
                            }
                        }
                        _ => {
                            // this is the case where p_i is a Sym
                            if !my_match(&es[i], p_i.clone(), &new_pos, pos_map, named_map) {
                                return false;
                            }
                        }
                    }
                }

                // what this is saying is:
                // if we've reached the end of the pattern list, finding mappings for all elements,
                // then we should be able to recreate ex from whatever was added to the maps
                // i think the issue is a depth mismatch, basically
                let new_ex = rebuild_and_splice(pat.clone(), named_map, pos_map);
                println!("final comparison: ORIG: {pat} | NEW: {new_ex} | OLD:{ex} || {named_map:?} {pos_map:?}");
                if pat == ex.clone() {
                    // if new_ex == ex.clone() {
                    return true;
                } else {
                    return false;
                }
            }
        }
        (_, Expr::List(ps)) => {
            if ps[0] == sym("pattern") {
                if let Some(v) = named_map.get(&pat) {
                    // remember it may be the case that we actually recurse here but i dont really think so
                    v == ex
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat, ex.clone());
                        true
                    } else {
                        false
                    }
                }
            } else if blank_syms.contains(&ps[0]) {
                // we can assume that there is no key at our current position (we've never been here before),
                // or if we have, we removed the key appropriately
                assert!(!pos_map.contains_key(&pos.clone()));
                if is_blank_match(ex.clone(), pat.clone()) {
                    pos_map.insert(pos.to_vec(), ex.clone());
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        (e, p) => e == p,
        // _ => false,
    }
}

// this whole thing needs to be rewritten
// just a total mess
fn is_match(expr: &Expr, pattern_ex: &Expr, bindings: &mut HashMap<String, Expr>) -> bool {
    // this is questionable
    let mut pattern_expr = pattern_ex.clone();
    if head(&pattern_expr) == sym("hold_pattern") {
        pattern_expr = pattern_expr[1].clone();
    }

    match (expr, pattern_expr) {
        (Expr::List(e_list), Expr::List(p_list)) => {
            // println!("e_list: {:?}", e_list);
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
                    //  else if p_head == "blank_null_seq" {
                    //     return true;
                    // }
                }
            }

            if p_list.len() == 0 || e_list.len() != p_list.len() {
                // println!("length case ");
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
        (l, r) => l == &r,
    }
}

pub fn bindings_to_rules(bindings: &HashMap<String, Expr>) -> Expr {
    let mut rules = Expr::List(vec![sym("list")]);
    for (name, binding) in bindings.clone() {
        rules.push(Expr::List(vec![sym("rule"), sym(&name), binding.clone()]));
    }
    rules
}

// pub struct ContextWhatever {
//     bindings: HashMap<Expr, Expr>
// }

// impl ContextWhatever {
//     pub fn pat_bindings_to_rules(&self) -> Expr {
//         let mut rules = Expr::List(vec![sym("list")]);
//         for (pat, binding) in bindings.clone() {
//             if let Expr::List(ps) = pat {
//                 let p_name = &ps[1]; // (pattern x (blank))
//                 rules.push(Expr::List(vec![
//                     sym("rule"),
//                     p_name.clone(),
//                     binding.clone(),
//                 ]));
//             }
//         }
//         rules
//     }
// }

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
        let mut bindings = HashMap::new();
        // println!("rule: {}", rule);
        assert!(head(&rule) == sym("rule") || head(&rule) == sym("rule_delayed"));
        if is_match(expr, &rule[1], &mut bindings) {
            return replace_all(&rule[2], &bindings_to_rules(&bindings));
            // let mut new_expr = rule[2].clone();
            // new_expr = replace_all(&new_expr, &bindings_to_rules(&bindings));
            // return new_expr;
        }
    }
    expr.clone()
}

// &self
pub fn replace_all(expr: &Expr, rules: &Expr) -> Expr {
    // &self
    let rules_list = norm_rules(rules); // rules.normalize()
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
// (set x 1)
// (set x 2) -> (set 1 2)
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
            Cow::Borrowed(&self.colored_prompt)
        } else {
            Cow::Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
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
                    // impl Expr {
                    //     evaluate
                    // }
                    // ex.evaluate(Expr::list(), ctx)
                    // Expr::list().evaluate(ctx, ex)
                    let mut stack = Expr::List(vec![sym("list")]);
                    evaluate(&mut stack, ctx, ex);
                }
            }
            Err(error) => {
                // tracing::error!("")
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
                // tracing::trace!({})

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
                        let out_i = expr_parser::Expr(format!("(set (Out {i}) {})", expr).as_str())
                            .unwrap();
                        evaluate(&mut stack, &mut ctx, &out_i);

                        println!("\x1B[1m(Out {i}) = {}\x1B[0m", res);

                        i += 1;
                    }

                    Err(err) => println!("Failed to parse: {}", err),
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => eprintln!("Error: {:?}", err),
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
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut ctx = Context2 {
        vars: HashMap::new(),
    };

    startup(&mut ctx, Path::new("startup.sexp"))?;
    startup_attrs(&mut ctx);
    run(rl, ctx)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn evaluation() {
        assert_eq!(
            evalparse("(sameq (f (Sequence a b) c (Sequence d e)) (f a b c d e))"),
            sym("true")
        );
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
            let m = my_match(ex, pat.clone(), &pos, &mut pos_map, &mut named_map);
            let rebuilt_ex = rebuild_and_splice(pat.clone(), &named_map, &pos_map);
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
