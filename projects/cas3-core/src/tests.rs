use super::*;

#[test]
fn test_parser() {
    assert_eq!(parse("(f (* *hi* *)  x)"), parse("(f x)"));
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
        evalparse("(replace x (List (rule a b) (rule x y)))"),
        sym("y")
    );
    assert_eq!(
        evalparse("(replace x (List (rule x y) (rule x z)))"),
        sym("y")
    );

    // doesn't keep going
    assert_eq!(
        evalparse("(replace x (List (rule x y) (rule y z)))"),
        sym("y")
    );

    // case where no rules apply
    assert_eq!(
        evalparse("(replace_all x (List (rule y a) (rule z b)))"),
        sym("x")
    );

    // test for blank with head + nested List
    assert_eq!(
        evalparse(r#"(replace_all (List 1 1.5 Pi (List a 2)) (rule (blank Int) "hi"))"#),
        expr_parser::Expr(r#"(List "hi" 1.5 Pi (List a "hi"))"#).unwrap()
    );

    assert_eq!(
        evalparse("(replace_all (List x (power x 2) y z) (List (rule x 1)))"),
        expr_parser::Expr("(List 1 (power 1 2) y z)").unwrap()
    );

    assert_eq!(
        evalparse("(replace_all (List x (power x 2) y z) (List (rule x 1) (rule y 2)))"),
        expr_parser::Expr("(List 1 (power 1 2) 2 z)").unwrap()
    );

    assert_eq!(
        evalparse("(replace_all (plus 1 (pow x 2) (pow x 4)) (rule (pow x (pattern p (blank))) (f p)))"),
        expr_parser::Expr("(plus 1 (f 2) (f 4))").unwrap()
    );

    let s = "(replace_repeated (List (f (f x)) (f x) (g (f x)) (f (g (f x)))) (List (rule (f (pattern x (blank))) x)))";
    // todo test s above to give (List x x (g x) (g x))
    assert_eq!(
        evalparse(s),
        expr_parser::Expr("(List x x (g x) (g x))").unwrap()
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
    let crules_str = "(List (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x))";
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
        evalparse("(sameq (Part (f x y z) (List 1 2 3)) (List x y z))"),
        sym("true")
    );
}

#[test]
fn seqs_and_geeks() {
    assert_eq!(
        evalparse("(sameq (f (Sequence a b) c (Sequence d e)) (f a b c d e))"),
        sym("true")
    );

    let mut ctx = Cas3VM {
        vars: HashMap::new(),
    };
    ctx.run_file( Path::new("lang/attrs.sexp")).unwrap();
    ctx_evalparse(
        &mut ctx,
        "(setd (listq (pattern x (blank))) (sameq list (head x)))",
    );
    println!("listq ctx {:?}", ctx);
    assert_eq!(ctx_evalparse(&mut ctx, "(listq (list a b c))"), sym("true"));

    ctx.run_file( Path::new("lang/startup.sexp")).unwrap();
    // issue #2
    assert_eq!(
        ctx_evalparse(&mut ctx, "(sameq (Nest (f) x 2) ((f) ((f) x)))"),
        sym("true")
    )
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
        let rebuilt_ex = final_rebuild_and_splice(pat.clone(), &vec![], &pos_map, &named_map);
        // let rebuilt_ex = rebuild_and_splice(pat.clone(), &vec![], &pos_map, &named_map);
        println!("rebuilt:{rebuilt_ex:?}\n\npos:\n{pos_map:?}\nnamed:\n{named_map:?}\n\n");

        assert_eq!(m, *expected);

        if *expected {
            assert_eq!(rebuilt_ex, ex.clone());
        }
    }
}

/// https://github.com/anandijain/cas3.rs/issues/1
#[test]
fn issue_1() {
    assert_eq!(
        evalparse("(matchq (f a b 0 c) (f (blank_seq) 0 (blank_seq)))"),
        sym("true")
    )
}

#[test]
fn table_tests() {
    let mut ctx = Cas3VM {
        vars: HashMap::new(),
    };
    ctx.run_file( Path::new("lang/attrs.sexp")).unwrap();
    ctx_evalparse(&mut ctx, "(set xs (List 1 2 3 4 5))");

    let cases = vec![
        ("(Table f 3)", "(List f f f)"),
        ("(Table i (List i 3))", "(List 1 2 3)"),
        ("(Table i (List i 2 4))", "(List 2 3 4)"),
        ("(Table i (List i 1 6 2))", "(List 1 3 5)"),
        ("(Table i (List i (List 1.5 3.5)))", "(List 1.5 3.5)"),
        (
            "(Table (List i j) (List i 2) (List j 2))",
            "(List (List (List 1 1) (List 1 2)) (List (List 2 1) (List 2 2)))",
        ),
        (
            "(Table (Part xs (List i (Plus i 1) (Plus i 2))) (List i (Plus (Length xs) -2)))",
            "(List (List 1 2 3) (List 2 3 4) (List 3 4 5))",
        ),
    ];
    for (lhs, rhs) in cases {
        let res = ctx_evalparse(&mut ctx, lhs);
        assert_eq!(res, expr_parser::Expr(rhs).unwrap());
    }
}

#[test]
fn issue_2() {
    let mut ctx = Cas3VM {
        vars: HashMap::new(),
    };
    ctx.run_file( Path::new("lang/attrs.sexp")).unwrap();
    ctx.run_file( Path::new("lang/startup.sexp")).unwrap();
    assert_eq!(
        ctx_evalparse(&mut ctx, "(sameq (Nest (f) x 2) ((f) ((f) x)))"),
        sym("true")
    )
}

#[test]
fn alternatives_test() {
    let mut ctx = Cas3VM {
        vars: HashMap::new(),
    };
    ctx.run_file( Path::new("lang/attrs.sexp")).unwrap();
    ctx.run_file( Path::new("lang/startup.sexp")).unwrap();
    let cases = vec![
        ("(matchq a (Alternatives a b))", sym("true")),
        ("(matchq (f a) (f (Alternatives a b)))", sym("true")),
        (
            "(matchq (f a b c) (Alternatives a (f (blank_seq))))",
            sym("true"),
        ),
        (
            "(matchq (f a b c) (Alternatives a (f (pattern xs (blank_seq)))))",
            sym("true"),
        ),
        (
            "(matchq (f a b c) (Alternatives a (f (blank_seq))))",
            sym("true"),
        ),
        (
            "(matchq (f) (Alternatives a (f (blank_seq))))",
            sym("false"),
        ),
        // ("(matchq a (Alternatives a b))", sym("true")),
    ];

    for (c, e) in cases {
        assert_eq!(ctx_evalparse(&mut ctx, c), e)
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
