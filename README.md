# cas3.rs

## Running cas3:
To run cas3.rs, make sure rust is installed, and run `cargo run --release` in the root directory.

## language highlights - todo make sure these are all tested
the entire code block can be copy and pasted into the REPL
```wl
(* Computing with Combinators *)
(* First we define the sk rules *)
(set sk_rules (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))

(* now we define the rule for incrementing *)
(set succ (s ((s (k s)) k)))

(* now we apply succ to (s k) 10 times *)
(Nest succ (s k) 10)

(* now we apply [s][k] to the result and do fixed-point replacement using our SK-rules *)
(rr (((Nest succ (s k) 10) s) k) sk_rules)
(* you should see: *)
(* (s (s (s (s (s (s (s (s (s (s k)))))))))) *)

(* now we define a helper function to get an sk representation of a Natural number *)
(setd (skn (pattern n (blank Int))) (Nest succ (s k) n))

(* it turns out the following is the sk representation of plus *)
(* in the future i would like to do a search to find this  *)
(set sk_plus ((s (k s)) (s (k ((s (k s)) k)))))

(* so now we can compute 60 + 9 using sk_plus *)
(rr ((((sk_plus (skn 60)) (skn 9)) s) k) sk_rules)

(* multiplication  *)
(set sk_times ((s (k s)) k))
(rr ((((sk_times (skn 7)) (skn 7)) s) k) sk_rules)

(* pow  *)
(set sk_pow ((s (k (s ((s k) k)))) k))
(rr ((((sk_pow (skn 2)) (skn 4)) s) k) sk_rules)

(* big ints, look, no overflow  *)
(Fac 1000)

(* Symbolic Differentiation  *)
(* (see definition of `D` in ./lang/calculus.sexp) *)
(* note Flat and Orderless are attributes are not implemented so the derivative, while correct, is not in its simplest form *)
(D (Plus (Power x 2) (Times 3 x)) x)
(D (Times (Sin x) (Cos x)) x)
(D (Exp (Power x 2)) x)
```


my notes: 

https://reference.wolfram.com/language/tutorial/EvaluationOfExpressions.html
https://reference.wolfram.com/language/tutorial/TheInternalsOfTheWolframSystem.html
https://reference.wolfram.com/language/tutorial/SomeNotesOnInternalImplementation.html

"The Wolfram Language is an infinite evaluation system. When you enter an expression, the Wolfram Language will keep on using definitions it knows until it gets a result to which no definitions apply."

"In the standard evaluation procedure, the Wolfram System first evaluates the head of an expression and then evaluates each element of the expression. These elements are in general themselves expressions, to which the same evaluation procedure is recursively applied."

"A pattern like f[x__,y__,z__] can match an expression like f[a,b,c,d,e] with several different choices of x, y, and z. The choices with x and y of minimum length are tried first. In general, when there are multiple __ or `___` in a single function, the case that is tried first takes all the __ and `___` to stand for sequences of minimum length, except the last one, which stands for "the rest" of the arguments."

https://mathematica.stackexchange.com/questions/96/what-is-the-distinction-between-downvalues-upvalues-subvalues-and-ownvalues

https://mathematica.stackexchange.com/questions/192278/is-there-a-defined-priority-for-pattern-matching
https://reference.wolfram.com/language/tutorial/TransformationRulesAndDefinitions.html#26982

https://reference.wolfram.com/language/tutorial/Evaluation.html


this links to a book that describes the evaluation order (section 7.1.3)
https://mathematica.stackexchange.com/questions/16485/are-you-interested-in-purchasing-david-wagners-power-programming-with-mathemat


combinators 
https://writings.stephenwolfram.com/2020/12/combinators-a-centennial-view/
https://writings.stephenwolfram.com/2020/12/combinators-and-the-story-of-computation/

```mathematica
k[x_][y_] := x
s[x_][y_][z_] := x[z][y[z]]

or 
EXPR //. {s[x_][y_][z_] -> x[z][y[z]], k[x_][y_] -> x}
```

goals of project:
* simulate a variety of the computational systems in NKS (cellular automata, combinators, turing machines, etc)
* provide a simple educational example for how to build a symbolic language and how the wolfram language works
    - personally, I think this is already a much better example than Mathics 

random todo - not critical for combinator reduction

* improve parser (whitespace and EOF robustness) - infix/m-expr if im feeling naughty
* arb numerics- switch to rug/gmp for all number types - this will be a journey 
* string escaping 
* levels have a simple structure that spans wl. like replace with {{}} makes a list applying rules separately, 
* having x_Head infix syntax would be really nice 
* being able to paste in multiple expressions and have them all evaluate
* list operations
* id really like to make trace work but idk how 
* make pattern matching for __ (BlankSequence) and ___ (BlankNullSequence)
* need a ClearAll
* Function for anonymous functions
* subvalues 
* options 
* caching/memoization. fib[3] gets cached in the evaluation of fib[5]. can see this by looking at DownValues
* `Block` and `Module` . todo- find the post that shows it, i cant find it atm 
* fix nonsense that printing 3.0 actually prints `3` which is indistinguishable from exact 3
* its time to take a real look at how we insert values into symbol table. for instance redefining a function (downvalue) just pushes, so the old one still matches even though its the one you dont want 
* Cases and Position
* tab completion 
* ReleaseHold

completed:
* evaluataion control
* attributes system (mainly just hold*, i don't need listable yet)
* basic clear function
* integers have arbitrary width 
* make factorial and fib / recursive functions work (depends on numerics)

Here are some examples of how to use the language. see startup.sexp or tests for more 

(pretty sure my nand is incorrectly translated)
```scheme
(set xb (pattern x (blank)))
(set yb (pattern y (blank)))
(set zb (pattern z (blank)))

(set l1 (((s xb) yb) zb))
(set r1 ((x z) (y z)))
(set rule1 (rule l1 r1))
(set l2 ((k xb) yb))
(set r2 x)
(set rule2 (rule l2 r2))
(set crules (list rule1 rule2))

(set crules (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))
(set nand ((s (s (k (s ((s s) (s (k (k k)))))))) s))


(rr ((nand (s k)) (s k)) crules)
(rr ((nand (s k)) k) crules)
(rr ((nand k) (s k)) crules)
(rr ((nand k) k) crules)


(set (fib 0) 0)
(set (fib 1) 1)
(set (fib (pattern n (blank Int))) (Plus (fib (Plus n -2)) (fib (Plus n -1))))
(fib 10)


(set (Part (pattern xs (blank list)) All) xs)

```

need to fix panic. 
in wl this returns: `{s[x_][y_][z_]->1[z][y[z]],k[x_][y_]->1}`

```scheme
(set x 1)


thread 'main' panicked at 'head must be a symbol, got 1', src/main.rs:437:68
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

```


pattern matcher examples with __ and ___
```wl
In[7]:= MatchQ[g[a], g[a, ___]]

Out[7]= True

In[8]:= MatchQ[Null, _]

Out[8]= True

In[10]:= MatchQ[g[], g[___,_]]

Out[10]= False

In[11]:= MatchQ[g[a], g[___,_]]

Out[11]= True

In[12]:= ReplaceAll[g[a], g[xs___,x_]->{xs, x}]

Out[12]= {a}

In[14]:= ReplaceAll[g[a,b,c], g[xs__,x_]->{{xs}, {x}}]

Out[14]= {{a,b},{c}}

In[15]:= ReplaceAll[g[a,b,c], g[xs___,ys___,x_]->{{xs},{ys}, {x}}]

Out[15]= {{},{a,b},{c}}

In[27]:= ReplaceAll[g[a,b,c,d], g[xs__,ys___,x_]->{{xs},{ys}, {x}}]

Out[27]= {{a},{b,c},{d}}

In[34]:= ReplaceAll[g[a,b,a, b,c], g[xs__,xs___,x_]->{{xs},{xs}, {x}}]

Out[34]= {{a,b},{a,b},{c}}

In[38]:= ReplaceAll[g[a,b,c, a,b,d], g[xs__,ys___,xs__,x_]->{{xs},{ys}, {x}}]

Out[38]= {{a,b},{c},{d}}

In[39]:= ReplaceAll[g[a,b,c, a,b,d], g[xs___,ys___,xs___,x_]->{{xs},{ys}, {x}}]

Out[39]= {{},{a,b,c,a,b},{d}}

In[35]:= MatchQ[g[a,b,a,b],g[xs__,xs__]]

Out[35]= True

In[36]:= MatchQ[g[a,b,a,c],g[xs__,xs__]]

Out[36]= False

In[8]:= MatchQ[f[a,a],f[x__,x_]]

TemplateBox[{"Pattern", "patv", "\"\:f7c1\:f7c9\:f7c8RowBox[{\"\\:f3b5Name \", StyleBox[TagBox[\"x\", Function[Short[Slot[1], 5]]], ShowStringCharacters -> False], \" used for both fixed and variable length patterns.\\:f3b5\"}]\:f7c0\"", 2, 8, 2, 19654811836800435566, "Local"}, "MessageTemplate"]

Out[8]= True

```

walkthrough for (f a b c ) (f x__ y_)
we cross off f giving
ex = (a b c) 
pat = (x__ y_)

now we start with __ being as short as possible, meaning 1 
so x__ -> Sequence[a], then we set y_ -> b,
so bindings = {x__ -> Sequence[a], y_ -> b}
now we need a way to say, "is this a valid solution/match?"
i think the way that this can be accomplished is by evaluating 

SameQ[ex, pat/.bindings]. if not we need to backtrack.
so lets start completely over (not taking care of optimality or w/e)
so we make a note that the first bindings were wrong.
so we go to the next possible binding 
x-> Sequence[a,b], y_ -> c
SameQ[ex, pat/.bindings], which ends up being true. 

now working through 
In[12]:= f[a,b,c]/. f[x__,y___,z_]-> {{x}, {y}, {z}}

Out[12]= {{a},{b},{c}}

again cross off f 
ex = (a b c)
pat = (x__ y___ z_)
now we take x to be len 1, y 0, z 1. summing lengths, we see that 2 != 3, so we backtrack
the question is why do we not take x to be [a, b] and y to be [c]?
we have access to the information, which patterns in `pat` can be increased, ie which are __ or ___.
the solution is go to the last possible seq pattern and increase it.
so we go back to y___ and increase it to take y -> Sequence[b], and z -> c, finding lengths equal, we are done

(set crules (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))

In[16]:= f[a,b,c]/. f[x___,y__,z_]-> {{x}, {y}, {z}}

Out[16]= {{},{a,b},{c}}

in this case we start with a ___, give it length zero. 
give y a, z ->b , lengths dont match, so we backtrack to y, not x 
y -> {a,b}, z -> c, lengths match, we are done

((g x) ((g x) ((g x) y)))

((g x) ((g x) ((g x) y)))
