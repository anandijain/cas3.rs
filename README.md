# cas3.rs

https://reference.wolfram.com/language/tutorial/EvaluationOfExpressions.html

https://reference.wolfram.com/language/tutorial/TheInternalsOfTheWolframSystem.html
https://reference.wolfram.com/language/tutorial/SomeNotesOnInternalImplementation.html

"The Wolfram Language is an infinite evaluation system. When you enter an expression, the Wolfram Language will keep on using definitions it knows until it gets a result to which no definitions apply."

"In the standard evaluation procedure, the Wolfram System first evaluates the head of an expression and then evaluates each element of the expression. These elements are in general themselves expressions, to which the same evaluation procedure is recursively applied."

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

(set crules (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))

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

```