;; Clear[MyD]
;; (* Rule for constants *)
;; MyD[c_?NumericQ, x_] := 0

(setd (D (pattern c (blank Int)) (pattern x (blank Sym))) 0)
(setd (D (pattern c (blank Real)) (pattern x (blank Sym))) 0)


;; // MyD[x_, x_] := 1

(setd (D (pattern x (blank Sym)) (pattern x (blank Sym))) 1)

;; // MyD[a_?NumericQ x_, x_] := a

(setd (D (Times (pattern a (blank Int)) (pattern x (blank Sym))) (pattern x (blank Sym))) a)
(setd (D (Times (pattern a (blank Real)) (pattern x (blank Sym))) (pattern x (blank Sym))) a)

(setd (D (Times (pattern x (blank Sym)) (pattern a (blank Int))) (pattern x (blank Sym))) a)
(setd (D (Times (pattern x (blank Sym)) (pattern a (blank Real))) (pattern x (blank Sym))) a)

;; // (* Rule for sum *)
;; // MyD[expr1_ + expr2_, x_] := MyD[expr1, x] + MyD[expr2, x]

;; // todo make a lot of these variadic 
(setd (D (Plus (pattern expr1 (blank)) (pattern expr2 (blank))) (pattern x (blank Sym))) (Plus (D expr1 x) (D expr2 x))) 

;; // (* Rule for product *)
;; //MyD[expr1_ expr2_, x_] := expr1 MyD[expr2, x] + expr2 MyD[expr1, x]
(setd (D (Times (pattern expr1 (blank)) (pattern expr2 (blank))) (pattern x (blank Sym))) (Plus (Times expr1 (D expr2 x)) (Times expr2 (D expr1 x))))

;; (* Rule for power *)
;; MyD[expr1_^n_, x_] := n expr1^(n - 1) MyD[expr1, x]
(setd (D (Power (pattern expr1 (blank)) (pattern n (blank Int))) (pattern x (blank Sym))) (Times n (Power expr1 (Plus n -1)) (D expr1 x)))

;; (* Rule for exponentials *)
;; MyD[Exp[expr_], x_] := MyD[expr, x] Exp[expr]
(setd (D (Exp (pattern expr (blank))) (pattern x (blank Sym))) (Times (D expr x) (Exp expr)))

;; (* Rule for trigonometric functions *)
;; MyD[Sin[expr_], x_] := MyD[expr, x] Cos[expr]
(setd (D (Sin (pattern expr (blank))) (pattern x (blank Sym))) (Times (D expr x) (Cos expr)))
;; MyD[Cos[expr_], x_] := -MyD[expr, x] Sin[expr]
(setd (D (Cos (pattern expr (blank))) (pattern x (blank Sym))) (Times (Times -1 (D expr x)) (Sin expr)))

;; (* Test cases *)
;; MyD[x^2 + 3 x + 2, x]
;; (D (Plus (Power x 2) (Times 3 x) 2) x)
;; MyD[Sin[x] Cos[x], x]
;; (D (Times (Sin x) (Cos x)) x)
;; MyD[Exp[x^2], x]
;; (D (Exp (Power x 2)) x)

