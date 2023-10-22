(setd (D (pattern c (blank Int)) (pattern x (blank Sym))) 0)
(setd (D (pattern c (blank Real)) (pattern x (blank Sym))) 0)

(setd (D (pattern x (blank Sym)) (pattern x (blank Sym))) 1)

(setd (D (Times (pattern a (blank Int)) (pattern x (blank Sym))) (pattern x (blank Sym))) a)
(setd (D (Times (pattern a (blank Real)) (pattern x (blank Sym))) (pattern x (blank Sym))) a)

(setd (D (Times (pattern x (blank Sym)) (pattern a (blank Int))) (pattern x (blank Sym))) a)
(setd (D (Times (pattern x (blank Sym)) (pattern a (blank Real))) (pattern x (blank Sym))) a)

(setd (D (Plus 
        (pattern expr1 (blank)) 
        (pattern expr2 (blank))) 
        (pattern x (blank Sym))) 
        (Plus (D expr1 x) (D expr2 x))) 

(setd (D (Times 
        (pattern expr1 (blank)) 
        (pattern expr2 (blank))) 
        (pattern x (blank Sym))) 
        (Plus (Times expr1 (D expr2 x)) (Times expr2 (D expr1 x))))

(setd (D (Power 
        (pattern expr1 (blank)) 
        (pattern n (blank Int))) 
        (pattern x (blank Sym))) 
        (Times n (Power expr1 (Plus n -1)) (D expr1 x)))

(setd (D (Exp 
        (pattern expr (blank))) 
        (pattern x (blank Sym))) 
        (Times (D expr x) (Exp expr)))

(setd (D (Sin 
        (pattern expr (blank))) 
        (pattern x (blank Sym))) 
        (Times (D expr x) (Cos expr)))


(setd (D (Cos (pattern expr (blank))) (pattern x (blank Sym))) (Times (Times -1 (D expr x)) (Sin expr)))
