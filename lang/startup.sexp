(* bool *)
(set (And true true) true)
(set (And true false) false)
(set (And false true) false)
(set (And false false) false)

(set (Or true true) true)
(set (Or true false) true)
(set (Or false true) true)
(set (Or false false) false)

(set (Xor true true) false)
(set (Xor true false) true)
(set (Xor false true) true)
(set (Xor false false) false)

(set (Nand true true) false)
(set (Nand true false) true)
(set (Nand false true) true)
(set (Nand false false) true)

(set (Not true) false)
(set (Not false) true)

(set (Not (Not (pattern x (blank)))) x)

(setd (If true (pattern x (blank)) (pattern y (blank))) x)
(setd (If false (pattern x (blank)) (pattern y (blank))) y)

(setd (If true (pattern x (blank))) x)
(setd (If false (pattern x (blank))) Null)

(setd (If true (pattern x (blank)) (pattern y (blank)) (pattern z (blank))) x)
(setd (If false (pattern x (blank)) (pattern y (blank)) (pattern z (blank))) y)
(setd (If (pattern t (blank)) (pattern x (blank)) (pattern y (blank)) (pattern z (blank))) z)

(* (setd (Boole 0) false)
(setd (Boole 1) true) *)

(* Identity laws for addition *)
(* Adding 0 to any number results in the number itself *)
(setd (Plus (pattern x (blank)) 0) x)
(setd (Plus 0 (pattern x (blank))) x)

(* Identity laws for multiplication *)
(* Multiplying any number by 1 results in the number itself *)
(setd (Times (pattern x (blank)) 1) x)
(setd (Times 1 (pattern x (blank))) x)

(* Absorbing laws for multiplication *)
(* Multiplying any number by 0 results in 0 *)
(* (setd (Times (pattern x (blank)) 0) 0) *)
(* (setd (Times 0 (pattern x (blank))) 0) *)
(setd (Times (pattern xs (blank_null_seq)) 0 (pattern ys (blank_null_seq))) 0)
(setd (Times (pattern xs (blank_null_seq)) 1 (pattern ys (blank_null_seq))) (Times xs ys))


(* Identity laws for exponentiation *)
(* Raising any number to the power of 1 results in the number itself *)
(* Raising any number to the power of 0 results in 1 *)
(setd (Power (pattern x (blank)) 1) x)
(setd (Power (pattern x (blank)) 0) 1)

(setd (Nest (pattern f (blank)) (pattern x (blank)) 0) x)
(setd (Nest (pattern f (blank)) (pattern x (blank)) (pattern n (blank Int))) (f (Nest f x (Plus n -1))))

(set (Fac 1) 1)
(set (Fac (pattern n (blank Int))) (Times n (Fac (Plus n -1))))

(setd (First (list (pattern x (blank)) (pattern rest (blank_null_seq)))) x)
(setd (First (pattern xs (blank_null_seq))) (First (list xs)))
(setd (Rest (list (blank) (pattern rest (blank_null_seq)))) (list rest))
(setd (Rest (pattern xs (blank_null_seq))) (Rest (list xs)))

(* note this definition is different than wolfram which gives some "Identity[a,b,c]" and a warning *)
(set (to_seq (list (pattern xs (blank_null_seq)))) xs)

(* broken, implemented kernel side for now *)
(* (setd (Map (pattern f (blank)) (list)) (list)) *)
(* (setd (Map (pattern f (blank)) (list (pattern xs (blank_seq)))) (list (f (First xs)) (to_seq (Map f (Rest (list xs)))))) *)

(setd (ListQ (pattern x (blank))) (sameq list (head x)))

(setd (Succ (pattern n (blank Int))) (Plus n 1))

(* works but is not that general *)
(* for example *)

(* Range[x, x + 4] *)
(* Range[1.2, 2.2, 0.15] *)
(setd (Range (pattern n (blank Int))) (Table i (list i n)))
(setd (Range 
    (pattern imin (blank Int))
    (pattern imax (blank Int))
    ) (Table i (list i imin imax)))

(setd (Range 
    (pattern imin (blank Int))
    (pattern imax (blank Int))
    (pattern di (blank Int))
    ) (Table i (list i imin imax di)))

