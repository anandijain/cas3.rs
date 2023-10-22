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

(setd (Boole 0) false)
(setd (Boole 1) true)

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

(setd (listq (pattern x (blank))) (sameq list (head x)))


(setd (Table (pattern val (blank)) 0) (list))
(setd (Table (pattern val (blank)) (pattern n (blank Int))) (list val (to_seq (Table val (Plus n -1)))))



(* (setd (Partition (pattern list (blank list)) (pattern n (blank Int)) (pattern d (blank Int)))) *)