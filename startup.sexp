(set (attrs setd) (list HoldAll SequenceHold))
(set (attrs clear) (list HoldAll))
(set (attrs hold) (list HoldAll))
(set (attrs pattern) (list HoldFirst))

(set (attrs true) (list locked protected))
(set (attrs false) (list locked protected))

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

(set (Nest (pattern f (blank)) (pattern x (blank)) 0) x)
(set (Nest (pattern f (blank)) (pattern x (blank)) (pattern n (blank Int))) (f (Nest f x (Plus n -1))))

(set (Fac 1) 1)
(set (Fac (pattern n (blank Int))) (Times n (Fac (Plus n -1))))

(setd (First (list (pattern x (blank)) (pattern rest (blank_null_seq)))) x)
(setd (First (pattern xs (blank_null_seq))) (First (list xs)))
(setd (Rest (list (blank) (pattern rest (blank_null_seq)))) (list rest))
(setd (Rest (pattern xs (blank_null_seq))) (Rest (list xs)))

// note this definition is different than wolfram which gives some "Identity[a,b,c]" and a warning
(set (to_seq (list (pattern xs (blank_null_seq)))) xs)

(setd (Map (pattern f (blank)) (list)) (list))
(setd (Map (pattern f (blank)) (list (pattern xs (blank_seq)))) (list (f (First xs)) (to_seq (Map f (Rest (list xs))))))
