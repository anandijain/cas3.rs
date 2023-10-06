(set (attrs setd) (list HoldAll))
(set (attrs clear) (list HoldAll))
(set (attrs hold) (list HoldAll))
(set (attrs pattern) (list HoldFirst))

(set (attrs True) (list locked protected))
(set (attrs False) (list locked protected))

(set (And True True) True)
(set (And True False) False)
(set (And False True) False)
(set (And False False) False)

(set (Or True True) True)
(set (Or True False) True)
(set (Or False True) True)
(set (Or False False) False)

(set (Not True) False)
(set (Not False) True)

(set (Not (Not (pattern x (blank)))) x)

(set (Nest f x 0) x)
(set (Nest f x (pattern n (blank Int))) (f (Nest f x (Plus n -1))))

(set (Fac 1) 1)
(set (Fac (pattern n (blank Int))) (Times n (Fac (Plus n -1))))
