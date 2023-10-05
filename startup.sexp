(set (attrs setd) (list HoldAll))
(set (attrs clear) (list HoldAll))

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
