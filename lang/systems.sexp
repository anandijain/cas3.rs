(* combinators *)
(set sk_rules 
    (list 
        (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) 
        (rule ((k (pattern x (blank))) (pattern y (blank))) 
        x)))

(set succ (s ((s (k s)) k)))
(setd (skn (pattern n (blank Int))) (Nest succ (s k) n))

(set sk_plus ((s (k s)) (s (k ((s (k s)) k)))))
(set sk_times ((s (k s)) k))
(set sk_pow ((s (k (s ((s k) k)))) k))

(* CA *)
(set (rule_30 (pattern p (blank)) (pattern r (blank)) (pattern q (blank))) (Xor p (Or r q)))

(setd (rule_30 
    (List 
        (pattern p (blank)) 
        (pattern r (blank)) 
        (pattern q (blank)))) 
    (Xor p (Or r q)))

(* (setd (pad_zero (pattern xs (blank List))) (Join (List 0) xs (List 0)))
(setd (idxs (pattern n (blank Int))) (Table (Plus i n_) (List n_ 0 n))) *)

(setd (pad_val 
    (pattern xs (blank List))
    (pattern val (blank)))
    (Join (List val) xs (List val)))

(setd (lil_partition3
    (pattern xs (blank List)))
    (Table (List (Part xs i) (Part xs (Plus i 1)) (Part xs (Plus i 2))) (List i (Plus (Length xs) -2))
    ))

(setd (foo (pattern xs (blank List)))
    (Map rule_30 (lil_partition3 (pad_val xs false))))

(set u0 (replace_all (List 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (List (rule 0 false) (rule 1 true))))

(set ls (replace_all (NestList foo u0 20) (List (rule false 0) (rule true 1))))

