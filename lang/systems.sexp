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