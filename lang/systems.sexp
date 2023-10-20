(set (rule_30 (pattern p (blank)) (pattern r (blank)) (pattern q (blank))) (Xor p (Or r q)))

(set sk_rules (list (rule (((s (pattern x (blank))) (pattern y (blank))) (pattern z (blank))) ((x z) (y z))) (rule ((k (pattern x (blank))) (pattern y (blank))) x)))


(set succ (s ((s (k s)) k)))