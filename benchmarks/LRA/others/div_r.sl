(set-logic LRA)

(synth-inv inv_fun ((x Real) (n Real)))

(define-fun pre_fun ((x Real) (n Real)) Bool
    (= x n))
(define-fun trans_fun ((x Real) (n Real) (x! Real) (n! Real)) Bool
    (and (and (>= x 1.) (= x! (/ x 2.))) (= n! n)))
(define-fun post_fun ((x Real) (n Real)) Bool
    (not (and (<= x 0.) (and (not (= x 0.)) (>= n 0.)))))


(inv-constraint inv_fun pre_fun trans_fun post_fun)

(check-synth)
