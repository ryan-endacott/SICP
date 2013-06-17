
(define (p' p q)
  (+ (* p p) (* q q)))

(define (q' p q)
  (+ (* q q) (* p q 2)))

(define (fib n)
    (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (p' p q)      ; compute p'
                   (q' p q)      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
