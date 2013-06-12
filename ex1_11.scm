; Recursive
(define (f n)
  (if (>= n 3)
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))
      n))

; Iterative
(define (f-iter n)
  (define (iter x y z n)
    (if (>= n 3)
        (iter (+ x (* 2 y) (* 3 z)) x y (- n 1))
        x))
  (if (>= n 3)
      (iter 2 1 0 n)
      n))
