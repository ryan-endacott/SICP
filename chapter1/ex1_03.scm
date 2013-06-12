(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (square-sum-larger a b c)
    (if (> a b)
        (sum-of-squares a (if (> b c) b c))
        (sum-of-squares b (if (> a c) a c))))
