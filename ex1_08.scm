(define (cube x) (* x x x))

(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) (* x .0001)))
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))
  (cube-iter 1.0))
