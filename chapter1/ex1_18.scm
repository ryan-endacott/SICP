(define (even? n)
  (= (mod n 2) 0))

; assume defined
(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (mult a b)
  (define (mult-iter a b c)
        (cond
            ((= b 0) c)
            ((even? b) (mult-iter (double a) (halve b) c))
            (else (mult-iter a (- b 1) (+ c a)))))
  (mult-iter a b 0))
