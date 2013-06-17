
(define (even? n)
  (= (mod n 2) 0))

; Assume defined already
(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (mult a b)
  (cond
      ((= b 0) 0)
      ((even? b) (double (mult a (halve b))))
      (else (+ a (mult a (- b 1))))))
