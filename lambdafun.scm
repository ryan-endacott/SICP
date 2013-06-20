
; List out of lambda!


; Type system!
(define +type-list+ 'list)
(define +type-number+ 'number)

(define (type x)
    (x (lambda (t . rest) t)))

(define (list? c)
  (eq? (type c) +type-list+))

(define (number? c)
  (eq? (type c) +type-number+))



; List!

(define (prepend a b)
  (lambda (fn) (fn (type b) a b)))

(define (head list)
  (list (lambda (t a b) a)))

(define empty
  (lambda (fn) (fn +type-list+)))

(define (empty? list) (eq? empty list))

(define (tail list)
  (list (lambda (t a b) b)))

(define (each fn list)
    (cond ((empty? list) empty)
        (else
          (fn (head list))
          (each fn (tail list)))))

(define (map fn list)
  (if (empty? list) empty
      (prepend (fn (head list)) (map fn (tail list)))))



; Numbers! 

; Numbers are defined as a list of zeroes :O
; (0, 0, 0) = 3
(define zero
  (lambda (fn t) (fn +type-number+)))

(define (zero? n) (eq? zero n))

(define (inc n)
  (prepend zero n))

(define (dec n)
  (if (zero? n) zero ; No negative nancies allowed!
      (tail n)))




; Prints an entire list
; or prints a number in real form
(define (print n)
  (define (getnum n)
    (if (zero? n) 0
        (+ 1 (getnum (dec n)))))
  (cond 
        ((list? n)
          (each (lambda (el) (print el) (display " ")) n))
        ((number? n) 
          (display (getnum n)) zero)))
         


; Some numbers to make typing easier..
(define one (inc zero))
(define two (inc one))
(define five (inc (inc (inc (inc one)))))

(define (add x y)
  (if (zero? y) x
     (add (inc x) (dec y))))

(define (sub x y)
  (if (zero? y) x
      (sub (dec x) (dec y))))

(define (mul x y)
  (if (zero? y) zero
      (add x (mul x (dec y)))))

; Some more numbers
(define ten (mul five two))
(define one-hundred (mul ten ten))
(define fifty (mul ten five))
(define forty-five (sub fifty five))
(define forty (sub fifty ten))
(define thirty (sub forty ten))
(define twenty (mul ten two))


(define (length list)
    (if (empty? list) zero
      (inc (length (tail list)))))



; Used to reset the REPL on REPL.it...
(define (loop) (loop))




