(define (pascal row col)
  (cond ((< row 1) 0)
        ((< col 1) 0)
        ((> col row) 0)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))
