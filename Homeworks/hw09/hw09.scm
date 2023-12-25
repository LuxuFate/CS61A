
; Tail recursion

(define (replicate x n)
  (define (helper x n lst)
    (if (= n 0)
      lst
      (helper x (- n 1) (cons x lst))
    )
  )
  (helper x n nil)
)

(define (accumulate combiner start n term)
  (define (helper combiner n term)
    (if (= n 1)
      (term n)
      (combiner (term n) (helper combiner (- n 1) term))
    )
  )
  (combiner start (helper combiner n term))
)

(define (accumulate-tail combiner start n term)
  (define (helper combiner n term total)
    (if (= n 0)
      total
      (helper combiner (- n 1) term (combiner total (term n)))
      )
    )
  (helper combiner n term start)
)

; Streams

(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (cons-stream 3 (map-stream (lambda (x) (+ x 3)) multiples-of-three))
)

(define (cut len s)
  (if (= len 0)
    s
    (cut (- len 1) (cdr-stream s))
  )
)

(define (add s list)
  (if (null? (cdr-stream s))
    (append list (cons (car s) nil))
    (if (<= (car s) (car (cdr-stream s)))
      (add (cdr-stream s) (append list (cons (car s) nil)))
      (append list (cons (car s) nil))
    )
  )
)

(define (nondecreastream s)
  (if (null? (cdr-stream s))
    (cons-stream (list(car s)) nil)
    (if (<= (car s) (car (cdr-stream s)))
      (cons-stream
        (add s nil)
        (nondecreastream (cut (length (add s nil)) s)))
      (nondecreastream (cdr-stream s))
    )
  )
)


(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))
