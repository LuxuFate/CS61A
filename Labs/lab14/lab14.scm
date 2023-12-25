; Lab 14: Final Review

(define (compose-all funcs)
    (define (create x)
      (if (null? funcs)
        x
        ((compose-all(cdr funcs)) ((car funcs) x))
      )
    )
    create
)


(define (has-cycle? s)
  (define (pair-tracker seen-so-far curr)
    (cond ((null? curr) #f)
          ((contains? seen-so-far curr) #t)
          (else (pair-tracker (cons curr seen-so-far) (cdr-stream curr)))
    )
  )
  (pair-tracker nil s)
)

(define (contains? lst s)
  (if (null? lst)
    #f
    (if (eq? (car lst) s)
			#t
			(contains? (cdr lst) s)
    )
  )
)
