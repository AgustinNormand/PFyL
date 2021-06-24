#lang racket

(define (ocurreM elementoBuscado lista)
    (cond
        ((null? lista) 0)
        ((list? (car lista)) 
            (+ 
                (ocurreM elementoBuscado (car lista)) 
                (ocurreM elementoBuscado (cdr lista))
            )
        )
        ((equal? (car lista) elementoBuscado) (+ 1 (ocurreM elementoBuscado (cdr lista))))
        (else (ocurreM elementoBuscado (cdr lista)))
    )
)