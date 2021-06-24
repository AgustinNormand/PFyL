#lang racket

(define (ultimo lista)
    (cond
        ((null? lista) '())
        ((null? (cdr lista)) (car lista))
        (else (ultimo (cdr lista)))
    )
)