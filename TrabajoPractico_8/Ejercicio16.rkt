#lang racket

(define (siguientes elemento lista)
    (cond
        ((null? lista) '())
        ((equal? (car lista) elemento) (cdr lista))
        (else (siguientes elemento (cdr lista)))
    )
)
