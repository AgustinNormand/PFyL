#lang racket

(define (pertenece elemento lista)
    (cond
        ((null? lista) #f)
        ((equal? elemento (car lista)) #t)
        (else (pertenece elemento (cdr lista)))
    )
)