#lang racket

(define (longM lista)
    (cond
        ((null? lista) 0)
        ((list? (car lista)) (+ (longM (car lista)) (longM (cdr lista))))
        (else (+ 1 (longM (cdr lista))))
    )
)
