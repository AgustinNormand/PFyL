#lang racket

(define (sumaElemM lista)
    (cond
        ((null? lista) 0)
        ((list? (car lista)) (+ (sumaElemM (car lista)) (sumaElemM (cdr lista))))
        (else (+ (car lista) (sumaElemM (cdr lista))))
    )
)