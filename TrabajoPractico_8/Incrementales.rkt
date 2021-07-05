#lang racket

(define (incremental? numero)
    (incrementalAux? (quotient numero 10) (modulo numero 10))
)

(define (incrementalAux? numeroRestante numeroAProcesar) 
    (cond
        ((= numeroRestante 0) #t)
        ((< numeroAProcesar (modulo numeroRestante 10)) #f)
        (else (incrementalAux? (quotient numeroRestante 10) (modulo numeroRestante 10)))
    )
)