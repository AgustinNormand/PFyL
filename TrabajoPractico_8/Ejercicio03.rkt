#lang racket

; Recursividad de pila
(define (sumatoria lista)
    (cond
        ((null? lista) 0)
        (else (+ (sumatoria (cdr lista)) (car lista)))
    )
)

; Recursividad de cola
(define (sumatoriaCola lista)
    (sumatoriaAux lista 0)
)

(define (sumatoriaAux lista acumulador)
    (cond
        ((null? lista) acumulador)
        (else (sumatoriaAux (cdr lista) (+ acumulador (car lista))))
    )
)