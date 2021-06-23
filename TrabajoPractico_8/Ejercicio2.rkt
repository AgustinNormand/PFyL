#lang racket

; Recursividad de pila
(define (tamano lista)
    (cond
        ((null? lista) 0)
        (else (+ 1 (tamano (cdr lista))))
    )
)

;Recursividad de cola
(define (tamanoCola lista)
    (tamanoAux lista 0)
)

(define (tamanoAux lista contador)
    (cond
        ((null? lista) contador)
        (else (tamanoAux (cdr lista) (+ contador 1)))
    )
)