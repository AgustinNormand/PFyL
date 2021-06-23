#lang racket

(define (xMenores lista valor)
    (xMenoresAux lista valor '())
)

(define (xMenoresAux lista valor listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        (else 
            (cond
                ((< (car lista) valor) (xMenoresAux (cdr lista) valor (append listaAcumuladora (list (car lista)))))
                (else (xMenoresAux (cdr lista) valor listaAcumuladora))
            )
        )
    )
)

; Recursividad de pila
(define (xMenoresAuxPila lista valor)
    (cond
        ((null? lista) '())
        (else 
            (cond
                ((< (car lista) valor) (append (list (car lista)) (xMenoresAuxPila (cdr lista) valor)))
                (else (xMenoresAuxPila (cdr lista) valor))
            )
        )
    )
)