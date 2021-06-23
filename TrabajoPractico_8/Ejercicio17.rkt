#lang racket

(define (invertir lista)
    (invertirAux lista '())
)

(define (invertirAux lista listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        (else 
            (invertirAux 
                (cdr lista) 
                (append (list (car lista)) listaAcumuladora)
            )
        )
    )
)