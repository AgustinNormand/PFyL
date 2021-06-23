#lang racket

(define (obtenerPrimerosN lista cantidad)
    (obtenerPrimerosNAux lista cantidad '())
)

(define (obtenerPrimerosNAux lista cantidad listaAcumuladora)
    (cond
        ((= cantidad 0) listaAcumuladora)
        ((null? lista) listaAcumuladora)
        (else (obtenerPrimerosNAux (cdr lista) (- cantidad 1) (append listaAcumuladora (list (car lista)))))
    )
)

(define (obtenerSiguientes lista cantidad)
    (cond
        ((= cantidad 0) lista)
        ((null? lista) '())
        (else (obtenerSiguientes (cdr lista) (- cantidad 1)))
    )
)
 
(define (moverAtras lista cantidad)
    (append (obtenerSiguientes lista cantidad) (obtenerPrimerosN lista cantidad))
)