#lang racket

(define (xMayores lista valor)
    (xMayoresAux lista valor '())
)

(define (xMayoresAux lista valor listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        (else 
            (cond
                ((> (car lista) valor) (xMayoresAux (cdr lista) valor (append listaAcumuladora (list (car lista)))))
                (else (xMayoresAux (cdr lista) valor listaAcumuladora))
            )
        )
    )
)


