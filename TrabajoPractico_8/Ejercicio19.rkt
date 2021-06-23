#lang racket

(define (ocurrencias lista elementoBuscado)
    (ocurrenciasAux lista elementoBuscado 0 '())
)

(define (ocurrenciasAux lista elementoBuscado indiceActual listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        (
            (equal? elementoBuscado (car lista)) 
            (ocurrenciasAux 
                (cdr lista) 
                elementoBuscado 
                (+ indiceActual 1) 
                (append listaAcumuladora (list indiceActual))
            )
        )
        (
            else
            (ocurrenciasAux (cdr lista) elementoBuscado (+ indiceActual 1) listaAcumuladora)
        )
    )
)
