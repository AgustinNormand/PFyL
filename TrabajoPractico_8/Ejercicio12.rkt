#lang racket

(define (aPartir indice lista)
    (aPartirAux indice lista 0)
)

(define (aPartirAux indice lista indiceActual)
    (cond
        ((>= indiceActual indice) lista)
        (else (aPartirAux indice (cdr lista) (+ indiceActual 1)))
    )
)