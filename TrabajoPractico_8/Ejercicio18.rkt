#lang racket

(define (xMayMen lista separador)
    (xMayMenAux lista separador '() '())
)

(define (xMayMenAux lista separador listaMenores listaMayores)
    (cond
        ((null? lista) (cons listaMenores (list listaMayores)))
        ((< (car lista) separador)
            (xMayMenAux (cdr lista) separador (cons (car lista) listaMenores) listaMayores)
        )
        ((> (car lista) separador)
            (xMayMenAux (cdr lista) separador listaMenores (cons (car lista) listaMayores))
        )
        (else
            (xMayMenAux (cdr lista) separador listaMenores listaMayores)
        )
    )
)