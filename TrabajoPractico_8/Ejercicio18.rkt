#lang racket

(define (xMayMen lista separador)
    (xMayMenAux lista separador '())
)

(define (xMayMenAux lista separador listaAcumuladora)
    (cond
        ((equal? (car lista) separador) (list listaAcumuladora (cdr lista)))
        (else 
            (xMayMenAux 
                (cdr lista) 
                separador 
                (append listaAcumuladora (list (car lista)))
            )
        )
    )
)