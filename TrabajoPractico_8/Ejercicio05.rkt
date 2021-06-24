#lang racket

; Recursividad de pila
(define (estaEntre limiteInferior limiteSuperior)
    (cond
        ((= limiteInferior limiteSuperior) (list limiteInferior))
        (else (append (list limiteInferior) (estaEntre (+ limiteInferior 1) limiteSuperior)))
    )
)

; Recursividad de cola
(define (estaEntreCola limiteInferior limiteSuperior)
    (estaEntreAux limiteInferior limiteSuperior '())
)

(define (estaEntreAux limiteInferior limiteSuperior listaAcumuladora)
    (cond
        ((> limiteInferior limiteSuperior) listaAcumuladora)
        (else (estaEntreAux (+ limiteInferior 1) limiteSuperior (append listaAcumuladora (list limiteInferior))))
    )
)