#lang racket

#| Opcion1: Contar todos los elementos de la lista, asi saber a partir de que indice son los N ultimos|#
; No tan fea

#| Opcion2: Hacer una funcion que verifique si a partir de la posicion actual, hay N restantes|#
; Horrible

#| Opcion3: Recursividad de backtracking|#
; Buena

(define (size list)
    (sizeAux list 0)
)

(define (sizeAux lista acum)
    (cond
        ((null? lista) acum)
        (else (sizeAux (cdr lista) (+ acum 1)))
    )
)

(define (nUltimos cantidad lista)
    (elementsAfter (- (size lista) cantidad) lista)
)

(define (elementsAfter index lista)
    (elementsAfterAux index lista '() 0)
)

(define (elementsAfterAux index lista listaAcumuladora actualIndex)
    (cond
        ((null? lista) listaAcumuladora)
        ((>= actualIndex index) (elementsAfterAux index (cdr lista) (append listaAcumuladora (list (car lista))) (+ actualIndex 1)))
        ((< actualIndex index) (elementsAfterAux index (cdr lista) listaAcumuladora (+ actualIndex 1)))
    )
)

#|
(define (nUltimos cantidad lista)
    (nUltimosAux cantidad lista '())
)

(define (nUltimosAux cantidad lista listaAcumuladora)
    (cond
        ((null? lista) 0)
        (else (nUltimosAux cantidad (cdr lista) listaAcumuladora))

    )
            (cond
                ((> (+ (nUltimosAux cantidad (cdr lista) listaAcumuladora) 1) cantidad) (append (list (car lista)) listaAcumuladora))
                (else listaAcumuladora)
            )
        )
    )
)|#