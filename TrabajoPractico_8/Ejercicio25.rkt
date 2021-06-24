#lang racket

(define (maxM lista)
    (maxMAux lista (obtenerPrimerAtomo lista))
)

(define (obtenerPrimerAtomo lista)
    (cond
        ((null? lista) -1)
        ((list? (car lista)) (obtenerPrimerAtomo (car lista)))
        (else (car lista))
    )
)

(define (maxMAux lista mayor)
    (cond
        ((null? lista) mayor)
        ((list? (car lista))
            (cond
                (
                    (>= 
                        (maxMAux (car lista) mayor) 
                        (maxMAux (cdr lista) mayor)
                    )
                    (maxMAux (car lista) (maxMAux (car lista) mayor))
                )
                (
                    else 
                    (maxMAux (cdr lista) (maxMAux (cdr lista) mayor))
                )
            )
        )
        (else 
            (cond
                ((>= (car lista) mayor) (maxMAux (cdr lista) (car lista)))
                (else (maxMAux (cdr lista) mayor))
                
            )
        )
    )
)

; Como el codigo anterior quedó medio confuso, 
; por ahi es mejor, planchar la lista primero,
; y despues buscar el mayor

(define (aplanar lista)
    (cond
        ((null? lista) '())
        ((list? (car lista)) (append (aplanar (car lista)) (aplanar (cdr lista))))
        (else (append (list (car lista)) (aplanar (cdr lista))))
    )
)


(define (min lista)
    (minAux (cdr (aplanar lista)) (car lista))
)

(define (minAux lista menor)
    (cond
        ((null? lista) menor)
        ((<= (car lista) (minAux (cdr lista) menor)) (minAux (cdr lista) (car lista)))
        (else (minAux (cdr lista) menor))
    )
)