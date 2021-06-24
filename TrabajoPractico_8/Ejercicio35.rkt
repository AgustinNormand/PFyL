#lang racket

(define (heladosPosiblesSinRepeticion lista)
    (cond
        ((null? lista) '())
        (else 
            (append 
                (heladosPosiblesSinRepeticionAux (cdr lista) (car lista)) 
                (heladosPosiblesSinRepeticion (cdr lista))
            )
        )
    )
    
)

(define (heladosPosiblesSinRepeticionAux lista listaAcumuladora)
(cond
        ((null? lista) '())
        (else
            (cons
                (cons listaAcumuladora (list (car lista)))
                (heladosPosiblesSinRepeticionAux (cdr lista) listaAcumuladora)
            )
        )
    )
)

(define (heladosPosiblesConRepeticion lista)
    (cond
        ((null? lista) '())
        (else 
            (append 
                (heladosPosiblesConRepeticionAux lista (car lista)) 
                (heladosPosiblesConRepeticion (cdr lista))
            )
        )
    )
    
)

(define (heladosPosiblesConRepeticionAux lista listaAcumuladora)
(cond
        ((null? lista) '())
        (else
            (cons
                (cons listaAcumuladora (list (car lista)))
                (heladosPosiblesConRepeticionAux (cdr lista) listaAcumuladora)
            )
        )
    )
)



