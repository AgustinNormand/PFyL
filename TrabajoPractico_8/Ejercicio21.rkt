#lang racket

(define (listaAtomos lista)
    (cond
        ((null? lista) 
            '()
        )
        ((list? (car lista)) 
            (append 
                (listaAtomos (car lista)) 
                (listaAtomos (cdr lista))
            )
        )
        (else 
            (append 
                (list (car lista))
                (listaAtomos (cdr lista))
            )
        )
    )
)