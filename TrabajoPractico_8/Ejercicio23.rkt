#lang racket

(define (perteneceM elementoBuscado lista)
    (cond
        ((null? lista) #f)
        ((list? (car lista))
            (or 
                (perteneceM elementoBuscado (car lista))
                (perteneceM elementoBuscado (cdr lista))
            )
        )
        (else 
            (or 
                (equal? elementoBuscado (car lista)) 
                (perteneceM elementoBuscado (cdr lista))
            )
        )
    )
)