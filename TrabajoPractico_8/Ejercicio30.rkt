#lang racket

(define (xReemplazarM valorBuscado valorReemplazo lista)
    (cond
        ((null? lista) '())
        ((list? (car lista)) 
            (cons 
                (xReemplazarM valorBuscado valorReemplazo (car lista))
                (xReemplazarM valorBuscado valorReemplazo (cdr lista))
            )
        )
        ((equal? (car lista) valorBuscado) 
            (append 
                (list valorReemplazo) 
                (xReemplazarM valorBuscado valorReemplazo (cdr lista))
            )
        )
        (else 
            (append 
                (list (car lista)) 
                (xReemplazarM valorBuscado valorReemplazo (cdr lista))
            )
        )
    )
)