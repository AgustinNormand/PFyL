#lang racket

(define (formatoReducido lista)
    (formatoReducidoAux (cdr lista) (car lista) 1)
)

(define (formatoReducidoAux lista elementoConsecutivo repeticiones)
    (cond
        ((null? lista) (list (cons elementoConsecutivo repeticiones)))
        ((equal? (car lista) elementoConsecutivo) 
            (formatoReducidoAux (cdr lista) elementoConsecutivo (+ repeticiones 1)))
        ((not (equal? (car lista) elementoConsecutivo)) 
            (cons 
                (cons elementoConsecutivo repeticiones) 
                (formatoReducidoAux lista (car lista) 0)
            )
        )
    )
)