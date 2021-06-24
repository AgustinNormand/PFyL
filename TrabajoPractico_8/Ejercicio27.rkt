#lang racket

(define (aplanar lista)
    (cond
        ((null? lista) '())
        ((list? (car lista)) (append (aplanar (car lista)) (aplanar (cdr lista))))
        (else (append (list (car lista)) (aplanar (cdr lista))))
    )
)

(define (consecutivoM lista)
    (consecutivo (aplanar lista))
)

(define (consecutivo lista)
    (consecutivoAux (cdr lista) (car lista))
)

(define (consecutivoAux lista elementoAnterior)
    (cond
        ((null? lista) #t)
        ((< (car lista) elementoAnterior) #f)
        (else (consecutivoAux (cdr lista) (car lista)))
    )
)