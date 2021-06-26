#lang racket

(define (escribir elemento veces)
    (escribirAux elemento veces '())
)

(define (escribirAux elemento veces listaAcumuladora)
    (cond
        ((= veces 0) listaAcumuladora)
        (else (escribirAux elemento (- veces 1) (append (list elemento) listaAcumuladora)))
    )
)

(define (formatoAmpliado paresOrdenados)
    (formatoAmpliadoAux paresOrdenados '())
)

(define (formatoAmpliadoAux paresOrdenados listaAcumuladora)
    (cond
        ((null? paresOrdenados) listaAcumuladora)
        (else (formatoAmpliadoAux (cdr paresOrdenados) (append listaAcumuladora (escribir (car (car paresOrdenados)) (cdr (car paresOrdenados))))))
    )
)
