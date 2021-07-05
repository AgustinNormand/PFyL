#lang racket

(define (chicago lista)
    (chicagoAux lista 2 0)
)

(define (suma parOrdenado)
    (+ (car parOrdenado) (cdr parOrdenado))
)

(define (obtenerPuntaje puntuacionObjetivo dadosTirados)
    (cond
        ((= (suma dadosTirados) puntuacionObjetivo) puntuacionObjetivo)
        (else 0)
    )
)

(define (chicagoAux lista puntuacionObjetivo puntosActuales)
    (cond
        ((null? lista) puntosActuales)

        ;A partir de la ronda 11, los puntos no los considero válidos
        ((> puntuacionObjetivo 12) puntosActuales) 

        
        (else 
            (chicagoAux 
                (cdr lista) 
                (+ puntuacionObjetivo 1) 
                (+ puntosActuales (obtenerPuntaje puntuacionObjetivo (car lista)))
            )
        )
    )
)