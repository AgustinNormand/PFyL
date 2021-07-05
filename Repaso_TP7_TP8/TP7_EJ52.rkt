#lang racket

(define (aMm valor unidadOrigen)
    (cond
        ((equal? unidadOrigen 'mm) valor)
        ((equal? unidadOrigen 'cm) (* valor 10))
        ((equal? unidadOrigen 'dm) (* valor 100))
        ((equal? unidadOrigen 'm) (* valor 1000))
    )
)

(define (mmA valorOrigen unidadDestino)
    (cond
        ((equal? unidadDestino 'mm) valorOrigen)
        ((equal? unidadDestino 'cm) (/ valorOrigen 10))
        ((equal? unidadDestino 'dm) (/ valorOrigen 100))
        ((equal? unidadDestino 'm) (/ valorOrigen 1000))
    )
)

(define (convertir valorOrigen unidadOrigen unidadDestino)
    (mmA (aMm valorOrigen unidadOrigen) unidadDestino)
)