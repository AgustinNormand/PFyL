#lang racket

(define identificadorDerecha 'derecha)
(define identificadorIzquierda 'izquierda)
(define identificadorArriba 'arriba)
(define identificadorAbajo 'abajo)

(define (llegada coordenadas pasos)
    (cond
        ((null? pasos) coordenadas)
        (else (llegada (calcularCoordenadas coordenadas (car pasos)) (cdr pasos)))
    )
)

(define (sumarX coordenadas valor)
    (cons
        (+ (car coordenadas) valor)
        (cdr coordenadas)
    )
)

(define (sumarY coordenadas valor)
    (cons
        (car coordenadas)
        (+ (cdr coordenadas) valor)
    )
)

(define (calcularCoordenadas coordenadas paso)
    (cond
        ((equal? paso identificadorDerecha) 
            (sumarX coordenadas 1)
        )
        ((equal? paso identificadorIzquierda) 
            (sumarX coordenadas -1)
        )
        ((equal? paso identificadorArriba) 
            (sumarY coordenadas 1)
        )
        ((equal? paso identificadorAbajo) 
            (sumarY coordenadas -1)
        )
    )
)