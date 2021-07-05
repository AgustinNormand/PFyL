#lang racket

(define (cuadratica A B C X)
    (+ (* A (expt X 2)) (* B X) C)
)

(define (puntosCuadratica A B C X0 X1)
    (puntosCuadraticaAux A B C X0 X1 '())
)

; Recursividad de cola
(define (puntosCuadraticaAux A B C X0 X1 listaAcumuladora)
    (cond
        ((> X0 X1) listaAcumuladora)
        (else 
            (puntosCuadraticaAux 
                A 
                B 
                C 
                (+ X0 1) 
                X1 
                (append 
                    listaAcumuladora 
                    (list (cons X0 (cuadratica A B C X0)))
                )
            )
        )
    )
)

; Recursividad de pila
(define (puntosCuadraticaPila A B C X0 X1)
    (cond
        ((> X0 X1) '())
        (else 
            (append 
                (list (cons X0 (cuadratica A B C X0)))
                (puntosCuadraticaPila A B C (+ X0 1) X1)
            )
                
        )
    )
)