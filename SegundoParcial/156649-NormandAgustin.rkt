#lang racket

#| EJERCICIO 1|#

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

#| EJERCICIO 2|#

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

#| EJERCICIO 3|#


(define izquierdaDedo2 '(R T F G V B))
(define izquierdaDedo3 '(E D C))
(define izquierdaDedo4 '(W S X))
(define izquierdaDedo5 '(Q A Z))

(define derechaDedo2 '(Y U H J N M))
(define derechaDedo3 '(I K))
(define derechaDedo4 '(O L))
(define derechaDedo5 '(P))

(define (existe letra listaDedo)
    (cond
        ((null? listaDedo) #f)
        ((equal? letra (car listaDedo)) #t)
        (else (existe letra (cdr listaDedo)))
    )
)

(define (determinarDedo letra)
    (cond
        ((existe letra izquierdaDedo2) (list (cons 'I 2)))
        ((existe letra izquierdaDedo3) (list (cons 'I 3)))
        ((existe letra izquierdaDedo4) (list (cons 'I 4)))
        ((existe letra izquierdaDedo5) (list (cons 'I 5)))

        ((existe letra derechaDedo2) (list (cons 'D 2)))
        ((existe letra derechaDedo3) (list (cons 'D 3)))
        ((existe letra derechaDedo4) (list (cons 'D 4)))
        ((existe letra derechaDedo5) (list (cons 'D 5)))

        (else (list (cons '? '?)))
    )
)

; Recursividad de cola
(define (dedos palabra)
    (dedosAux palabra '())
)

(define (dedosAux palabra listaAcumuladora)
    (cond
        ((null? palabra) listaAcumuladora)
        (else (dedosAux (cdr palabra) (append listaAcumuladora (determinarDedo (car palabra)))))
    )
)



; Recursividad de pila
(define (dedosPila palabra)
    (cond
        ((null? palabra) '())
        (else 
            (append 
                (determinarDedo (car palabra)) 
                (dedosPila (cdr palabra))
            )
        )
    )
)

#| EJERCICIO 4|#


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