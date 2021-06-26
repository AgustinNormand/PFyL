#lang racket

(define (ocurrencias lista elemento)
    (ocurrenciasAux lista elemento 0)
)

(define (ocurrenciasAux lista elemento cantidadActual)
    (cond
        ((null? lista) cantidadActual)
        ((equal? (car lista) elemento) (ocurrenciasAux (cdr lista) elemento (+ cantidadActual 1)))
        (else (ocurrenciasAux (cdr lista) elemento cantidadActual))
    )
)

(define (generala? lista)
    (= (ocurrencias lista (car lista)) 5)
)

(define (poker? lista)
    (or
        (= (ocurrencias lista (car lista)) 4)
        (= (ocurrencias lista (car (cdr lista))) 4)
    )
)

(define (full? lista)
    (or
        (and
            (= (ocurrencias lista (car lista)) 3)
            (= (ocurrencias lista (cadddr lista)) 2)
        )
        (and
            (= (ocurrencias lista (car lista)) 2)
            (= (ocurrencias lista (caddr lista)) 3)
        )
    )
)

(define (escalera? lista)
    (or
        (escaleraAux lista 1)
        (escaleraAux lista 2)
    )
)

(define (escaleraAux lista siguienteValor)
    (cond
        ((null? lista) #t)
        ((equal? (car lista) siguienteValor) (escaleraAux (cdr lista) (+ siguienteValor 1)))
        (else #f)
    )
)

(define (mayor lista)
    (mayorAux (cdr lista) (car lista))
)

(define (mayorAux lista mayor)
    (cond
        ((null? lista) mayor)
        ((> (car lista) mayor) (mayorAux (cdr lista) (car lista)))
        (else (mayorAux (cdr lista) mayor))
    )
)

(define (puntajeElse lista)
    (puntajeElseAux lista (mayor lista))
)

(define (puntajeElseAux lista mayorPuntaje)
    (cond
        ((null? lista) mayorPuntaje)
        ((> (* (ocurrencias lista (car lista)) (car lista)) mayorPuntaje)
            (puntajeElseAux (cdr lista) (* (ocurrencias lista (car lista)) (car lista)))
        )
        (else (puntajeElseAux (cdr lista) mayorPuntaje))
    )
)

(define (puntaje lista)
    (cond
        ((generala? lista) 60)
        ((poker? lista) 40)
        ((full? lista) 30)
        ((escalera? lista) 20)
        (else (puntajeElse lista))
    )
)

(define (generalaPFyL parJugador1 parJugador2)
    (cond
        ((> (puntaje (cdr parJugador1)) (puntaje (cdr parJugador2))) (car parJugador1))
        ((> (puntaje (cdr parJugador2)) (puntaje (cdr parJugador1))) (car parJugador2))
        (else 'empate)
    )
)
