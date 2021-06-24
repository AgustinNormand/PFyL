#lang racket

(define (bonus? elemento)
    (equal? elemento 'bonus)
)

(define (puntaje elemento)
    (cond
        ((equal? elemento 'puntito) 1)
        ((equal? elemento 'frutilla) 10)
        ((equal? elemento 'banana) 30)
        ((equal? elemento 'cerezas) 50)
        ((equal? elemento 'fantasma) 100)
    )
)

(define duracionBonus 5)

(define (puntajePacman lista)
    (puntajePacmanAux lista 0 0)
)

(define (puntajePacmanAux lista bonusRestante puntajeAcumulado)
    (cond
        ((null? lista) puntajeAcumulado)
        ((bonus? (car lista)) (puntajePacmanAux (cdr lista) duracionBonus puntajeAcumulado))
        ((> bonusRestante 0) 
            (puntajePacmanAux 
                (cdr lista) 
                (- bonusRestante 1) 
                (+ puntajeAcumulado (* (puntaje (car lista)) 2))
            )
        )
        (else
            (puntajePacmanAux
                (cdr lista)
                bonusRestante
                (+ puntajeAcumulado (puntaje (car lista)))
            )
        )
    )
)