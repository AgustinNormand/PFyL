#lang racket

(define (contar lista mayoresQue menoresQue)
    (cond
        ((null? lista) 0)
        (
            (and
                (<= (car lista) menoresQue)
                (>= (car lista) mayoresQue)
            )
            (+ (contar (cdr lista) mayoresQue menoresQue) 1))
        (else (contar (cdr lista) mayoresQue menoresQue))
    )
)

(define (condiciones notas)
    (list
        (cons
            'libre
            (contar notas 0 3)
        )
        (cons
            'regular
            (contar notas 4 6)
        )
        (cons
            'promovido
            (contar notas 7 10)
        )
    )
)