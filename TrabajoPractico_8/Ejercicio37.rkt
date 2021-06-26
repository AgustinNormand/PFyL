#lang racket

(define (nivelMasProfundo elemento lista)
    (nivelMasProfundoAux elemento lista 1 0)
)

(define (nivelMasProfundoAux elemento lista nivelActual nivelMaximo)
    (cond
        ((null? lista) nivelMaximo)
        ((list? (car lista)) 
            (cond
                (
                    (> 
                        (nivelMasProfundoAux elemento (car lista) (+ nivelActual 1) nivelMaximo)
                        nivelMaximo
                    )

                    (nivelMasProfundoAux elemento (cdr lista) nivelActual (nivelMasProfundoAux elemento (car lista) (+ nivelActual 1) nivelMaximo))
                )
                (else
                    (nivelMasProfundoAux elemento (cdr lista) nivelActual nivelMaximo)
                )
            )
        )
        ((equal? (car lista) elemento) 
            (cond
                ((> nivelActual nivelMaximo)
                    (nivelMasProfundoAux elemento (cdr lista) nivelActual nivelActual)
                )
                (else 
                    (nivelMasProfundoAux elemento (cdr lista) nivelActual nivelMaximo)
                )
            )
        )
        (else
            (nivelMasProfundoAux elemento (cdr lista) nivelActual nivelMaximo)
        )
    )
)