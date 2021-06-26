#lang racket

(define (cantPalabras listaCaracteres numero)
    (cantPalabrasAux listaCaracteres numero 0 0)
)

(define (cantPalabrasAux listaCaracteres numero acumulador cantidadEncontrada)
    (cond
        ((null? listaCaracteres) 
            (cond
                ((= acumulador numero) (+ cantidadEncontrada 1))
                (else cantidadEncontrada)
            )
        )
        ((equal? (car listaCaracteres) '_)
            (cond
                ((= numero acumulador) 
                    (cantPalabrasAux (cdr listaCaracteres) numero 0 (+ cantidadEncontrada 1))
                )
                (else (cantPalabrasAux (cdr listaCaracteres) numero 0 cantidadEncontrada))
            )
        )
        (else
            (cantPalabrasAux (cdr listaCaracteres) numero (+ acumulador 1) cantidadEncontrada)
        )
    )
)