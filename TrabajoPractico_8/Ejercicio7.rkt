#lang racket

; Esta es la version que me resultó mas intuitiva, pero las concatena mal
(define (concatenadas lista1 lista2)
    (cond
        ((null? lista1) lista2)
        (else (concatenadas (cdr lista1) (cons (car lista1) lista2)))
    )
)

; Version de Pablo
(define (concatenadasPablo lista1 lista2)
    (cond
        ((null? lista1) lista2)
        (else 
            (cons 
                (car lista1) 
                (concatenadasPablo (cdr lista1) lista2)
            )
        )
    )
)