#lang racket

(define (clasifica lista)
    (list
        (cons 'helado (contarHelados lista))
        (cons 'frio (contarFrios lista))
        (cons 'templado (contarTemplados lista))
        (cons 'calido (contarCalidos lista))
    )
)

(define (contarHelados lista)
    (cond
        ((null? lista) 0)
        ((<= (car lista) 0) (+ 1 (contarHelados (cdr lista))))
        (else (contarHelados (cdr lista)))
    )
)

(define (contarFrios lista)
    (cond
        ((null? lista) 0)
        ((and (> (car lista) 0) (<= (car lista) 10)) (+ 1 (contarFrios (cdr lista))))
        (else (contarFrios (cdr lista)))
    )
)

(define (contarTemplados lista)
    (cond
        ((null? lista) 0)
        ((and (> (car lista) 10) (<= (car lista) 25)) (+ 1 (contarTemplados (cdr lista))))
        (else (contarTemplados (cdr lista)))
    )
)


(define (contarCalidos lista)
    (cond
        ((null? lista) 0)
        ((> (car lista) 25) (+ 1 (contarCalidos (cdr lista))))
        (else (contarCalidos (cdr lista)))
    )
)