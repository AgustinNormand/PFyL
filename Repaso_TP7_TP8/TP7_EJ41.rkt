#lang racket

(define (comision monto1 monto2)
    (cond
        ((> monto1 monto2) (* monto1 0.4))
        (else (* monto1 0.3))
    )
)

(define (comisiones monto1 monto2)
    (list
        (cons 'Vendedor1 (comision monto1 monto2))
        (cons 'Vendedor2 (comision monto2 monto1))
    )
)