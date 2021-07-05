#lang racket

(define (multiplo7? numero)
    (cond
        ((or (= numero 0) (= numero -7) (= numero 7)) #t)
        ((< numero 0) #f)
        (else (multiplo7? (- (quotient numero 10) (* (modulo numero 10) 2))))   
    )
)