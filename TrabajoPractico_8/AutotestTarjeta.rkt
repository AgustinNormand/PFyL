#lang racket

(define (contar lista)
    (contarAux lista 0)
)

(define (contarAux lista contadorAcumulado)
    (cond
        ((null? lista) contadorAcumulado)
        (else (contarAux (cdr lista) (+ contadorAcumulado 1)))
    )
)

(define (validarTarjeta lista)
    (and
        (= (contar lista) 16)
        (= (modulo (sumar (duplicar lista)) 10) 0)
    )
)

(define (duplicar lista)
    (duplicarAux lista 'duplicar '())
)

(define (duplicarAux lista accion listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        ((equal? accion 'duplicar) (duplicarAux (cdr lista) 'noDuplicar (append listaAcumuladora (list (* (car lista) 2)))))
        (else (duplicarAux (cdr lista) 'duplicar (append listaAcumuladora (list (car lista)))))
    )
)

(define (sumar lista)
    (sumarAux lista 0)
)

(define (sumarAux lista acumulador)
    (cond
        ((null? lista) acumulador)
        (else (sumarAux (cdr lista) (+ acumulador (sumarDigitos (car lista)))))
    )
)

(define (sumarDigitos elemento)
    (sumarDigitosAux elemento 0)
)

(define (sumarDigitosAux elemento acumulador)
    (cond
        ((= elemento 0) acumulador)
        (else (sumarDigitosAux (quotient elemento 10) (+ acumulador (modulo elemento 10))))
    )
)