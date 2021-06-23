#lang racket

(define (indexOf elemento lista)
    (indexOfAux elemento lista 0)
)

(define (indexOfAux elemento lista indiceActual)
    (cond
        ((null? lista) -1)
        ((equal? (car lista) elemento) indiceActual)
        (else (indexOfAux elemento (cdr lista) (+ indiceActual 1)))
    )
)

(define (nPrimerosRecursividadCola cantidad lista)
        (nPrimerosAux cantidad lista 0 '()))

(define (nPrimerosAux cantidad lista cantidadActual listaAcumuladora)
        (cond ((= cantidad cantidadActual) listaAcumuladora)
              ((null? lista) listaAcumuladora)
              (else
               (nPrimerosAux cantidad
                             (cdr lista)
                             (+ cantidadActual 1)
                             (append listaAcumuladora (list (car lista)))))))
                            #|Esto está medio confuso, lo intuitivo era (list listaAcumuladora (car lista))
                              pero eso devolvía parentesis de más y quedaba resprolijo |#

(define (anteriores elemento lista)
    (nPrimerosRecursividadCola (indexOf elemento lista) lista)
)
