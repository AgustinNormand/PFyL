#lang racket

(define (index elemento lista)
    (indexAux elemento lista 1)
)

(define (indexAux elemento lista indiceActual)
    (cond
        ((null? lista) 0)
        ((equal? (car lista) elemento) indiceActual)
        (else (indexAux elemento (cdr lista) (+ indiceActual 1)))
    )
)

(define (saltos palabra cinta)
    (saltosAux palabra cinta '() 0)
)

(define (saltosAux palabra cinta listaAcumuladora indiceAnterior)
    (cond
        ((null? palabra) listaAcumuladora)
        (else 
            (saltosAux 
                (cdr palabra) 
                cinta 
                (append listaAcumuladora 
                (list (- (index (car palabra) cinta) indiceAnterior)))
                (index (car palabra) cinta)
            )
        )
    )
)

(define (palabra saltos cinta)
    (palabraAux saltos cinta '() 0)
)

(define (palabraAux saltos cinta listaAcumuladora indiceAnterior)
    (cond
        ((null? saltos) listaAcumuladora)
        (else (palabraAux (cdr saltos) cinta (append listaAcumuladora (get (+ indiceAnterior (car saltos)) cinta)) (+ indiceAnterior (car saltos))))
    )
)

(define (get index lista)
    (getAux index lista 1)
)

(define (getAux index lista indiceActual)
    (cond
        ((null? lista) -1)
        ((= indiceActual index) (list (car lista)))
        (else (getAux index (cdr lista) (+ indiceActual 1)))
    )
)