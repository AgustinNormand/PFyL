#lang racket

; TO-FIX

(define (sublistas lista)
    (subListasAux lista '())
)

(define (subListasAux lista listaAcumuladora)
    (cond
        ((null? lista) '())
        ((list? (car lista)) 
            (append 
                (subListasAux 
                    (car lista) 
                    listaAcumuladora
                )
                (subListasAux 
                    (cdr lista) 
                    listaAcumuladora
                )
            )
        )
        (else 
            (cons 
                (append 
                    listaAcumuladora
                    (list (car lista))
                )
                (append
                    (subListasAux (cdr lista) (append listaAcumuladora (list (car lista))))
                    (subListasAux (cdr lista) listaAcumuladora)
                )
            )
        )
    )
)