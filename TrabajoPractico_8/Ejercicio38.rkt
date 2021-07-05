#lang racket

(define (antecesores persona arbolGenialogico)
    (antecesoresAux persona arbolGenialogico '())
)

(define (antecesoresAux persona arbolGenialogico listaAcumuladora)
    (cond
        ((null? arbolGenialogico) listaAcumuladora)
        ((list? (car arbolGenialogico)) 
            (cond
                ((contains persona (car arbolGenialogico))
                    (antecesoresAux 
                        persona 
                        (cdar arbolGenialogico) 
                        (append 
                            listaAcumuladora 
                            (list (car (car arbolGenialogico)))
                        )
                    )
                )
                (else
                    (antecesoresAux 
                        persona 
                        (cdr arbolGenialogico) 
                        listaAcumuladora 
                    )
                )
            )
        )
        ((contains persona (cdr arbolGenialogico))
            (antecesoresAux persona (cdr arbolGenialogico) (append listaAcumuladora (list (car arbolGenialogico))))
        )
        (else (antecesoresAux persona (cdr arbolGenialogico) listaAcumuladora))
    )
)

(define (contains persona lista)
    (cond
        ((null? lista) #f)
        ((list? (car lista))
            (or
                (contains persona (car lista))
                (contains persona (cdr lista))
            )
        )
        ((equal? (car lista) persona) #t)
        (else (contains persona (cdr lista)))
    )
)