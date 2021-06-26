#lang racket

(define (trim listaDeCaracteres)
    (eliminarFinal (eliminarPrincipio listaDeCaracteres))
)

(define (eliminarPrincipio listaCaracteres)
    (cond
        ((equal? (car listaCaracteres) '_) (eliminarPrincipio (cdr listaCaracteres)))
        (else listaCaracteres)
    )
)

(define (eliminarFinal listaCaracteres)
    (eliminarFinalAux listaCaracteres '())
)

(define (eliminarFinalAux listaCaracteres listaAcumuladora)
    (cond
        ((null? listaCaracteres) listaAcumuladora)
        ((equal? (car listaCaracteres) '_)
            (cond
                ((equal? (cadr listaCaracteres) '_) listaAcumuladora)
                (else (eliminarFinalAux (cdr listaCaracteres) (append listaAcumuladora (list (car listaCaracteres)))))
            )
        )
        (else (eliminarFinalAux (cdr listaCaracteres) (append listaAcumuladora (list (car listaCaracteres)))))
    )
)