#lang racket

(define (elim listaDeElementosAEliminar listaOriginal)
    (eliminaMx listaOriginal listaDeElementosAEliminar)
)

(define (hayQueEliminar elemento listaDeElementosAEliminar)
    (cond
        ((null? listaDeElementosAEliminar) #f)
        ((equal? (car listaDeElementosAEliminar) elemento) #t)
        (else (hayQueEliminar elemento (cdr listaDeElementosAEliminar)))
    )
)

(define (eliminaMx lista listaDeElementosAEliminar)
    (cond
        ((null? lista) '())
        ((list? (car lista)) 
            (cons ;Esta linea fue la que mas me costó darme cuenta
                (eliminaMx (car lista) listaDeElementosAEliminar)
                (eliminaMx (cdr lista) listaDeElementosAEliminar)
            )
        )
        ((hayQueEliminar (car lista) listaDeElementosAEliminar)
            (eliminaMx (cdr lista) listaDeElementosAEliminar)
        )
        (else 
            (append 
                (list (car lista)) 
                (eliminaMx (cdr lista) listaDeElementosAEliminar)
            )
        )
    )
)