#lang racket

(define (eliminaMx elementoAEliminar lista)
    (cond
        ((null? lista) '())
        ((list? (car lista)) 
            (cons ;Esta linea fue la que mas me costó darme cuenta
                (eliminaMx elementoAEliminar (car lista))
                (eliminaMx elementoAEliminar (cdr lista))
            )
        )
        ((equal? (car lista) elementoAEliminar) 
            (eliminaMx elementoAEliminar (cdr lista))
        )
        (else 
            (append 
                (list (car lista)) 
                (eliminaMx elementoAEliminar (cdr lista))
            )
        )
    )
)