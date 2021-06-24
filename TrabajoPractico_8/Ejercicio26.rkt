#lang racket

; Asumí que el enunciado estaba mal, proque era mas simple, en tal caso, despues lo resulevo de 
; la otra forma tambien

#|
¿L1 es sublista de L2? Multinivel. O sea que 
todos los elementos de L1 se encuentren en 
L2 juntos y en el mismo orden.

(subListaM '(3 5 7) '(1 2 3 (7 6 (3 5 7) (2 3)) 2)) => #t
|#

(define (subLista listaBuscada lista)
    (cond
        ((null? lista) #f)
        ((consecutivos listaBuscada lista) #t)
        (else (subLista listaBuscada (cdr lista)))
    )
)

(define (consecutivos listaBuscada lista)
    (cond
        ((and (null? lista) (null? listaBuscada)) #t)
        ((and (not (null? lista)) (null? listaBuscada)) #t)
        ((and (null? lista) (not (null? listaBuscada))) #f)
        ((equal? (car lista) (car listaBuscada)) 
            (consecutivos (cdr listaBuscada) (cdr lista))
        )
        (else #f)
    )
)

(define (subListaM listaBuscada lista)
    (cond
        ((null? lista) #f)
        ((list? (car lista)) 
            (or 
                ; O la sublista es (car lista)
                (subLista listaBuscada (car lista)) 

                ; O la sublista esta dentro de (car lista)
                (subListaM listaBuscada (car lista)) 

                ; O la sublista esta en (cdr lista)
                (subListaM listaBuscada (cdr lista))
            )
        )
        (else (subListaM listaBuscada (cdr lista)))
    )
)