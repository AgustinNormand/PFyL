#lang racket

(define izquierdaDedo2 '(R T F G V B))
(define izquierdaDedo3 '(E D C))
(define izquierdaDedo4 '(W S X))
(define izquierdaDedo5 '(Q A Z))

(define derechaDedo2 '(Y U H J N M))
(define derechaDedo3 '(I K))
(define derechaDedo4 '(O L))
(define derechaDedo5 '(P))

(define (existe letra listaDedo)
    (cond
        ((null? listaDedo) #f)
        ((equal? letra (car listaDedo)) #t)
        (else (existe letra (cdr listaDedo)))
    )
)

(define (determinarDedo letra)
    (cond
        ((existe letra izquierdaDedo2) (list (cons 'I 2)))
        ((existe letra izquierdaDedo3) (list (cons 'I 3)))
        ((existe letra izquierdaDedo4) (list (cons 'I 4)))
        ((existe letra izquierdaDedo5) (list (cons 'I 5)))

        ((existe letra derechaDedo2) (list (cons 'D 2)))
        ((existe letra derechaDedo3) (list (cons 'D 3)))
        ((existe letra derechaDedo4) (list (cons 'D 4)))
        ((existe letra derechaDedo5) (list (cons 'D 5)))

        (else (list (cons '? '?)))
    )
)

; Recursividad de cola
(define (dedos palabra)
    (dedosAux palabra '())
)

(define (dedosAux palabra listaAcumuladora)
    (cond
        ((null? palabra) listaAcumuladora)
        (else (dedosAux (cdr palabra) (append listaAcumuladora (determinarDedo (car palabra)))))
    )
)



; Recursividad de pila
(define (dedosPila palabra)
    (cond
        ((null? palabra) '())
        (else 
            (append 
                (determinarDedo (car palabra)) 
                (dedosPila (cdr palabra))
            )
        )
    )
)