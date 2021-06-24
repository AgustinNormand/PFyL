#lang racket

(define (sustituida elB elR lista)
    (sustituidaAux elB elR lista '())
)

(define (sustituidaAux elB elR lista listaAcumuladora)
    (cond
        ((null? lista) listaAcumuladora)
        (
            ;Si el elemento de la cabeza, es el que quiero reemplazar
            (equal? (car lista) elB) 
            ;Agrego a la lista acumuladora, el elemento reemplazo
            ;Y después, a esa lista, le agrego la cola de la lista que faltaba procesar
            (sustituidaAux
                elB
                elR
                (cdr lista)
                (append listaAcumuladora (list elR))
            )
        )
        (
            ;Si el elemento de la cabeza no es el que quiero reemplazar
            else 
            ;Lo agrego a la lista acumuladora y vuelvo a llamar a la funcion
            (sustituidaAux elB elR 
                (cdr lista) 
                (append listaAcumuladora (list (car lista)))
            )
        )
    )
)
