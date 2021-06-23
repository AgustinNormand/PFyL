#lang racket

(define (sustituida1 elB elR lista)
    (sustituida1Aux elB elR lista '())
)

(define (sustituida1Aux elB elR lista listaAcumuladora)
    (cond
        (
            ;Si el elemento de la cabeza, es el que quiero reemplazar
            (equal? (car lista) elB) 
            ;Agrego a la lista acumuladora, el elemento reemplazo
            ;Y después, a esa lista, le agrego la cola de la lista que faltaba procesar
            (append 
                (append listaAcumuladora (list elR)) 
                (cdr lista)
            )
        )
        (
            ;Si el elemento de la cabeza no es el que quiero reemplazar
            else 
            ;Lo agrego a la lista acumuladora y vuelvo a llamar a la funcion
            (sustituida1Aux elB elR 
                (cdr lista) 
                (append listaAcumuladora (list (car lista)))
            )
        )
    )
)
