#lang racket

(define anioActual 2020)

(define valorMetroZonaA 100)
(define valorHabitacionesZonaA 5000)
(define valorGarageZonaA 15000)
(define valorCoeficienteZonaA 1)

(define valorMetroZonaB 110)
(define valorHabitacionesZonaB 5000)
(define valorGarageZonaB 18000)
(define valorCoeficienteZonaB 1.2)

(define (inmuebles listaInmuebles precioMaximo)
    (inmueblesAux listaInmuebles precioMaximo '())
)

(define (inmueblesAux listaInmuebles precioMaximo listaAcumuladora)
    (cond
        ((null? listaInmuebles) listaAcumuladora)
        ((<= (precio (car listaInmuebles)) precioMaximo)
            (inmueblesAux (cdr listaInmuebles) precioMaximo (append listaAcumuladora (codigoYprecio (car listaInmuebles))))
        )
        (else
            (inmueblesAux (cdr listaInmuebles) precioMaximo listaAcumuladora)
        )
    )
)

(define (precio listaInmueble)
    (precioAux
        (cadr listaInmueble)
        (caddr listaInmueble)
        (cadddr listaInmueble)
        (caddddr listaInmueble)
        (cadddddr listaInmueble)
    )
)

(define (precioAux anio metros habitaciones garage zona)
    (define (precioInmueble)
        (cond
            ((equal? zona 'a)
                (precioInmuebleGenerico valorMetroZonaA valorHabitacionesZonaA valorGarageZonaA valorCoeficienteZonaA)
            )
            ((equal? zona 'b)
                (precioInmuebleGenerico valorMetroZonaB valorHabitacionesZonaB valorGarageZonaB valorCoeficienteZonaB)
            )
        )
    )

    (define (precioInmuebleGenerico valorMetroZona valorHabitacionesZona 
                                    valorGarageZona valorCoeficienteZona)
        (* 
            (* 
                (+ 
                    (* metros valorMetroZona) 
                    (* habitaciones valorHabitacionesZona)
                    (* (toInt garage) valorGarageZona)
                ) 
                (- 
                    1 
                    (/ (antiguedad anio) 100)
                )
            ) 
            valorCoeficienteZona
        )
    )

    (precioInmueble)
)

(define (codigoYprecio listaInmueble)
    (list (cons (car listaInmueble) (precio listaInmueble)))
)

(define (antiguedad anio)
    (- anioActual anio)
)

(define (toInt boolean)
    (cond
        (boolean 1)
        (else 0)
    )
)

(define (caddddr lista)
    (car (cdr (cdr (cdr (cdr lista)))))
)

(define (cadddddr lista)
    (car (cdr (cdr (cdr (cdr (cdr lista))))))
)

