#lang racket

(define pi 3.1416)

(define (Area-Circulo radio)
  (* pi (* radio radio)))

(define (Propiedades-circulo Radio)
  (define Area (* pi (* Radio Radio)))
  (define Circunferencia (* 2 pi Radio))
  (list Area Circunferencia))

(define (Area-Perimetro-Rectangulo Largo Ancho)
  (define Area (* Largo Ancho))
  (define Perimetro (+ (* 2 Largo) (* 2 Ancho)))
  (list Area Perimetro))

(define (find-needle lst)
  (cond
    ((equal? (car lst) 'needle) 1) 
    ((equal? (cadr lst) 'needle) 1) 
    ((equal? (caddr lst) 'needle) 1) 
    (else -1))) 

(define (absolut x)
  (if (< x 0) (- x) x))

(define lista-original '(1 2 3))

(define (incrementar-uno Numero)
  (+ Numero 1))

(define lista-incrementada (map incrementar-uno lista-original))

(define (Even? numero)
 (= (remainder numero 2) 0))

(define lista-numeros '(1 2 3 4 5 6))

(define Lista-Even (map Even? lista-numeros))

(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m] 
      [else (add1 (another-add (sub1 n) m))])))


pi
(Area-Circulo 5)
(Propiedades-circulo 5)
(Area-Perimetro-Rectangulo 2 4 )
(find-needle '(hay hay needle))
(absolut -5)
(display lista-incrementada)
(newline)
(display Lista-Even)
another-add
(another-add 5 10)