#lang racket

(define pi 3.1416)

(define (area-circle radio)
  (* pi (* radio radio)))

(define (circle-properties Radio)
  (define Area (* pi (* Radio Radio)))
  (define Circunferencia (* 2 pi Radio))
  (list Area Circunferencia))

(define (rectangle-properties Largo Ancho)
  (define Area (* Largo Ancho))
  (define Perimetro (+ (* 2 Largo) (* 2 Ancho)))
  (list Area Perimetro))

(define (find-needle lst)
  (cond
    ((equal? (car lst) 'needle) 1) 
    ((equal? (cadr lst) 'needle) 1) 
    ((equal? (caddr lst) 'needle) 1) 
    (else -1))) 

(define (abs x)
  (if (< x 0) (- x) x))

(define (addone Numero)
  (+ Numero 1))

(define (Even? numero)
 (= (remainder numero 2) 0))

(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m] 
      [else (add1 (another-add (sub1 n) m))])))


pi
(area-circle 5)
(circle-properties 5)
(rectangle-properties 2 4 )
(find-needle '(hay hay needle))
(abs -5)
(map addone '(1 2 3))
(map Even? '(1 2 3 4 5 6))
(another-add 5 10)
