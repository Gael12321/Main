#lang racket



(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (car x))
           (= (string-length (car x)) 1)
           (unit-string-list? (cdr x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))


(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e"
           ls))
  (apply string-append ls))

(define (take l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     '()]
    [else
     (cons (first l) (take (rest l) (sub1 n)))]))


(define (drop l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     l]
    [else
     (drop (rest l)(sub1 n))]))


(define (bundle s n)
  (cond
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))




(provide (all-defined-out))