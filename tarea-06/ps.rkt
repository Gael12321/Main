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


;Problema 2.1
(define (take l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     '()]
    [else
     (cons (first l) (take (rest l) (sub1 n)))]))

;Problema 2.2
(define (drop l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     l]
    [else
     (drop (rest l)(sub1 n))]))

;Problema 3
(define (bundle s n)
  (cond
    [(null? s) null]
    [(equal? n 0) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

;Problema 5
(define (list->chunks l n)
  (cond
    [(null? l) null]
    [(equal? n 0) null]
    [else
     (cons (take l n)
           (list->chunks (drop l n) n))]))

(define (bundle-chunk s n)
  (map implode(list->chunks s n))) 

;Problema 6
(define (partition s n)
  (cond
    [(null? s) null]
    [(zero? n) null]
    [else
     (if (<= (string-length s) n)
         (list s)
         (cons (substring s 0 n)
               (partition (substring s n) n)))]))

;Problema 7
(define (isort ls)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls)))))
;
;(define (insert n ls)
;  (cond
;    [(empty? ls) (list n)]
;    [(>= n (first ls)) (cons n ls)]
;    [else (cons (first ls)(insert n (rest ls)))]))

(define (insert n ls ord)
  (cond
    [(empty? ls) (list n)]
    [(equal? ord "asc")
     (if (< n (first ls))
         (cons n ls)
         (cons (first ls) (insert n (rest ls) ord)))]
    [(equal? ord "desc")
     (if (> n (first ls))
         (cons n ls)
         (cons (first ls) (insert n (rest ls) ord)))]))




;Problema 9.1
(define (smallers ls pivot)
  (cond
    [(empty? ls) null]
    [(< (first ls) pivot)
     (cons (first ls) (smallers (rest ls) pivot))]
    [else
     (smallers (rest ls) pivot)]))

;Problema 9.2
(define (largers ls pivot)
  (cond
    [(empty? ls) null]
    [(> (first ls) pivot)
     (cons (first ls) (largers(rest ls) pivot))]
    [else
     (largers (rest ls) pivot)]))


;Problema 10.2
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))
     (append (quicksort (smallers ls pivot))
             pivot-igual
             (quicksort (largers ls pivot)))]))

;Problema 11 ;Agrega el elemento falta shortearlo asi de chill 
(define (quicksort-add n ls ord)
  (cond
    [(empty? n) null]
    [(empty? ls) null]
    [(equal? ord "asc")
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))
     (append (append (quicksort-add n (smallers ls pivot) ord)            
                     pivot-igual
                     (quicksort-add n (largers ls pivot) ord)))]
    [(equal? ord "desc")
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))     
     (append (quicksort-add n (largers ls pivot) ord)             
             pivot-igual
             (quicksort-add n (smallers ls pivot) ord))]))

(define (add-quicksort n ls ord) ;El poderoso recursion hace que agregue el valor de n cada vez que se llama asi que gg toco agregar algo mas  
  (define(new-list n ls)
    (if (empty? ls) (list n)
        (append (list n) ls)))
  (quicksort-add n (new-list n ls) ord))


;Problema 12

(define (quicksort-isort n ls ord)
  (define umbral 15)
  (if (<= (length ls) umbral)
      (isort n ls ord)
      (add-quicksort n ls ord)))


;Problema 13

;Problema 14








(provide (all-defined-out))
