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
;;;take : integer? (listof a) -> (listof a)
;;; Devuelve los primeros n elementos de la lista recibida
;;; Se recorre la lista agregando cada elemento hasta que se llegue a n o hasta que acabe
(define (take l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     '()]
    [else
     (cons (first l) (take (rest l) (sub1 n)))]))

;Problema 2.2
;;; drop : (listof a?) integer? -> (listof a?)
;;; Toma una lista y un número entero, y devuelve una nueva lista que contiene todos los elementos de la lista original excepto los primeros n elementos.
;;; Se recorre la lista eliminando cada elemento  hasta que se llegue a n o hasta que acabe

(define (drop l n)
  (cond 
    [(or (null? l) (equal? n 0 ))
     l]
    [else
     (drop (rest l)(sub1 n))]))

;Problema 3
;;; bundle : (listof string?) integer? -> (listof string?)
;;; Toma una lista de caracteres y un número entero, y devuelve una nueva lista que contiene subcadenas de longitud n de la lista original.
;;; Utiliza las funciones `take` y `drop` para crear las sub-listas.

(define (bundle s n)
  (cond
    [(null? s) null]
    [(equal? n 0) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

;Problema 5
;;; list->chunks : (listof string?) integer? -> (listof (listof string??))
;;; Toma una lista y un número entero, y devuelve una nueva lista que contiene sublistas de longitud n de la lista original.
;;; Recorre la lista de cadenas, en cada iteraccion agarra la primera n y evalua el resto
(define (list->chunks l n)
  (cond
    [(null? l) null]
    [(equal? n 0) null]
    [else
     (cons (take l n)
           (list->chunks (drop l n) n))]))

;;; bundle-chunk : (listof string?) integer? -> (listof string?)
;;; Toma una lista de cadenas y un número entero, y devuelve una nueva lista que contiene subcadenas de longitud n de las cadenas originales.
;;; Funciona iterativamente, aplicando la función implode a cada sublista de la lista original.
(define (bundle-chunk s n)
  (map implode(list->chunks s n))) 

;Problema 6
;;; partition : string? integer? -> (listof string?)
;;; Toma una cadena y un número entero, y devuelve una lista que contiene subcadenas de longitud n de la cadena original.
;;; Funciona recursivamente, dividiendo la cadena en subcadenas de longitud n y luego concatenando las subcadenas resultantes en una lista.
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

;; isort : integer? (listof integer?) string? -> (listof integer?)
;;; Toma una lista de números enteros y un orden de clasificación, y devuelve una nueva lista que contiene los números enteros de la lista original ordenados según el orden dado.
;;; Funciona recursivamente, comparando los elementos de la lista y luego intercambiando los elementos si están en el orden incorrecto.
(define (isort n ls ord)
  (cond [(empty? ls) (list n)]
        [(eq? ord "asc")
         (if (< n (first ls))
             (cons n ls)
             (cons (first ls) (isort n (rest ls) ord)))]
        [(eq? ord "desc")
         (if (> n (first ls))
             (cons n ls)
             (cons (first ls) (isort n (rest ls) ord)))]))

;Problema 9.1
;;; smallers : (listof a?) a? -> (listof a?)
;;; Toma una lista y un elemento, y devuelve una nueva lista que contiene todos los elementos de la lista original que son menores que el elemento dado.
;;; Funciona recursivamente, comparando los elementos de la lista con el elemento dado y luego agregando los elementos a la nueva lista si son menores que el elemento dado.

(define (smallers ls pivot)
  (cond
    [(empty? ls) null]
    [(< (first ls) pivot)
     (cons (first ls) (smallers (rest ls) pivot))]
    [else
     (smallers (rest ls) pivot)]))

;Problema 9.2
;;; largers : (listof a?) a? -> (listof a?)
;;; Toma una lista y un elemento, y devuelve una nueva lista que contiene todos los elementos de la lista original que son mayores que el elemento dado.
;;; Funciona recursivamente, comparando los elementos de la lista con el elemento dado y luego agregando los elementos a la nueva lista si son mayores que el elemento dado.
(define (largers ls pivot)
  (cond
    [(empty? ls) null]
    [(> (first ls) pivot)
     (cons (first ls) (largers(rest ls) pivot))]
    [else
     (largers (rest ls) pivot)]))


;Problema 10.2
;;; quicksort : (listof a?) -> (listof a?)
;;; Toma una lista y devuelve una nueva lista que contiene los elementos de la lista original ordenados de menor a mayor.
;;; Funciona recursivamente, dividiendo la lista en dos sublistas, una con los elementos menores o iguales al pivote y otra con los elementos mayores o iguales al pivote. A continuación, ordena cada sublista recursivamente y luego concatena las dos sublistas ordenadas.
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))
     (append (quicksort (smallers ls pivot))
             pivot-igual
             (quicksort (largers ls pivot)))]))

;Problema 11 
;; quicksort-add : integer? (listof integer?) string? -> (listof integer?)
;;; Toma un número entero, una lista y un orden de clasificación, y devuelve una nueva lista que contiene los elementos de la lista original ordenados según el orden dado, con el número entero dado agregado al principio de la lista.
;;; Funciona recursivamente, comparando los elementos de la lista y luego intercambiando los elementos si están en el orden incorrecto.

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


;;; add-quicksort : integer? (listof integer?) string? -> (listof integer?)
;;; Toma un número entero, una lista de números enteros y manda a llamar a la funcion quicksort-add para con la nueva lista con el numero n agregado
;;; Checa si la lista esta vacia y si lo esta regresa el valor n agregado, si no esta vacia agrega el valor agregado a la lista y manda a llamar a quicksort-add con la nueva lista generada

(define (add-quicksort n ls ord) 
  (define(new-list n ls)
    (if (empty? ls) (list n)
        (append (list n) ls)))
  (quicksort-add n (new-list n ls) ord))


;Problema 12
;;; quicksort-isort : integer? (listof a?) string? -> (listof a?)
;;; Toma un número entero, una lista y un orden de clasificación, y devuelve una nueva lista que contiene los elementos de la lista original ordenados según el orden dado.
;;; Utiliza el algoritmo de ordenación por inserción si la lista es lo suficientemente pequeña, y el algoritmo de ordenación rápida en caso contrario.
(define (quicksort-isort n ls ord)
  (define umbral 10)
  (if (<= (length ls) umbral)
      (isort n ls ord)
      (add-quicksort n ls ord)))


;Problema 13
;;; smallers-filter: (listof a?)  integer? -> (listof a?)
;;;Toma una lista y un número entero, y devuelve una nueva lista que contiene todos los elementos de la lista  que son menores que el número entero dado.
;;; Utiliza la función filter para filtrar la lista original y devolver solo los elementos que son menores que el número entero dado.
(define (smallers-filter ls n)
  (filter (lambda (x) (< x n)) ls))

;;; largers-filter: (listof a?)  integer? -> (listof a?)
;;;Toma una lista y un número entero, y devuelve una nueva lista que contiene todos los elementos de la lista  que son mayores que el número entero dado.
;;; Utiliza la función filter para filtrar la lista original y devolver solo los elementos que son mayores que el número entero dado.
(define (largers-filter ls n)
  (filter (lambda (x) (> x n)) ls))


;Problema 14
;;;quicksort-add-filter:  integer? (listof a?) string? -> (listof a?)
;;;Toma un número entero, una lista y un orden de clasificación, y devuelve una nueva lista que contiene los elementos de la lista  ordenados según el orden dado, con el número entero dado agregado al principio de la lista.
;;;Utiliza el algoritmo de ordenación rápida para ordenar la lista original. Si el orden de clasificación es "asc", agrega el número entero dado al principio de la lista ordenada. Si el orden de clasificación es "desc", agrega el número entero dado al final de la lista ordenada.
(define (quicksort-add-filter n ls ord)
  (cond
    [(empty? n) null]
    [(empty? ls) null]
    [(equal? ord "asc")
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))
     (append (append (quicksort-add-filter n (filter (lambda (x) (< x pivot)) ls) ord)
                     pivot-igual
                     (quicksort-add-filter n (filter (lambda (x) (> x pivot)) ls) ord)))]
    [(equal? ord "desc")
     (define pivot (first ls))
     (define pivot-igual (filter (lambda (x) (equal? x pivot)) ls))     
     (append (quicksort-add-filter n (filter (lambda (x) (> x pivot)) ls) ord)         
             pivot-igual
             (quicksort-add-filter n (filter (lambda (x) (< x pivot)) ls) ord))]))


;; add-quicksort-filter: integer? (listof a?) string? -> (listof a?)
;; Toma un número entero, una lista y un orden de clasificación, y devuelve una nueva lista que contiene todos los elementos de la lista y n agregado
;; Funciona llamando a la función quicksort-add-filter con el número entero dado, la lista nueva y el orden de clasificación dado.
(define (add-quicksort-filter n ls ord)
  (define(new-list n ls)
    (if (empty? ls) (list n)
        (append (list n) ls)))
  (quicksort-add-filter n (new-list n ls) ord))

;Problema 16



;Problema 17

;(define (smallers l n)
;  (cond
;    [(empty? l) '()]
;    [else (if (<= (first l) n)
;              (cons (first l) (smallers (rest l) n))
;              (smallers (rest l) n))]))
;En el caso de que el primer elemento first l no es menor o igual a n, la llamada recursiva pasa al siguiente elemento de la lista rest l,
;asi que puede llegar a haber no-terminacion si no existen elementos iguales o menores a n en el inico de la lista

(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= i 1) 1]
          [(= (remainder n i) (remainder m i) 0) i]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

(provide (all-defined-out))
