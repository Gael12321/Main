#lang racket


(define (countdown n)
  (cond
    ((zero? n) '())              
    (else (cons n (countdown (- n 1))))))

(define (insertL simbolo1 simbolo2 lista)
  (if (null? lista)
      '()
      (if (eqv? (car lista) simbolo1)
          (cons simbolo2 (cons (car lista) (insertL simbolo1 simbolo2 (cdr lista))))
          (cons (car lista) (insertL simbolo1 simbolo2 (cdr lista))))))

(define (remv-1st simbolo lista)
  (cond
    ((null? lista) '())               
    ((eqv? (car lista) simbolo) (cdr lista)) 
    (else (cons (car lista) (remv-1st simbolo (cdr lista))))))

(define (map p ls)
  (if (null? ls)
      '() ; 
      (cons (p (car ls)) (map p (cdr ls))))) 


(define (filter predicado lista)
  (cond
    ((null? lista) '()) 
    ((predicado (car lista)) (cons (car lista) (filter predicado (cdr lista)))) 
    (else (filter predicado (cdr lista)))))


(define (zip lista1 lista2)
  (if (or (null? lista1) (null? lista2))
      '() 
      (cons (list (car lista1) (car lista2)) (zip (cdr lista1) (cdr lista2)))))

(define (list-index-ofv elemento lista)
  (define (index-helper elemento lista indice)
    (cond
      ((null? lista) -1)
      ((equal? (car lista) elemento) indice)
      (else (index-helper elemento (cdr lista) (+ indice 1)))))
  (index-helper elemento lista 0))
 


(countdown 10)

(insertL 'x 'y '(x z z x y x))

(remv-1st 'a '(a b c d a d))

(map sub1 '(1 2 3 4))

(filter even? '(1 2 3 4 5 6))

 (zip '(1 2 3) '(a b c))
 (zip '(1 2 3 4 5 6) '(a b c))
 (zip '(1 2 3) '(a b c d e f))

(list-index-ofv 'x '(x y z x x))


