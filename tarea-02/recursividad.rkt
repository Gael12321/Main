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

(define (append ls1 ls2)
  (if (null? ls1)        
      ls2
      (cons (car ls1)      
            (append (cdr ls1) ls2))))

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) 
              (list (car lst)))))

(define (repeat lst n)
  (if (= n 0)  
      '()
      (append lst (repeat lst (- n 1)))))

(define (same-lists* lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) #t)  
    ((or (null? lst1) (null? lst2)) #f)
    ((not (= (length lst1) (length lst2))) #f)
    (else (and (equal? (car lst1) (car lst2))   
               (same-lists* (cdr lst1) (cdr lst2))))))

(define (binary->natural binary-list)
  (define (binary->natural-helper lst exponent result)
    (cond
      ((null? lst) result)
      ((= (car lst) 0) (binary->natural-helper (cdr lst) (+ exponent 1) result))
      ((= (car lst) 1) (binary->natural-helper (cdr lst) (+ exponent 1) (+ result (* (expt 2 exponent) (car lst)))))))
  
  (binary->natural-helper binary-list 0 0))

(define (dividend-divisible? dividend divisor)
  (if (< dividend divisor)
      (= dividend 0)
      (dividend-divisible? (- dividend divisor) divisor)))

(define (div dividend divisor)
  (if (dividend-divisible? dividend divisor)
      1
      (+ 1 (div (- dividend divisor) divisor))))



 
;////////////////////////////////////////////////////////////// 


(countdown 10)

(insertL 'x 'y '(x z z x y x))

(remv-1st 'a '(a b c d a d))

(map sub1 '(1 2 3 4))

(filter even? '(1 2 3 4 5 6))

(zip '(1 2 3) '(a b c))
(zip '(1 2 3 4 5 6) '(a b c))
(zip '(1 2 3) '(a b c d e f))

(list-index-ofv 'x '(x y z x x))

(append '(42 120) '(1 2 3))

(reverse '(a 3 x))

(repeat '(4 8 11) 4)

(same-lists* '() '())

(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))

(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))

(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ()))))

 (binary->natural '())
 (binary->natural '(0 0 1))

 (binary->natural '(0 0 1 1))



