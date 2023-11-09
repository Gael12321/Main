#lang racket/base

(require racket/contract
         racket/match)

(struct expval () #:transparent)

(struct num-val expval (x) #:transparent)
(struct bool-val expval (x) #:transparent)
(struct pair-val expval (x y) #:transparent)
(struct emptylist-val expval () #:transparent)

(define (expval->num x)
  (match x
    [(num-val x) x]
    [(bool-val x)
     (error (format "Expected integer but got boolean ~a" x))]
    [(pair-val x y)
     (error (format "Expected integer but got pair ~a" x))]))

(define (expval->bool x)
  (match x
    [(bool-val x) x]
    [(num-val x)
     (error (format "Expected boolean but got integer ~a" x))]
    [(pair-val x y)
     (error (format "Expected boolean but got pair ~a" x))]))

(define (expval->pair x y)
  (match x
    [(pair-val x y) x]
    [(num-val x)
     (error (format "Expected pair but got integer ~a" x))]
    [(bool-val x)
     (error (format "Expected pair but got bool ~a" x))])
  (match y
    [(pair-val x y) y]
    [(num-val y)
     (error (format "Expected pair but got integer ~a" y))]
    [(bool-val y)
     (error (format "Expected pair but got bool ~a" y))]))





(provide
 expval?
 (contract-out
  [expval->num (-> expval? integer?)]
  [expval->bool (-> expval? boolean?)]
  [expval->pair (-> expval? pair?) ]
  [struct emptylist-val ()]
  [struct num-val ((x integer?))]
  [struct bool-val ((x boolean?))]
  [struct pair-val ((x pair?) (y pair?))]))


