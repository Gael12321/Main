#lang racket/base

(require racket/contract
         racket/match)

(struct expval () #:transparent)

(struct num-val expval (x) #:transparent)
(struct bool-val expval (x) #:transparent)
(struct list-val expval (x) #:transparent)

(define (expval->num x)
  (match x
    [(num-val x) x]
    [(bool-val x)
     (error (format "Expected integer but got boolean ~a" x))]
    [(list-val x)
     (error (format "Expected integer but got list ~a" x))]))

(define (expval->bool x)
  (match x
    [(bool-val x) x]
    [(num-val x)
     (error (format "Expected boolean but got integer ~a" x))]
    [(list-val x)
     (error (format "Expected integer but got list ~a" x))]))

(define (expval->list x)
  (match x
    [(list-val x) x]
    [(num-val x)
     (error (format "Expected list but got integer ~a" x))]
    [(bool-val x)
     (error (format "Expected list but got boolean ~a" x))]))



(provide
 expval?
 (contract-out
  [expval->num (-> expval? integer?)]
  [expval->bool (-> expval? boolean?)]
  [expval->list (-> expval? any/c)]
  [struct num-val ((x integer?))]
  [struct list-val ((x any/c))]
  [struct bool-val ((x boolean?))]))

