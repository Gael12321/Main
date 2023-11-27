#lang racket/base

(require racket/contract
         racket/match
         "let-ast.rkt"
         "let-vals.rkt"
         "let-env.rkt")

(define (value-of-program pgm)
  (match pgm
    [(a-program exp1)
     (value-of exp1 (init-env))]
    [_
     (error (format "Expected program but got ~a" pgm))]))

(define (printval val)
  (match val
    [(num-val x)   (printf "~a" x)]
    [(bool-val x)  (printf "~a" (if x "#t" "#f"))]
    [(pair-val x)  (print-pair x)]
    [(null-val)    (printf "()")]
    [_             (printf "Wtf is this!?")]))

(define (print-pair pair)
  (printf "(")
  (printval (car pair))
  (print-list (cdr pair))
  (printf ")"))


(define (value-of exp env)
  (match exp
    [(const-exp num)
     (num-val num)]
    [(var-exp var)
     (apply-env env var)]
    [(diff-exp exp1 exp2)
     (num-val (- (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))]
    [(add-exp exp1 exp2)
     (num-val (+ (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))]
    [(mult-exp exp1 exp2)
     (num-val (* (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))]
    [(div-exp exp1 exp2)
     (num-val (quotient (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env))))]
    [(zero?-exp exp1)
     (bool-val (zero? (expval->num (value-of exp1 env))))]
    [(equal?-exp exp1 exp2)
     (bool-val (equal? (expval->num (value-of exp1 env))
                       (expval->num (value-of exp2 env))))]
    [(greater?-exp exp1 exp2)
     (bool-val (< (expval->num (value-of exp1 env))
                  (expval->num (value-of exp2 env))))]
    [(less?-exp exp1 exp2)
     (bool-val (> (expval->num (value-of exp1 env))
                  (expval->num (value-of exp2 env))))]
    [(null?-exp exp1)
      (bool-val (equal? (value-of exp1 env) null-val))]
    [(cons-exp exp1 exp2)
     (let ([val1 (value-of exp1 env)]
           [val2 (value-of exp2 env)])
       (pair-val (cons val1 val2)))]
    [(car-exp exp1)
     (let ([val (value-of exp1 env)])
       (match val
         [(pair-val x) (car x)]))]
    [(cdr-exp exp1)
     (let ([val (value-of exp1 env)])
       (match val
         [(pair-val x) (cdr x)]))]
    [(emptylist-exp)
     (null-val)]
   [(list-exp)
    ()]


    [(if-exp exp1 exp2 exp3)
     (if (expval->bool (value-of exp1 env))
         (value-of exp2 env)
         (value-of exp3 env))]
    [(let-exp var exp1 body)
     (value-of body (extend-env var (value-of exp1 env) env))]
    [(minus-exp exp1)
     (num-val (- 0 (expval->num (value-of exp1 env))))]
    [(print-exp exp1)
     (printf "\"")
     (printval (value-of exp1 env))
     (printf "\"\n")
     (num-val 1)]
    [_
     (error (format "Expected expression but got ~a" exp))]))

(provide
 (contract-out
  [value-of-program (-> a-program? expval?)]
  [value-of (-> expression? environment? expval?)]))
