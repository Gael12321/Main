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

    
    [(cons-exp exp1 exp2)
    ]

    [(cdr-exp exp1)
     ]

    [(car-exp exp1)
     ]

    [(null?-exp exp1)
     (bool-val (null? ()))]
    
    [(emptylist-exp)
     ]


    [(if-exp exp1 exp2 exp3)
     (if (expval->bool (value-of exp1 env))
         (value-of exp2 env)
         (value-of exp3 env))]
    [(let-exp var exp1 body)
     (value-of body (extend-env var (value-of exp1 env) env))]
    [(minus-exp exp1)
     (num-val (- 0 (expval->num (value-of exp1 env))))]
    [_
     (error (format "Expected expression but got ~a" exp))]))

(provide
 (contract-out
  [value-of-program (-> a-program? expval?)]
  [value-of (-> expression? environment? expval?)]))
