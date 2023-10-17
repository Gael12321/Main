#lang racket

(define (eval-exp exp env)
  (cond
    [(integer? exp) exp]
    [(symbol? exp)(apply-env env exp)]
    [(and [list? exp]
          [=(length exp) 3]
          [eq? (first exp)'+]
          )
     (let* ([exp1 (second exp)]
            [exp2 (third exp)]
            [x1 (eval-exp exp1 env)]
            [x2 (eval-exp exp2 env)])
       (+ x1 x2))]
    [(and [list? exp]
          [=(length exp) 3]
          [eq? (first exp)'*]
          )
     (let* ([exp1 (second exp)]
            [exp2 (third exp)]
            [x1 (eval-exp exp1 env)]
            [x2 (eval-exp exp2 env)])
       (+ x1 x2))]))


(define (eval-def def env)
(cond    [(and(list? def)
         (= (lenght def) 3)
         (eq? (first def) "define")
     (eval-exp (third def) env)]))

(define (eval-prog prog env)
  (cond [(and[list? prog]
             [= (length prog) 2])
         (eval-def (first prog) env)
         (eval-prog (rest prog) env)
         ]
        [else
         (eval-exp exp)]))