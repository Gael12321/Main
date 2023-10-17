#lang racket

(define (empty-env)
  (lambda (var)
    (error "Plomo detectado" var)))


(define (apply-env env var)
  (env var))

(define (extend-env var1 val env)
  (lambda (var2)
    (if(eq? var1 var2)
       val
       (env var2))))




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
         (= (length def) 3)
         (eq? (first def) "define")
     (eval-exp (third def) env))]))


(define (eval-prog prog env)
  (cond [(and[list? prog]
             [= (length prog) 2])
         (eval-def (first prog) env)
         (eval-prog (rest prog) env)
         ]
        [else
         (eval-exp exp)]))
