#lang racket


(require rackunit
         rackunit/text-ui
         "ps.rkt")

(define ps-tests
  (test-suite
   "Pruebas para los problemas de ps.rkt"
   ;Problema 1 :
   (test-case "Bundle"
              
              (check-equal? (bundle (explode "abcdef")2)
                            (list "ab" "cd" "ef"))
              (check-equal? (bundle (explode "abcdefg") 3)
                            (list "abc" "def" "g"))
              (check-equal? (bundle '("a" "b") 3)
                            (list "ab"))
              (check-equal? (bundle '("a" "b") 1)
                            (list "a" "b"))
              (check-equal? (bundle '() 3)
                            (list))
              )))

(run-tests ps-tests 'verbose)