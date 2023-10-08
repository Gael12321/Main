
#lang racket
(require rackunit
         rackunit/text-ui
         "regex.rkt")

(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-true (regexp-match? open-paren-regex "("))
              (check-false (regexp-match? open-paren-regex ")"))
              (check-false (regexp-match? open-paren-regex "x")))
   
   (test-case "close-paren-regex"
              (check-true (regexp-match? close-paren-regex ")"))
              (check-false (regexp-match? close-paren-regex "("))
              (check-false (regexp-match? close-paren-regex "x")))
   
   (test-case "define-regex"
              (check-true (regexp-match? define-regex "define"))
              (check-false (regexp-match? define-regex "defined"))
              (check-false (regexp-match? define-regex "xxxdefine")))
  
  (test-case "sum-regex"
             (check-true (regexp-match? sum-regex "+"))
             (check-false (regexp-match? sum-regex "++"))
             (check-false (regexp-match? sum-regex "+1")))
  
  (test-case "mult-regex"
             (check-true (regexp-match? mult-regex "*"))
             (check-false (regexp-match? mult-regex "**"))
             (check-false (regexp-match? mult-regex "*1")))
  (test-case "identifier-regex"
             (check-true (regexp-match? identifier-regex "x"))
             (check-true (regexp-match? identifier-regex "xy1"))
             (check-true (regexp-match? identifier-regex "x1z"))
             (check-false (regexp-match? identifier-regex "1xyz"))
             (check-false (regexp-match? identifier-regex "123xyz123")))
  (test-case "number-regex"
             (check-true (regexp-match? number-regex "123"))
             (check-true (regexp-match? number-regex "-123"))
             (check-true (regexp-match? number-regex "+123"))
             (check-false (regexp-match? number-regex "-45.67"))
             (check-false (regexp-match? number-regex "+0.123"))
             (check-false (regexp-match? number-regex "xyz"))
             (check-false (regexp-match? number-regex "x12")))))

(run-tests regex-tests 'verbose)
