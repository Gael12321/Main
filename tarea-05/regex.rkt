#lang racket
(define open-paren-regex #rx"\\(")
(define close-paren-regex #rx"\\)")
(define define-regex #rx"^define$")
(define sum-regex #rx"^\\+$") 
(define mult-regex #rx"^\\*$")
(define identifier-regex #px"^[x-z][x-z\\d]*")
(define number-regex #px"^[+-]?[\\d][\\d]*$")

(provide open-paren-regex
         close-paren-regex
         define-regex
         sum-regex
         mult-regex
         identifier-regex
         number-regex)
