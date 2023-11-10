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
                            (list)))
              
               ;Problema 2.1 :
   (test-case "take"
              (check-equal? (take (list "ab" "bc" "ef") 3)
                            (list "ab" "bc" "ef"))
              (check-equal? (take (list "abcdef") 0)
                            (list))
              (check-equal? (take '() 3)
                            (list)))

   ;Problema 2.2 :
   (test-case "drop"
              (check-equal? (drop (list 1 2 3 4 5) 3)
                            (list 4 5))
              (check-equal? (drop (list 1 2 3 4 5) 0)
                            (list 1 2 3 4 5))
              (check-equal? (drop '() 0)
                            (list)))


   ;Problema 5 :
   (test-case "list->chunks"
              (check-equal? (list->chunks (list 1 2 3 4 5) 2)
                            '((1 2) (3 4) (5)))
              (check-equal? (list->chunks (list 1 2 3 4 5) 3)
                            '((1 2 3) (4 5)))
              (check-equal? (list->chunks '() 2)
                            (list)))

   ;Problema 6 :
   (test-case "partition"
              (check-equal? (partition "abcdef" 2)
                            (list "ab" "cd" "ef"))
              (check-equal? (partition "abcdef" 3)
                            (list "abc" "def")))

   ;Problema 7 :
   (test-case "add-isort" ;manda a llamar al isort pero no jala asi que gg
              (check-equal? (add-isort 1 (list 2 3 4 5) "asc")
                            (list 1 2 3 4 5))
              (check-equal? (add-isort 5 (list 1 2 3 4) "desc")
                            (list 5 4 3 2 1))
              (check-equal? (add-isort 3 (list 1 2 4 5) "asc")
                            (list 1 2 3 4 5)))

 ; Pruebas para la función smallers
   (test-case "Smallers"
              (check-equal? (smallers '(4 2 6 1 5) 5) '(4 2 1))
              (check-equal? (smallers '(10 20 30 40) 25) '(10 20))
              (check-equal? (smallers '() 3) '()))

   ; Pruebas para la función largers
   (test-case "Largers"
              (check-equal? (largers '(4 2 6 1 5) 5) '(6))
              (check-equal? (largers '(10 20 30 40) 25) '(30 40))
              (check-equal? (largers '() 3) '()))

   ; Pruebas para la función quicksort
   (test-case "Quicksort"
              (check-equal? (quicksort '(4 2 6 1 5)) '(1 2 4 5 6))
              (check-equal? (quicksort '(10 5 30 20 40)) '(5 10 20 30 40))
              (check-equal? (quicksort '()) '()))

   ; Pruebas para la función quicksort-add
   (test-case "Quicksort-Add"
              (check-equal? (quicksort-add 7 '(4 2 6 1 5) "asc") '(1 2 4 5 6 ))
              (check-equal? (quicksort-add 7 '(10 5 30 20 40) "desc") '(40 30 20 10 5)))

   ; Pruebas para la función add-quicksort
   (test-case "Add-Quicksort"
              (check-equal? (add-quicksort 7 '(4 2 6 1 5) "asc") '(1 2 4 5 6 7))
              (check-equal? (add-quicksort 7 '(10 5 30 20 40 50 70 60 50 10) "desc") '(70 60 50 50 40 30 20 10 10 7 5)))

   ; Pruebas para la función quicksort-isort
   (test-case "Quicksort-Isort"
              (check-equal? (quicksort-isort 7 '(4 2 6 1 5) "asc") '(1 2 4 5 6)) ;manda a llamar al isort pero no jala asi que gg
              (check-equal? (quicksort-isort 7 '(10 5 30 20 40) "desc") '(40 30 20 10 5)))

   ; Pruebas para la función smallers-filter
   (test-case "Smallers-Filter"
              (check-equal? (smallers-filter '(4 2 6 1 5) 5) '(4 2 1))
              (check-equal? (smallers-filter '(10 20 30 40) 25) '(10 20))
              (check-equal? (smallers-filter '() 3) '()))

   ; Pruebas para la función largers-filter
   (test-case "Largers-Filter"
              (check-equal? (largers-filter '(4 2 6 1 5) 5) '(6))
              (check-equal? (largers-filter '(10 20 30 40) 25) '(30 40))
              (check-equal? (largers-filter '() 3) '()))

   ; Pruebas para la función quicksort-add-filter
   (test-case "Quicksort-Add-Filter"
              (check-equal? (quicksort-add-filter 7 '(4 2 6 1 5) "asc") '(1 2 4 5 6 ))
              (check-equal? (quicksort-add-filter 7 '(10 5 30 20 40) "desc") '(40 30 20 10 5)))

   ; Pruebas para la función add-quicksort-filter
   (test-case "Add-Quicksort-Filter"
              (check-equal? (add-quicksort-filter 7 '(4 2 6 1 5) "asc") '(1 2 4 5 6 7))
              (check-equal? (add-quicksort-filter 7 '(10 5 30 20 40) "desc") '(40 30 20 10 7 5)))))


(run-tests ps-tests 'verbose)