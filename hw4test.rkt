#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

(require rackunit)
(require "hw4.rkt")

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test1")
   (check-equal? (sequence 1 3 1) (list 1 2 3) "Sequence test2")
   (check-equal? (sequence 1 300 100) (list 1 101 201) "Sequence test3")
   (check-equal? (sequence 10 3 1) '() "Sequence test4")
   (check-equal? (sequence -1 3 1) (list -1 0 1 2 3) "Sequence test5")
   
   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test1")
   (check-equal? (string-append-map (list "first" "second") "added") 
                 '("firstadded" "secondadded") "string-append-map test2")
   (check-equal? (string-append-map null "added") 
                 '() "string-append-map test3")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test1")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "list-nth-mod test2")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 6) 1 "list-nth-mod test3")
   (check-exn (regexp "list-nth-mod: negative number") 
              (lambda () (list-nth-mod (list 0 1 2 3 4) -2)) "list-nth-mod test4")
   (check-exn (regexp "list-nth-mod: empty list") 
              (lambda () (list-nth-mod null 2)) "list-nth-mod test5")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 1) (list 1) "stream-for-n-steps test1")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 2) (list 1 1) "stream-for-n-steps test2")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) 
                 (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test1")
   (check-equal? (stream-for-n-steps funny-number-stream 0) 
                 '() "funny-number-stream test2")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test1")
   (check-equal? (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg") 
                 "dan-then-dog test2")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1)
                 (list (cons 0 1)) "stream-add-zero test1")
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 5)
                 (list (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 -5)) 
                 "stream-add-zero test1")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 7) 
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a") (cons 3 "b") (cons 1 "a")) "cycle-lists test1")
   
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" )) 5) 
                 (list (cons 1 "a") (cons 2 "a") (cons 3 "a") (cons 1 "a") (cons 2 "a") ) "cycle-lists test2")
   
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 ) (list "a" "b" "c")) 7) 
                 (list (cons 1 "a") (cons 2 "b") (cons 1 "c") (cons 2 "a") (cons 1 "b") (cons 2 "c") (cons 1 "a")) "cycle-lists test3")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector '())) #f "vector-assoc test1")
   (check-equal? (vector-assoc 4 (vector (cons 4 1))) (cons 4 1) "vector-assoc test2")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test3")
   (check-equal? (vector-assoc 8 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test4")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test1")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 1) 4) #f "cached-assoc test2")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 1) 3) (cons 3 4) "cached-assoc test3")
   
   ; while-less test
   ;(check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
