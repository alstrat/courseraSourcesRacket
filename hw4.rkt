#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; (1)
; function sequence(low, high, stride)
; all arguments assumed to be numbers, stride is positive. 
; Produces a list of numbers from low to high (including low and possibly high)
; separted by stride and in sorted order.
(define (sequence low high stride) 
  (if (< high low)
      null
      (append (list low) (sequence (+ low stride) high stride))))

; (2)
; function string-append-map(list of strings xs, string suffix). 
; returns a - list of strings.
; Each element of the output is corresponding element of the input appended with suffix 
;(with no extra space between the element and suffix). 
; uses Racket-library functions map and string-append.
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; (3)
; function list-nth-mod(list xs, a number n). 
; If the number is negative,terminate the computation with (error "list-nth-mod: negative number"). 
; Else if the list is empty, terminate the computation with (error "list-nth-mod: empty list"). 
; Else return the ith element of the list 
; where we count from zero and i is the remainder produced when dividing n by the list's length. 
; Library functions length, remainder, car, and list-tail are all useful.
; see the Racket documentation. Sample solution is 6 lines.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(= (remainder n (length xs)) 0) (car xs) ]
        [#t (car(list-tail xs (remainder n (length xs))))] ))

; (4)
; Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
; the first n values produced by s in order. Assume n is non-negative.
(define (stream-for-n-steps s n) 
  (if (= n 0)
      '()
      (append 
       (list(car (s))) (stream-for-n-steps (cdr (s)) (- n 1)))))

; (5)
; funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
; except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). 
; Remember a stream is a thunk that when called produces a pair. 
; Here the car of the pair will be a number and the cdr will be another stream.
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x  (lambda () (f (cond
                                                   [ (< x 0)(+ 1 (* -1 x))]
                                                   [ (= (remainder (+ x 1) 5) 0)(* -1 (+ x 1))]
                                                   [ #t (+ x 1)])))))])
    (lambda () (f 1))))

; (6)
; stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called... etc.
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x  (lambda () (f (cond
                                                   [ (equal? x "dan.jpg") "dog.jpg"]
                                                   [ #t "dan.jpg"])))))])
    (lambda () (f "dan.jpg"))))

; (7)
; function stream-add-zero that takes a stream s and returns another stream. 
; If s would produce v for its ith element, then (stream-add-zero s) would produce 
; the pair (0 . v) for its ith element. 
(define (stream-add-zero s) 
  ( lambda () (cons (cons 0 (car (s))) (stream-add-zero(cdr (s))))))


; (8)
; function cycle-lists that takes two lists xs and ys and returns a stream. 
; The lists may or may not be the same length, but assume they are both non-empty. 
; The elements produced by the stream are pairs where the first part is from xs 
; and the second part is from ys. 
; The stream cycles forever through the lists. 
; For example, if xs is '(1 2 3) and ys is '("a" "b"), 
; then the stream would produce, (1 . "a"), (2 . "b"), (3 . "a"),
; (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"), etc.
; Sample solution is 6 lines and is more complicated than the previous stream problems. 
; Hints: Use one of the functions you wrote earlier. 
; Use a recursive helper function that takes a number n and calls
;itself with (+ n 1) inside a thunk.
(define (cycle-lists xs ys) 
  (letrec ([f (lambda (xs ys n) 
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                      (lambda ()(f xs ys (+ n 1)))))])
    (lambda () (f xs ys 0))))


; (9)
; function vector-assoc that takes a value v and a vector vec. 
; It should behave like Racket's assoc library function except 
;(1) it processes a vector (Racket's name for an array) instead of a list,
;(2) it allows vector elements not to be pairs in which case it skips them
;(3) it always takes exactly two arguments. 
; Process the vector elements in order starting from 0. 
; You must use library functions vector-length, vector-ref, and equal?. 
; Return #f if no vector element is a pair with a car field equal to v, 
; else return the first pair with an equal car field. 
(define (vector-assoc v vec) 
  (letrec ([f (lambda (v vec n)
                (cond
                  [ (= (vector-length vec) n) #f]
                  [ (and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v)) 
                    (vector-ref vec n)]
                  [#t (f v vec (+ n 1))])
                )])
    (f v vec 0)))

; (10)
; function cached-assoc that takes a list xs and a number n 
; and returns a function that takes one argument v and returns the same thing that (assoc v xs) would return. 
; However, you should use an n-element cache of recent results to possibly make this function faster than just calling assoc
;(if xs is long and a few elements are returned often).
; The cache must be a Racket vector of length n that is created by the call to cached-assoc (use Racket library function vector or make-vector) 
; and used-and-possibly-mutated each time the function returned by cached-assoc is called. 
; Assume n is positive.
; The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it first checks the cache for the answer. 
; If it is not there, it uses assoc and xs to get the answer and if the result is not #f, it adds the pair to the cache before returning (using vector-set!). 
; The cache slots are used in a round-robin fashion: the first time a pair is added to the cache it is put in position 0,
; the next pair is put in position 1, etc. up to position n - 1 and then back to position 0 (replacing the pair already there), then position 1, etc.
; Hints:
;  In addition to a variable for holding the vector whose contents you mutate with vector-set!,
;  use a second variable to keep track of which cache slot will be replaced next. 
;  After modifying the cache, increment this variable (with set!) or set it back to 0.
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [memoPos -1]
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans ans
                     (let ([new-ans (assoc v xs)])
                       (cond [ new-ans (begin
                                         (if (= (+ 1 memoPos) n)
                                              (set! memoPos 0 )
                                             (set! memoPos (+ 1 memoPos)))
                                         (vector-set! memo memoPos new-ans))])
                       new-ans))))])
    (lambda (v) (f v))))