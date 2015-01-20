;;Name - Liam Bolling

;;1 A
(define two-dice-add
  (+ (+ (random 5) 1) (+ (random 5) 1)))

;;1 B
(define u-odd?
  (lambda (ls)
    (cond
      [(equal? ls '(l)) #t]
      [(equal? ls '()) #f]
      [else (u-odd? (cdr ( cdr ls)))])))

;;1 C
(define u-even?
  (lambda (ls)
    (cond
      [(equal? ls '(l)) #f]
      [(equal? ls '()) #t]
      [else (u-even? (cdr ( cdr ls)))])))


;;2 A
;;What it does - a procedure interleave that takes two lists of the same
;;length, left-ls and right-ls, and returns the list which results from
;;interleaving the elements of the two lists starting with the first element
;;of left-ls.
(define interleave
  (lambda (left-ls right-ls)
    (cond
      [(null? left-ls) '()]
      [else (cons (car left-ls) (cons (car right-ls) (interleave (cdr left-ls)
                                                       (cdr right-ls))))])))


;;2 B
;;What it does - a procedure cubes that takes a nonnegative integer n and
;;returns a list of the cubes of the integers from 0 up to and including n.
(define cubes-helper
  (lambda (num ls)
    (cond
      [(equal? num 0) (cons 0 ls)]
      [else (cubes-helper (- num 1) (cons (* num (* num num)) ls))])))
(define cubes
  (lambda (num)
    (cubes-helper num '())))


;;2 - C
;;What is does - a procedure inverses that takes a positive integer n and
;;returns a list of the inverses of the integers from 1 up to and including n.
(define inverses
  (lambda (num)
    (inverses-helper num '())))
(define inverses-helper
  (lambda (num ls)
    (cond
      [(equal? num 0) ls]
      [else (inverses-helper (- num 1) (cons (/ 1 num) ls))])))


;;3
;;What it does - a procedure trim-when that takes a predicate of one argument
;;and a list ls, and returns the result of removing elements from the front of
;;ls when encountering an item that satisfies the given predicate.
(define trim-when
  (lambda (arg ls)
    (cond
      [(not (arg (car ls))) ls]
      [else (trim-when arg (cdr ls))])))


;;4 A
;;What it does - Complete the following definition of the variable www
;;so that it is an adjacency list representation of the above graph.
(define www '((a b c d)(b e)(c e f)(d f h)(e f)(f h g)(g i j h)(h o)(i j)
              (j l m n)(k o)(l o)(m o)(n o)(o p)(p a)))

;;4 B
;;What it does - Code for the lookup procedure from a5 into your a6.ss file.
(define lookup
  (lambda (sym ls)
    (cond
      [(null? ls) '()]
      [(equal? (caar ls) sym) (cdar ls)]
      [else (lookup sym (cdr ls))])))

;;4 C
;;What it does - a procedure get-vertices that takes a digraph and returns
;;a list of vertices in the graph, as shown below.
(define get-vertices
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons (caar ls) (get-vertices (cdr ls)))])))

;;4 D
;;What it does - a procedure num-vertices that takes a digraph and returns
;;the number of vertices.
(define num-vertices
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+ 1 (num-vertices (cdr ls)))])))


;;5 A
;;What it does - a procedure next-page that a vertex representing a page on
;;the web and a digraph representing the links connecting pages in the World
;;Wide Web, and returns the next page visited by a Random Surfer.
(define next-page-helper
  (lambda (sym ls)
    (cond
      [(equal? sym 0) (car ls)]
      [else (next-page-helper (- sym 1) (cdr ls))])))

(define next-page
  (lambda (sym ls)
    (cond
      [(<= (random 100) 15) (next-page-helper 2 (next-page-helper
                                              (random (num-vertices ls)) ls)) ]
      [else (next-page-helper 1 (lookup sym ls)) ]
      )))

;;5 B
;;What it does - a procedure random-surfer that takes web graph and a
;;non-negative integer representing the length of the path explored by
;;the surfer.
(define random-sufer
  (lambda (sym ls)
    (cond
      [(equal? sym 0) (car ls)]
      [else (next-page-helper (- sym 1) (cdr ls))])))



;;6 A
;;What it does - a procedure make-counters that takes web graph and returns a
;;list of two-element sublists. Each sublist contains a vertex and the value
;;zero.
(define make-counters
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons (cons (car (get-vertices ls)) (cons '0 '()))
              (make-counters (cdr ls)))])))


;;6 B
;;What it does - a procedure visit-page that takes a vertex (in the web graph)
;;and a list of visit counters, and returns the list of counters that results
;;from incrementing the counter associated with the given vertex's page.
;;(define visit-page
 ;;(lambda (sym ls)
  ;;(cond
   ;;[(null? ls)]
   ;;[])))