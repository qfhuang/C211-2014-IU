;;Name - Liam Bolling

;;1
;;What it does - a procedure molehill->mountain that takes a list
;;and returns it with all occurences of the symbol molehill replaced
;;by the symbol mountain. Use map.
(define molehill->mountain
  (lambda (ls)
    (map (lambda (x) (cond [(equal? x 'molehill) 'moutain] [else x])) ls)))



;;2
;;What it does - a procedure duck-duck-goose that takes a list ls and builds a
;;new list of the same length as ls, in which each element but the last is
;;the symbol duck, and the last element is the symbol goose.
(define duck-duck-goose
  (lambda (ls)
    (cond
      [(equal? (cdr ls) '()) (cons 'goose '())]
      [else (cons 'duck (duck-duck-goose (cdr ls)))])))



;;3 - A
;;What it does - A procedure u-double that takes a list ls
;;representing a number in unary and returns the list representing
;;twice the number.
(define u-double
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons (car ls) (cons (car ls) (u-double (cdr ls))))])))



;;3 - B
;;What it does - a procedure u-multiply that takes two lists ls1
;;and ls2 each representing a number in unary and returns a list
;;representing the multiplication of the two numbers in unary.
(define u-multiply
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [else (u-multiply (cdr (cdr ls1)) (u-double ls2))])))



;;3 - C
;;the procedure u-half that takes a list ls representing a number
;;in unary and returns a list representing half of the number.

;;This is the helper to u-half. Don't know if I need it or not but
;;it worked so... I keep it.
(define u-half-helper
  (lambda (ls ls2)
    (cond
      [(null? ls) ls2]
      [else (u-half-helper (cdr (cdr ls)) (cons 'l ls2)) ])))

(define u-half
  (lambda (ls)
    (u-half-helper ls '())))



;;3 - D
;;What it does - A procedure u-quotient that takes two lists ls1 and ls2
;;each representing a number in unary, with ls2 different from '(), and
;;returns the list representing the quotient of the two numbers in unary.

;;This helped the quotient. It does the subtraction for it.
(define u-subtract
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [(null? ls1) 'error]
      [else (u-subtract (cdr ls1) (cdr ls2))])))

(define u-quotient
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(equal? (u-subtract ls1 ls2) 'error) '()]
      [else (cons 'l (u-quotient (u-subtract ls1 ls2) ls2))])))



;;3 - E
;;What it does - A procedure u-abs-dif that takes two lists ls1 and ls2
;each representing a number in unary, and returns the list representing
;;the absolute value of their difference.

;;Quick Note - This should work. It errored out in Wombat on Mac
;;but it worked on my Wombat on my Windows. It makes sense...

(define u-abs-dif
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) (u-quotient ls2 ls1)]
      [(null? ls2) (u-quotient ls2 ls1)]
      [else (u-abs-dif (cdr ls1) (cdr ls2))])))





;;4 - A
;;What it does - a procedure midpoint that takes two points, and
;;returns the midpoint between them.
(define midpoint
  (lambda (ls1 ls2)
    (cons (/ (+ (car ls1) (car ls2)) 2)
      (cons (/ (+ (car (cdr ls1)) (car (cdr ls2))) 2) '()))))




;;4 - B
;;What it does - a procedure mid-triangle that takes three points p, q,
;;and r (which are the vertices of a triangle), and returns a list of three
;;points representing a new triangle which is obtained from the original
;;triangle by taking its midpoints.
(define mid-triangle
  (lambda (p q r)
    (cons (midpoint p q) (cons (midpoint r p) (cons (midpoint q r) '())))))


;;4 - C
;;What it does - a recursive procedure mid-n-triangle that takes a
;;list of three points ls and a nonnegative integer n, and returns
;;a list of three points representing a new triangle
(define mid-n-triangle
  (lambda (ls n)
    (cond
      [(equal? n 0) ls]
      [else (mid-n-triangle (mid-triangle (car ls) (car (cdr ls)) (car (cddr ls))) (- n 1))])))




;;5 - A
;;What it does - a procedure a procedure die-toss that takes a
;;nonnegative integer n and returns a list of n outcomes.
(define die-toss
  (lambda (n)
    (cond
      [(equal? n 0) '()]
      [else (cons (+ (random 6) 1) (die-toss (- n 1)))])))


;;5 - B
;;What it does - a procedure count-face that takes a list of numbers from one
;;to six and a number n representing one of the possible outcomes of a die
;;toss, and returns the number of occurrences of n in the list ls
(define count-face
  (lambda (ls n)
    (cond
      [(null? ls) 0]
      [(equal? (car ls) n) (+ 1 (count-face (cdr ls) n))]
      [else (count-face (cdr ls) n)])))



;;6
;;What it does - a procedure first-n-numbers that takes a non-negative
;;integer n and a procedure proc and returns a list of the first n numbers
;;resulting from the application of procedure proc to each number from 1 to n.
(define first-n-numbers
  (lambda (n proc)
    (first-n-numbers-helper n '() proc)))
(define first-n-numbers-helper
  (lambda (n ls proc)
    (cond
      [(equal? n 0) ls]
      [else (first-n-numbers-helper (- n 1) (cons (proc n) ls) proc)])))



;;7
;;To be honest, I don't even know what a Sufix is...
;;(define suffix?
  ;;(lambda (ls1 ls2)
    ;;(cond
      ;;[()]


;;8
;;What it does - Finds how many ways you can addd something together.
(define addends
  (lambda (n)
    (addends-helper 1 (- n 1) n)))
(define addends-helper
  (lambda (x y z)
    (cond
      [(equal? y 0) '()]
      [(equal? (+ x y) z) (cons (cons x (cons y '())) (addends-helper (+ x 1) (- y 1) z))]
      [else (addends-helper (+ x 1) (- y 1) z)])))



;;9 - A
;;What it does - a predicate any? that takes a one-argument predicate pred?
;;and a list ls and returns #t when there exists some element in ls that
;;satisfies the given predicate.
(define any?
  (lambda (pred? ls)
    (cond
      [(null? ls) #f]
      [(pred? (car ls)) #t]
      [else (any? pred? (cdr ls))])))

;;9 - B
;;what it does - a non-recursive procedure diverse? that takes a list
;;and returns #t if it's diverse, and #f otherwise.
(define diverse?
  (lambda (ls)
    (any? (lambda (x) (not (equal? x (car ls)))) ls)))



;;10 - a
;;What it does - a procedure find-last that takes a predicate of one
;;argument and a list, and returns the index of the rightmost element in
;;the list that satisfies the predicate.
(define find-last
  (lambda (pred? ls)
   (find-last-helper ls #f 0 pred?)))
(define find-last-helper
  (lambda (ls x y pred?)
    (cond
      [(null? ls) x]
      [(pred? (car ls)) (find-last-helper (cdr ls) y (add1 y) pred?)]
      [else (find-last-helper (cdr ls) x (add1 y) pred?) ])))



;;10 - B
;;What it does - a non-recursive procedure the-last-straw that takes a
;;list and returns the index of the rightmost occurrence of the symbol straw.
(define the-last-straw
  (lambda (ls)
    (cond
      [(equal? (find-last (lambda (x) (equal? x 'straw)) ls) #f) #f]
      [else (find-last (lambda (x) (equal? x 'straw)) ls)])))