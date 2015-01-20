;;Name - Liam Bolling

;; 1
;;What it does - a procedure double-symbol that takes a list ls and a symbol
;;symbol and returns the list with all top-level occurrences of the symbol
;;symbol doubled.
(define double-symbol
  (lambda (symbol ls)
    (cond
      [(null? ls) '()]
      [(equal? (car ls) symbol) (cons symbol ls)]
      [else (cons (car ls) (double-symbol symbol (cdr ls)))]
      )))

;; 2
;;What does it - a procedure find-replace that takes symbols f, s and a list
;;ls, and returns the list with all top-level occurrences of the symbol f
;;replaced by the symbol s
(define find-replace
  (lambda (f s ls)
    (cond
      [(null? ls) '()]
      [(equal? (car ls) f) (cons s (cdr ls))]
      [else (find-replace f s (cdr ls))]
      )))

;;3 A
;;What it does - takes two lists ls1 and ls2 each representing a number in
;;unary and returns the list representing the subtraction of the two numbers in
;;unary.
(define u-subtract
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) '()]
      [else (u-subtract (cdr ls1) (cdr ls2))]
      )))

;;3 B
;;What it does - takes two lists ls1 and ls2 each representing a number in
;;unary and returns #t if and only if the number represented by ls1 is greater
;;than or equal to the number represented by ls2.
(define u-gte?
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) #t]
      [(null? ls1) #f]
      [else (u-gte? (cdr ls1) (cdr ls2))]
      )))


;;4
;;What it does - A procedure index-of that takes a symbol symbol and a list
;;ls containing the symbol at least once, and returns the index of the first
;;occurrence of symbol in ls.
(define index-of
  (lambda (sym ls)
(cond
      [(equal? (car ls) symbol) 0]
      [else
       (not (equal? sym (car ls))) (+ (index-of-helper sym ls) (index-of sym
                                                        (cdr ls)))])))
(define index-of-helper
  (lambda (sym ls)
    (if (no (equal? sym (car ls))) 1 0)))


;;5
;;What it does - A procedure remove-symbol that takes a symbol symbol and a
;;list ls and returns a list consisting of the same elements of ls without
;;symbol.
(define remove-symbol
  (lambda (sym ls)
    (cond
      [(null? ls) '()]
      [(equal? sym (car ls)) (remove-symbol sym (cdr ls))]
      [else (cons (car ls) (remove-symbol sym (cdr ls)))])))


;;6 A
;;WHat it does - Picks a pile that is largest.
(define pick-pile
  (lambda (ls1 ls2)
    (cond
      [(equal? (u-gte? ls1 ls2) #f) ls2]
      [else ls1])))

;;6 B Problem


;;7 A
;;What it does - A procedure append that takes two lists, ls1 and ls2,
;;and returns the list consisting of all elements of ls1 followed by all
;;elements of ls2.
(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))
(define www
       '((1 the dog chewed the rug)
          (2 on the rug slept the dog)
          (3 the rug rats slept on)))
;;7 B
;;What it does - A procedure all-words that takes a list representing the
;;World Wide Web and returns a single list of all the words on all pages, as
;;shown below.
(define all-words
  (lambda (index)
    [cond
      [(null? index) '()]
      [else
       (append (get-words (car index)) (all-words (cdr index)))]]))



;;8 A
;;What it does - A predicate member? that takes an item and a list, and returns
;;#t if the item appears at top-level in the list, and #f otherwise.
(define member?
  (lambda (a ls)
    (cond
      [(equal? (member a ls) #f) #f]
      [(not (null? (member a ls))) #t]
      [else '()])))

;;8 B
;;What it does - A procedure intersection that takes two lists ls1 and ls2,
;;and returns a list containing those elements of ls1 that are also in ls2.
(define intersection
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(null? ls2) '()]
      [(member? (car ls1) ls2) (cons (car ls1) (intersection (cdr ls1) ls2))]
      [else (intersection (cdr ls1) ls2)])))

;;8 C
;;What it does - A procedure lookup that takes a symbol representing a word
;;and a web index, and returns the list of pages containing the word.
(define lookup
  (lambda (s ls)
    (cond
      [(null? ls) '()]
      [(equal? (caar ls) s) (cdar ls)]
      [else (lookup s (cdr ls))])))

;;8 D
;;What it does - A procedure match-multi-word-query that takes a non-empty list
;;of query words and a web index, and returns a list of pages containing all the
;;query words.
(define index
       '((a 3) (cat 1 3) (dog 2 3) (mat 1 2) (on 1 2) (sat 1 3) (stood 2 3)
          (the 1 2 3) (while 3)))
(define  match-multi-word-query
  (lambda (x index)
     (cond
       [(null? (cdr x))(lookup (car x) index)]
       [else (intersection (lookup (car x) index)(match-multi-word-query
                                                   (cdr x) index))])))

;;9
;;What it does - A predicate prefix? that takes two lists and returns #t if
;;the first list is a prefix of the second list, and #f otherwise.
(define prefix?
  (lambda (x y)
   (cond
    [(null? x) #t]
    [(null? y) #f]
    [else (prefix? (cdr x) (cdr y))])))


;9 B
;;What it does - a procedure fuse-fragments which takes two DNA sequences, seq1
;;and seq2, locates the area of overlap, and returns the result of joining the
;;two sequences together.
(define fuse-fragments
  (lambda (seq1 seq2)
    (cond
      [(prefix? seq1 seq2) seq2]
      [else (cons (car seq1) (fuse-fragments (cdr seq1) seq2))])))


;;10
;;What it does - A procedure unique that takes a list ls and returns a
;;list consisting of the first occurrence of each item in ls.
(define unique
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null?(cdr ls)) ls]
      [(member? (car ls) (cdr ls)) (cons (car ls) (unique (removal (car ls)
                                                            ls)))]
      [else (cons (car ls) (unique (cdr ls)))])))


;;11 A
;;What it does - Shows the distance of a point to an origin.
(define distance-to-origin
  (lambda (num1 num2)
    (sqrt (+ (* num1 num1) (* num2 num2)))))

;;11 B
;;What it does - Makes a set of points that are random in the first quadrent.
(define make-points
  (lambda (amount)
    (cond
      [(equal? amount 0) '()]
      [else (cons (cons (random 1.0) (cons (random 1.0) '())) (make-points
                                                       (- amount 1)))])))

;;11 C
;;What it does - Finds how many points are in a circle that is 1 x 1
(define count-in-circle
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(<= (distance-to-origin (car (car ls)) (car (cdr (car ls)))) 1) (+ 1
                                                 (count-in-circle (cdr ls)))]
      [else (count-in-circle (cdr ls))])))

;;11 D
(define calculate-pi
   (lambda(n)
      (/ (* 4.0 (count-in-circle (make-points n))) n)))
;;Answer - It finds how many random points are in one quadrent of the
;;circle and then multiples it by 4 to find the value of pi.