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