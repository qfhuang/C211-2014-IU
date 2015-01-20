;;Name - Liam Bolling

;;1. A
(define u-lt?
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) #f]
      [(null? ls1) #t]
      [else (u-lt? (cdr ls1) (cdr ls2))])))

;;1. B
(define u-min-helper
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) 'lukeSkywalker]
      [(null? ls1) 'darthVader]
      [else (u-min-helper (cdr ls1) (cdr ls2))])))
(define u-min
  (lambda (ls1 ls2)
    (cond
      [(equal? (u-min-helper ls1 ls2) 'lukeSkywalker) ls2]
      [else ls1])))

;;1. C
(define u-pow-two
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [else (u-pow-two (cons 'l ls1) (cdr ls2))])))

;;1. D
(define single-grain-toss
  (lambda ()
  (+ 1 (random 65))))

;;1. E
(define grains-toss
  (lambda (n)
    (cond
      [(equal? n 0) '()]
      [else (cons (single-grain-toss) (grains-toss (- n 1)))])))

;;1. F
(define empty-square?
  (lambda (n ls)
    (cond
      [(null? ls) #t]
      [(equal? (car ls) n) #f]
      [else (empty-square? n (cdr ls))])))

;;1. G
(define check-if-there
  (lambda (n ls)
    (cond
      [(null? ls) #f]
      [(equal? n (car ls)) #t]
      [else (check-if-there n (cdr ls))])))
(define empty-squares-helper
  (lambda (ls x)
    (cond
      [(equal? x 65) '()]
      [(equal? #t (check-if-there x ls)) (empty-squares-helper ls (add1 x))]
      [else (cons x (empty-squares-helper ls (add1 x)))])))
(define empty-squares
  (lambda (ls)
    (empty-squares-helper ls 1)))

;;1. H
(define area-trapezoid
  (lambda (base1 base2 leg)
    (* (/ (+ base1 base2) 2) leg)))

;;1. I
(define circle-y
  (lambda (x)
    (sqrt (- 1 (* x x)))))

;;1. J
(define split-equally-helper
  (lambda (x divisor ls)
    (cond
      [(equal? x 0) ls]
      [else (split-equally-helper (- x divisor) divisor (cons x ls))])))
(define split-equally
  (lambda (x)
    (cons 0.0 (split-equally-helper 1 (/ 1 x) '()))))

;;1. K
(define area-circle-helper
  (lambda (ls x)
    (cond
      [(null? ls) 0]
      [(null? (cdr ls)) 0]
      [else (+ (area-trapezoid (circle-y (car ls)) (circle-y (car (cdr ls))) (/ 1 x)) (area-circle-helper (cdr ls) x))])))
(define area-circle
  (lambda (x)
    (cond
      [(equal? x 0) 0]
      [else (* (area-circle-helper (split-equally x) x) 4)])))



;;2. A
;;What it does - A procedure squares that takes a list of numbers ls and
;;returns the list formed by taking the squares of each element of ls.
(define squares
  (lambda (ls)
    (map (lambda (x) (* x x)) ls)))

;;2. B
;;What it does - A procedure pair-additions that takes a list of lists of
;;two numbers ls and returns the list formed by taking the additions of the
;;two numbers in each element of ls.
(define pair-additions
  (lambda (ls)
    (map (lambda (x) (+ (car x) (car (cdr x)))) ls)))

;;2. C
;;What it does - A procedure values-only that takes an association list alist
;;and returns the list consisting of the value elements in alist.
(define values-only
  (lambda (ls)
    (map (lambda (x) (car (cdr x))) ls)))

;;2. D
;;What it does - A procedure list-pow2 that takes a list of non-negative
;;integers, representing exponents, and returns a list of powers of two, as
;;shown below.
(define list-pow2
  (lambda (ls)
    (map (lambda (x) (expt 2 x)) ls)))

;;2. E
;;What it does - A procedure prefix-3 that takes a list of strings and returns
;;the list of the strings without the substring of the first three characters in
;;each word, as shown below.
(define prefix-3
  (lambda (ls)
    (map (lambda (x) (substring x 3 (string-length x))) ls)))



;;3.
;;what it does - A procedure map-ran that takes two one-argument procedures,
;;proc1 and proc2, a list, and a probability value p (i.e., a number between
;;zero and one), and applies procedure proc1 with probability p and procedure
;;proc2 with probability 1 - p to each element of the list. Recall that
;;(random 1.0) gives a random number between zero and one.
(define map-ran
  (lambda (proc1 proc2 ls n)
    (map (lambda (x) (cond [(<= n (random 1.0)) (proc2 x)] [else (proc1 x)])) ls)))



;;4.
;;What it does - a procedure photo-negative that takes an image and inverts
;;each color value (red, green, and blue) by subtracting it from 255.
;;For example, an orange pixel, 255 165 0, becomes the light blue pixel, 0 90 255.
(import (c211 image))
(define photo-negative
  (lambda (img)
    (image-map (lambda (c) (color (- 255 (color-ref c 'red)) (- 255 (color-ref
                                c 'green)) (- 255 (color-ref c 'blue)))) img)))


;;5.
;;What it does - a procedure obamicon that takes an image and uses image-map
;;to produce a new image where each pixel is mapped to a new color based on the
;;sum of the RGB components in the given pixel.
(define obamicon
  (lambda (img)
    (image-map (lambda (c) (let ([x (+ (color-ref c 'red) (+ (color-ref
                                c 'green) (color-ref c 'blue)))])
                  (cond
                    [(<= x 181) (color 0 51 76)]
                    [(and (>= x 182) (<= x 363)) (color 217 26 33)]
                    [(and (>= x 364) (<= x 545)) (color 112 150 158)]
                    [(and (>= x 546) (<= x 765)) (color 252 227 166)]))) img)))