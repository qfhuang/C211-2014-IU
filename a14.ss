(import (c211 image))
(import (c211 matrix))
(import (c211 tree))

;1.
;A procedure b&w-subimage! that takes an image img, a row index start-row, a ;column index start-col, a height h, and a width w, and changes each pixel ;in a rectangular subimage of img to black and white.
(define b&w-subimage!
  (lambda (img start-row start-col h w)
    (define b&wColorMaker
      (lambda (clr)
        (let ([average (quotient (+ (color-ref clr 'red) (color-ref clr 'blue) (color-ref clr 'green)) 3)])
          (cond
            [(> average 127) (color 255 255 255)]
            [else (color 0 0 0)]))))
    (let ([image-height (image-cols img)] [image-width (image-cols img)])
      (let h-loop ([first-height 0])
        (when (< first-height image-height)
          (let w-loop ([first-width 0])
            (when (< first-width image-width)
              (cond
                [(and (< first-width w) (< first-height h))
                 (begin
                   (image-set! img first-height first-width (b&wColorMaker (image-ref img first-height first-width)))
                   (w-loop (add1 first-width)))]
                [else (w-loop (add1 first-width))])))
          (h-loop (add1 first-height)))))))


;2. A.
;A procedure u-rem that takes two lists ls1 and ls2 each representing a ;number in unary, with ls2 different from zero, and returns the list ;representing the remainder of the two numbers.
(define u-rem
  (lambda (ls1 ls2)
    (define u-rem-subtract
      (lambda (ls1 ls2 lsBackUp)
        (cond
          [(null? ls2) ls1]
          [(null? ls1) lsBackUp]
          [else (u-rem-subtract (cdr ls1) (cdr ls2) lsBackUp)])))
    (define check-less-than?
      (lambda (ls1 ls2)
        (cond
          [(null? ls1) #t]
          [(null? ls2) #f]
          [else (check-less-than? (cdr ls1) (cdr ls2))])))
    (cond
      [(check-less-than? ls1 ls2) (u-rem-subtract ls1 ls2 ls1)]
      [else (u-rem (u-rem-subtract ls1 ls2 ls1) ls2)])))


;2. B.
;A procedure u-log that takes a list ls representing a number in unary and ;returns the list representing the integer part of the logarithm in base two ;of the number ls.
(define u-log-help
  (lambda (ls)
    (let loop ((acc 0) (ls1 ls))
      (cond
        ((null? ls1) acc)
        (else (loop (add1 acc) (cdr ls1)))))))
(define u-log-helper
  (lambda (x y)
    (log x y)))
(define u-log
  (lambda (ls)
    (let loop ((ls1 '()) (n (floor (u-log-helper (u-log-help ls) 2))))
      (cond
        ((> n 0) (loop (cons 'l ls1) (sub1 n)))
        (else ls1)))))


;2. C.
;A procedure u-max that takes a list of lists ls each representing a number ;in unary and returns the list in ls representing the maximum number.

;Couldn't figure this out.. I am a bit lazy :/

;(define u-max
 ; (lambda (ls)
  ;  (cond
   ;   [()]


;3.
;A procedure index-of-max that takes a vector of numbers, vec, and a vector, ;index-vector, consisting only of valid indices within vec, and returns the ;index in index-vectors at which vec has the largest value.
(define index-of-max
  (lambda (vec index-vector)
    (define index-of-max-helper
      (lambda (vec index-vector currentMax index-number)
        (let ([vectorls (vector->list index-vector)])
          (cond
            [(null? vectorls) index-number]
            [(< currentMax (vector-ref vec (vector-ref index-vector 0)))
             (index-of-max-helper vec (list->vector (cdr vectorls))
               (vector-ref vec (vector-ref index-vector 0)) (car vectorls))]
            [else (index-of-max-helper vec (list->vector
                                             (cdr vectorls)) currentMax index-number)]))))
    (index-of-max-helper vec index-vector 0 0)))


;4.
;A procedure transpose that takes a matrix mat and returns its transpose.
(define mat1 (vov->matrix '#(#(1 2 3 4 5) #(6 7 8 9 10))))
(define mat2
  (vov->matrix
    '#(#(1 2 3 4 5) #(-1 0 8 9 7) #(-6 2 1 7 4) #(10 3 -3 -6 0)
        #(7 6 4 2 5) #(8 1 2 2 3))))

(define transpose
  (lambda (mat)
    (matrix-generator (matrix-cols mat) (matrix-rows mat)
      (lambda (r c) (matrix-ref mat c r)))))



;5. A.
;A procedure sum-rows that takes a matrix mat and returns a list of the sums o;f each row in the matrix (starting from the first row).
(define mat3
  (vov->matrix
    '#(#(4 14 15 1) #(9 7 6 12) #(5 11 10 8) #(16 2 3 13))))

(define sum-rows
  (lambda (mat)
    (define sumFinder
      (lambda (mat rowSelector c total)
        (let ([cols (matrix-cols mat)])
          (cond
            [(equal? cols c) total]
            [else (sumFinder mat rowSelector (add1 c) (+ total (matrix-ref mat rowSelector c)))]))))
    (define createList
      (lambda (mat i)
        (let ([repeatAmount (matrix-rows mat)])
          (cond
            [(equal? i repeatAmount) '()]
            [else (cons (sumFinder mat i 0 0) (createList mat (add1 i)))]))))
    (createList mat 0)))


;5. B.
;A procedure sum-cols that takes a matrix mat and returns a list of the sums ;of each column in the matrix (starting from the first column).
(define sum-cols
  (lambda (mat)
    (define sumFinder
      (lambda (mat colsSelector r total)
        (let ([rows (matrix-rows mat)])
          (cond
            [(equal? rows r) total]
            [else (sumFinder mat colsSelector (add1 r) (+ total (matrix-ref mat r colsSelector)))]))))
    (define createList
      (lambda (mat i)
        (let ([repeatAmount (matrix-cols mat)])
          (cond
            [(equal? i repeatAmount) '()]
            [else (cons (sumFinder mat i 0 0) (createList mat (add1 i)))]))))
    (createList mat 0)))


;5. C.
;A procedure sum-diag+ that takes a square matrix mat and returns the sum of ;each element in the main diagonal of the matrix (the main diagonal starts ;at the upper-left element and ends at the lower-right element).
(define sum-diag+
  (lambda (mat)
    (define help
      (lambda (mat sum count matrixSize)
        (cond
          [(equal? count matrixSize) sum]
          [else (help mat (+ sum (matrix-ref mat count count)) (add1 count) matrixSize)])))
    (help mat 0 0 (matrix-cols mat))))



;5. D.
;A procedure sum-diag- that takes a square matrix mat and returns the sum of ;each element in the non-main diagonal of the matrix (the non-mail diagonal ;starts at the upper right element and ends at the lower left element).
(define sum-diag-
  (lambda (mat)
    (define help
      (lambda (mat sum up left matrixSize)
        (cond
          [(equal? up matrixSize) sum]
          [else (help mat (+ sum (matrix-ref mat up left)) (add1 up) (sub1 left) matrixSize)])))
    (help mat 0 0 (sub1 (matrix-cols mat)) (matrix-cols mat))))



;5. E.
;A predicate magic-square? that takes a squared matrix mat and returns #t if ;and only if the matrix is a magic square..
(define mat4
  (vov->matrix
    '#(#(4 14 15 0) #(9 7 6 12) #(5 11 10 8) #(16 2 3 13))))

(define magic-square?
  (lambda (mat)
    (let [(setPoint (sum-diag- mat))]
      (define checkElements?
        (lambda (ls setPoint)
          (cond
            [(null? ls) #t]
            [(equal? setPoint (car ls)) (checkElements? (cdr ls) setPoint)]
            [else #f])))
      (cond
        [(and
              (checkElements? (sum-cols mat) setPoint)
              (and
                   (checkElements? (sum-rows mat) setPoint)
                   (and
                        (equal? (sum-diag- mat) setPoint)
                        (equal? (sum-diag+ mat) setPoint))))
         #t]
        [else #f]))))



;6. A.
;A procedure lose-leaves that takes a tree tr and returns the tree that ;results by removing the leaves from tr.
(define lose-leaves
  (lambda (tr)
    (cond
      [(empty-tree? tr) tr]
      [(leaf? tr) (empty-tree)]
      [else
       (tree (root-value tr) (lose-leaves (left-subtree tr))
         (lose-leaves (right-subtree tr)))])))




;6. B.
;A procedure get-leaves that takes a tree tr and returns the tree that ;results by adding 0-valued leaves to tr (adding one or two leaves whenever ;is possible to do so).
(define get-leaves
  (lambda (tr)
    (cond
      [(empty-tree? tr) (leaf 0)]
      [else
       (tree (root-value tr) (get-leaves (left-subtree tr))
         (get-leaves (right-subtree tr)))])))




;7.
;A procedure image->string that takes a black and white image of dimension ;2n x 2n, for some n, and returns a quadstring encoding of the image.

;D is a procedure that implements the function described in Problem 287 of
;Project Euler. (D n) returns an image of dimension 2^n x 2^n that looks
;like a black circle enscribed in a white rectangle.

(define solid-region?
  (lambda (img r c n)
    (let ([clr (image-ref img r c)]
          [rend (+ r (expt 2 n))]
          [cend (+ c (expt 2 n))])

      (let rloop ([r r])
        (or (= r rend)
            (and (let cloop ([c c])
                   (or (= c cend)
                       (and (color-equal?
                              (image-ref img r c) clr)
                            (cloop (+ c 1)))))
                 (rloop (+ r 1))))))))

(define (D n)
  (define ^2 (lambda (x) (* x x)))
  (let* ([a (expt 2 n)]
         [b (/ a 2)]
         [c (^2 b)])
    (make-image a a
      (lambda (x y)
        (if (< (+ (^2 (- x b)) (^2 (- y b))) c)
            black
            white)))))

(define image->string
  (lambda (img)
    (let* ([rows (image-rows img)]
           [cols (image-cols img)]
           [n (inexact->exact(floor (log rows 2)))])
      (let loop([r (quotient ( - rows (expt 2 n)) 2)]
                [c (quotient ( - cols (expt 2 n)) 2)]
                [n n])
        (if (solid-region? img r c n)
            (if (on-the-dark-side? (image-ref img r c))
                "10"
                "11")
            (let ([change (expt 2 (sub1 n))])
              (string-append "0"
                (loop r c (sub1 n))
                (loop r (+ c change) (sub1 n))
                (loop (+ r change) c (sub1 n))
                (loop (+ r change) (+ c change) (sub1 n)))))))))


;8.
;A procedure longest-path that takes a tree tr and returns a list consisting ;of the data in the tree along the longest root to leaf path.

;I couldn't get this to compile.

;(define longest-path
; (lambda (tr)
;  (define help
;   (lambda (tr)
;    (cond
;     [(empty-tree? tr) '()]
;    [else (cons (root-value tr)
;           (help (left-subtree tr))
;          (help (right-subtree tr))
;         )])))))



;9.
;A procedure seam-cost that takes an energy matrix, a start column, and a ;list of displacements, and returns the cost of the described seam.
(define seam-cost
  (lambda (mat starti colils)
    (let loop ([finish-loop (sub1 (matrix-rows mat))]
               [counter 0] [row-i 0] [col-i starti] [mainls colils])
      (cond
        [(null? mainls) (+ (matrix-ref mat row-i col-i) counter)]
        [else
         (loop finish-loop (+ (matrix-ref mat row-i col-i) counter)
           (add1 row-i) (+ (car mainls) col-i) (cdr mainls))]))))


(define em
  (vov->matrix
    '#(#(9 2 2 4 0 9) #(2 2 2 4 0 0) #(2 2 2 4 4 4) #(6 6 6 6 6 6)
        #(1 1 1 1 1 6) #(1 1 1 1 1 6) #(1 1 9 1 1 6) #(1 1 1 1 1 6)
        #(1 1 1 1 1 6) #(6 6 3 3 3 3) #(6 6 3 3 3 3)
        #(6 6 3 3 9 3))))