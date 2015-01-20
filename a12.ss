;;Name - Liam Bolling

;Import because I already know we need this.
(import (c211 image) (c211 tree))

;1. Right
(define right
  (lambda (pt1 pt2)
    (cond
      [(> (car pt1) (car pt2)) #t]
      [else #f])))


;1. Right-most
(define right-most
  (lambda (ls)
    (sort (lambda (x) (cond [(car x) #t] [else #f])) '((1 2)(3 4)(6 7)(1 3)))))



;1. Count-non-leaves
(define count-non-leaves
  (lambda (tr)
    (if (empty-tree? tr)
        0
        (if (leaf? tr)
            (begin
              (count-non-leaves (left-subtree tr))
              (count-non-leaves (right-subtree tr)))
            (add1 (+
                    (count-non-leaves (left-subtree tr))
                    (count-non-leaves (right-subtree tr))))))))

;1. Two Leaves
(define two-leaves
  (lambda (tr)
    (if (empty-tree? tr)
        tr
        (if (and (empty-tree? (left-subtree tr)) (leaf? (right-subtree tr)))
            (let [(rval (root-value (right-subtree tr)))]
              (tree (root-value tr) (leaf rval) (leaf rval))
              (if (and (leaf? (left-subtree tr))
                       (empty-tree? (right-subtree tr)))
                  (let [(lval (root-value (left-subtree tr)))]
                    (tree (root-value tr) (leaf lval) (leaf lval))
                    (tree (root-value tr) (two-leaves (left-subtree tr))
                      (two-leaves (right-subtree tr))))))))))



;2. A.
;A procedure sum-index-list that takes a vector of numbers vec, a list ls of ;valid indices for vec, and returns the sum of the elements in vec with ;indices in ls.
(define sum-index-list
  (lambda (vec ls)
    (cond
      [(null? ls) 0]
      [else (+ (vector-ref vec (car ls)) (sum-index-list vec (cdr ls)))])))



;2. B.
;a procedure sum-index-vector that takes a vector of numbers vec, a vector v ;of valid indices for vec, and returns the sum of the elements in vec with ;indices in v.
(define sum-index-vector
  (lambda (vec vec_2)
    (define help
      (lambda (vec vec_2 vec_length)
        (cond
          [(< vec_length 0) 0]
          [else (+ (vector-ref vec (vector-ref vec_2 vec_length)) (help vec vec_2 (sub1 vec_length)))])))
    (help vec vec_2 (sub1 (vector-length vec_2)))))



;3.
;A procedure char-count that takes a string and a character, and returns the ;number of occurrences of the character in the string.
(define char-count
  (lambda (charSet origChar)
    (define help
      (lambda (charSet origChar lengthCounter)
        (cond
          [(< lengthCounter 0) 0]
          [(char=? (string-ref charSet lengthCounter) origChar)
           (add1 (help charSet origChar (sub1 lengthCounter)))]
          [else (help charSet origChar (sub1 lengthCounter))])))
    (help charSet origChar (sub1 (string-length charSet)))
    ))



;4.
;A procedure vector-combine! that takes a vector v1, a procedure of two ;arguments, and a vector v2 and destructively updates v1 to contain the ;result of applying proc to corresponding elements of v1 and v2.
(define v (vector 1 2 3 4 5 6))

(define vector-combine!
  (lambda (vec1 prec vec2)
    (define help
      (lambda (vec1 prec vec2 vecLength)
        (cond
          [(< vecLength 0) vec1]
          [else
           (vector-set! vec1 vecLength (prec (vector-ref vec1 vecLength) (vector-ref vec2 vecLength)))
           (help vec1 prec vec2 (sub1 vecLength))])))
    (help vec1 prec vec2 (sub1 (vector-length vec1)))))





;5. A.
;A procedure image-copy that takes an image and returns a new image which is ;an exact copy of the given image.
(define image-copy
  (lambda (img)
    (make-image (image-cols img) (image-rows img) (lambda (r c) (image-ref img r c)))))




;5. B.
;A procedure image-transpose that takes an image and returns the image ;created by interchanging the rows and columns of the given image.
(define image-transpose
  (lambda (img)
    (make-image (image-cols img) (image-rows img)
      (lambda (r c)
        (image-ref img c r)))))



;5. C.
;A procedure image-crop that takes an image and a non-negative integer n, and ;returns the image that results from cropping the rightmost n columns from ;the given image.
(define image-crop
  (lambda (img n)
    (let ( [rows (image-rows img)]
          [cols (max 0 (- (image-cols img) n))] )
      (if (zero? cols)
          (make-image rows 0
            (lambda (r c)
              (image-ref img r c)))
          (make-image rows cols
            (lambda (r c)
              (image-ref img r c)))))))



;6. A.
;A procedure concatenate-h that takes two images im1 and im2, and returns the ;image formed by concatenating the two images from left to right.
(define concatenate-h
  (lambda (img1 img2)
    (let ((row-min (min (image-rows img1) (image-rows img2)))
          (combine (+ (image-cols img1) (image-cols img2))))
      (make-image row-min combine
        (lambda (r c)
          (cond
            [(< c (image-cols img1)) (image-ref img1 r c)]
            [else (image-ref img2 r (- c (image-cols img1)))]))))))




;6. B.
;A procedure concatenate-v that takes two images im1 and im2, and returns the ;image formed by concatenating the given images from top to bottom.
(define concatenate-v
  (lambda (img1 img2)
    (let ([col-min (min (image-cols img1) (image-cols img2))]
          [combine (+ (image-rows img1) (image-rows img2))])
      (make-image combine col-min
        (lambda (r c)
          (cond
            [(< r (image-rows img1)) (image-ref img1 r c)]
            [else (image-ref img2 (- r (image-rows img1)) c) ]))))))



;6. C.
;A procedure reduce that takes an image and returns a reduced image of the ;original.
(define reduce
  (lambda (img)
    (let ([new-width (quotient (image-cols img) 2)]
          [new-height (quotient (image-rows img) 2)])
      (make-image new-height new-width
        (lambda (r c)
          (image-ref img (* r 2) (* c 2)))))))



;6. D.
;A procedure image-fractal that takes an image and a non-negative integer n.
(define image-fractal
  (lambda (img n)
    (if (zero? n)
        img
        (concatenate-v (concatenate-h (reduce img)
                         (image-fractal (reduce img) (sub1 n)))
          (concatenate-h (reduce img) (reduce img))))))



;7. A.
;A procedure brightness that takes a color and returns the sum of the values ;on the three channels.
(define brightness
  (lambda (c)
    (+ (color-ref c 'green) (+ (color-ref c 'red) (color-ref c 'blue)))))


;7. B.
;A procedure energy that takes an image and two integers representing the row ;and column indices of some pixel on the image, and returns the energy of the ;indicated pixel.
(define energy
  (lambda (img x y)

    (define chick-fil-a
      (lambda (x y img)
        (cond
          [(< x 0) 0]
          [(> x (image-cols img)) 0]
          [(< y 0) 0]
          [(> y (image-rows img)) 0]
          [else (brightness (image-ref img x y))])))

    (let [(xenergy (+
                     (chick-fil-a (sub1 x) (add1 y) img)
                     (* (chick-fil-a (sub1 x) y img) 2)
                     (chick-fil-a (sub1 x) (sub1 y) img)
                     (-
                       (chick-fil-a (add1 x) (add1 y) img)
                       (* (chick-fil-a (add1 x) y img) 2)
                       (chick-fil-a (add1 x) (sub1 y) img)
                       )))
          (yenergy (+
                     (chick-fil-a (sub1 x) (add1 y) img)
                     (* (chick-fil-a x (add1 y) img) 2)
                     (chick-fil-a (add1 x) (add1 y) img)
                     (-
                       (chick-fil-a (sub1 x) (sub1 y) img)
                       (* (chick-fil-a x (sub1 y) img) 2)
                       (chick-fil-a (add1 x) (sub1 y) img)
                       )))]

      (sqrt (+ (* (xenergy) (xenergy)) (* (yenergy) (yenergy)))))))




;9.
;A procedure column-energy that takes an image and a column index, and ;returns the sum of all pixel energies in the given column.
(define column-energy
  (lambda (img col)
    (let ([row (image-rows img)])
      (let loop ([r 0])
        (if (< r row)
            (+ (energy img r col) (loop (add1 r)))
            0)))))




;10
;A procedure least-energy-column that takes a non-empty image and computes ;the column with minimal energy.
;Note: Energy doesn't work correctly/tested on friends energy
(define least-energy-column
   (lambda (img)
    (let ([col (image-cols img)] [v (make-vector 2)])
       (let loop
         ([counter 0] [sm-e (column-energy img 0)] [sm-i 0])
         (if (< counter col)
             (if (<= sm-e (column-energy img counter))
                 (loop (add1 counter) sm-e sm-i)
                 (loop (add1 counter) (column-energy img counter)
                   counter))
             (begin
               (vector-set! v 0 sm-i)
               (vector-set! v 1 sm-e)
               v))))))