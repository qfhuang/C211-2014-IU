;;Name - Liam Bolling

;These are required
(import (c211 image))
(import (c211 matrix))


(define image-copy
  (lambda (img)
    (make-image (image-cols img) (image-rows img) (lambda (r c) (image-ref img r c)))))

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



#|
(define img
  (make-image
    200
    300
    (lambda (r c)
      (let ([d (max (abs (- r 100)) (abs (- c 150)))])
        (if (even? (inexact->exact (floor (/ d 10))))
            orange
            black)))))
(define mat (make-energy-matrix img))
(list (matrix-rows mat) (matrix-cols mat))
(200 300)

(define em1
  (vov->matrix '#(#(7 2 1 3) #(2 3 1 2) #(1 4 5 1))))
|#


;1. A.
;A constructor procedure infinity that takes no arguments and returns the ;chosen representation of infinity.
(define infinity
  (lambda ()
    'oo))


;1. B.
;A predicate infinity? that takes an item x, and returns #t if x corresponds to our representation of infinity, and #f otherwise.
(define infinity?
  (lambda (x)
    (cond
      [(equal? x 'oo) #t]
      [else #f])))


;1. C.
;A procedure :+ that takes two inputs and returns their sum.
(define :+
  (lambda (x y)
    (cond
      [(or (infinity? x) (infinity? y)) 'oo]
      [else (+ x y)])))


;1. D.
;A predicate :< that takes two inputs and returns #t if the first is less ;than the second, and #f otherwise, as shown below.
(define :<
  (lambda (x y)
    (cond
      [(infinity? x) #f]
      [(< x y) #t]
      [else #f])))


;2.
;A procedure highlight-seam! that takes an image, a list of column ;indices ;representing a vertical seam, and a color, and destructively ;mutates the ;image by replacing all the pixels along the seam with the ;specified ;highlight color.

;Doesn't compile correctly

;(define highlight-seam!
  ;(lambda (img ls clr)
    ;(let ([rows (image-rows img)]
          ;[cols (image-cols img)])
      ;(let loop
        ;(let ([r 0]
                 ;[lsx ls])
        ;(if (null? lsx)
            ;img
            ;(begin
              ;(image-set! img r (car lsx) clr)
              ;(loop (add1 r) (cdr lsx)))))))))


;3. A.
;A procedure make-energy-matrix that takes an image and returns a matrix of ;the same dimensions, where each entry in the matrix is a non-negative real ;number representing the energy of the corresponding pixel in the given ;image.
(define make-energy-matrix
  (lambda (img)
    (matrix-generator (image-rows img) (image-cols img) (lambda (r c) (energy img r c)))))





;3. B.
;A procedure matrix-map that takes a procedure of one argument and a matrix, ;and returns a new matrix of the same dimensions as the given matrix and ;where each element is the result of applying the given procedure on the ;corresponding element in the given matrix.
(define matrix-map
  (lambda (proc mtrx)
    (let [(rows (matrix-rows mtrx)) (cols (matrix-cols mtrx))]
      (let ((new-matrix (make-matrix rows cols)))
        (define the_loop
          (lambda (r c)
            (cond
              [(equal? r rows) (the_loop 0 (add1 c))]
              [else
               (begin
                 (matrix-set! new-matrix r c (proc (matrix-ref mtrx r c)))
                 (the_loop (add1 r) c))])))))))



;3. C.
;A procedure matrix-copy that takes a matrix and returns a separate copy of the given matrix.
(define matrix-copy
  (lambda (mtrx)
    (define copy-help
      (lambda (mtrx) mtrx))
    (matrix-map copy-help mtrx)))




;3. D.
;A procedure cropped-matrix-ref that takes a matrix, a row index, a column index, and a maximum width n.
(define cropped-matrix-ref
  (lambda (mtrx r-index c-index n)
    (cond
      [(negative? r-index) (infinity)]
      [(negative? c-index) (infinity)]
      [(>= c-index n) (infinity)]
      [else (matrix-ref mtrx r-index c-index)])))




;4. A procedure best-direction that takes three costs and returns the best ;direction for the seam to take, based on the given costs.




;5. a procedure seam-origin that takes a matrix of costs and a positive ;integer representing the virtual width, and returns the column ;corresponding to the smallest cost.
(define (seam-origin mat width)
  (let loop ([i 1] [ls (list 0 (matrix-ref mat 0 0))])
    (cond
      [(= i (matrix-cols mat)) (car ls)]
      [(= i width) (car ls)]
      [(:< (matrix-ref mat 0 i) (cadr ls))
       (loop (add1 i) (list i (matrix-ref mat 0 i)))]
      [else
       (loop (add1 i) ls)])))



;6. A procedure walk-seam that takes a seam matrix, sm, and a starting ;column, c, and returns the seam in sm corresponding to the given column.
(define (walk-seam sm c)
  (let loop ([r 0] [newc c])
    (let ([next (matrix-ref sm r newc)])
      (cond
        [(= r (sub1 (matrix-rows sm))) (list newc)]
        [else
         (cons newc (loop (add1 r) next))]))))



;7. A procedure least-energy-seam that takes an energy matrix and a positive ;integer n, and returns a minimal cost vertical seam restricted to the first ;n columns of the given matrix.
(define (least-energy-seam em n)
  (let ([rows (matrix-rows em)]
        [cols (matrix-cols em)])
    (let loop ([cm (matrix-copy em)]
               [sm (make-matrix rows cols)]
               [row-i (- rows 2)]
               [col-i 0])
      (let
        ([left (cropped-matrix-ref cm (add1 row-i) (sub1 col-i) n)]
         [below (cropped-matrix-ref cm (add1 row-i) col-i n)]
         [right (cropped-matrix-ref cm (add1 row-i) (add1 col-i) n)])
        (matrix-set! cm row-i col-i
          (+ (matrix-ref cm row-i col-i)
            (matrix-ref cm (add1 row-i)
              (+ (best-direction left below right) col-i))))
        (matrix-set! sm row-i col-i (+ (best-direction left below right) col-i))
        (cond
          [(and (zero? row-i) (equal? col-i (sub1 n)))
           (walk-seam sm (seam-origin cm n))]
          [(zero? row-i)
           (loop cm sm row-i (add1 col-i))]
          [(= col-i (sub1 n))
           (loop cm sm (sub1 row-i) 0)]
          [else
           (loop cm sm row-i (add1 col-i))])))))



;8. A procedure image-seam-carve! that takes an image, a seam, and a ;positive integer n, and destructively mutates the image by carving out the ;given seam.
(define (image-seam-carve! img seam n)
  (let ([rows (image-rows img)])
    (let loopr ([rowi 0] [seamls seam])
      (when (< rowi rows)
        (let loopc ([coli (car seamls)])
          (when (< coli (sub1 n))
            (image-set! img rowi coli (image-ref img rowi (add1 coli)))
            (loopc (add1 coli))))
        (loopr (add1 rowi) (cdr seamls))))))



;9. A procedure content-aware-resize that takes an image and a non-negative ;integer pixels, and returns a new image formed by carving pixels vertical ;seams from the given image
(define (content-aware-resize img-orig pixels)
  (let ([img (image-copy img-orig)])
    (let loop ([n (image-cols img)])
      (let ([em (make-energy-matrix img)])
        (if (> n (- (image-cols img) pixels))
        (begin
           (image-seam-carve! img (least-energy-seam em n) n)
           (loop (sub1 n)))
            (image-crop img pixels))))))