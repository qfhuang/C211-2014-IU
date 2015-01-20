;;Name - Liam Bolling

(import (c211 image))

;1.
;A procedure image-distance that takes two equal-size images and returns their
;distance.
(define image_distance_helper
  (lambda (ls1 ls2 numberOfSim)
    (cond
      [(null? ls1) numberOfSim]
      [(equal? (car ls1) (car ls2)) (image_distance_helper (cdr ls1)
                                      (cdr ls2) numberOfSim)]
      [else (image_distance_helper (cdr ls1) (cdr ls2) (add1 numberOfSim))])))
(define image-distance
  (lambda (img_1 img_2)
    (image_distance_helper (image->list img_1) (image->list img_2) 0)))



;2. A.
;A procedure divide that takes a predicate pred? and a list ls and returns a
;list with two sublists: those elements in ls that satisfy the predicate and
;those that don't.
(define divide_helper
  (lambda (ls_origin ls1 ls2 pred?)
    (cond
      [(null? ls_origin) (list ls1 ls2)]
      [(pred? (car ls_origin)) (divide_helper (cdr ls_origin) (cons
                                         (car ls_origin) ls1) ls2 pred?)]
      [else (divide_helper (cdr ls_origin) ls1
              (cons (car ls_origin) ls2) pred?)])))
(define divide
  (lambda (pred? ls)
    (divide_helper ls '() '() pred?)))




;Draft of Join
#|
(define join
  (lambda (ls1 goo ls2)
    (cond
      [(and (null? ls1) (not (null? ls2))) (join ls2 goo ls1)]
      [(and (and (null? ls1) (not (null? goo))) (and (null? ls2)))
       (cons goo '())]
      [(null? ls2) '()]
      [(null? goo) (cons (car ls2) (join ls1 goo (cdr ls2)))]
      [(null? ls1) (cons goo (join ls1 '() ls2))]
      [else (cons (car ls1) (join (cdr ls1) goo ls2))])))
|#

;2. B.
;A procedure join that takes a list ls1, an item goo, and another list ls2,
;and returns a list consisting of the elements of ls1 followed by goo followed
;by the elements of ls2. (Do not use append.)
(define join
  (lambda (ls1 goo ls2)
    (cond
      [(null? ls1) (cons goo ls2)]
      [else (cons (car ls1) (join (cdr ls1) goo ls2))])))



;2. C.
;A procedure quicksort that takes a binary relation rel? and a list
;ls and returns the result of sorting the elements in ls according to rel?
;using the Quicksort algorithm described above.
(define quicksort
  (lambda (rel? ls)
    (cond
      [(or (null? ls) (null? (cdr ls))) ls]
      [else
       (let ([divide_con (divide (lambda (x) (rel? x (car ls))) (cdr ls))])
         (join (quicksort rel? (car divide_con))
           (car ls)
           (quicksort rel? (cadr divide_con))))])))



(define euclidean-distance
  (lambda (p q)
    (sqrt
      (+ (expt (- (car p) (car q)) 2) (expt (- (cadr p) (cadr q)) 2)))))
(define training-data
  '((d (1 8)) (d (2 9)) (d (8 10)) (d (4 2)) (r (1 3)) (r (2 1)) (r (4 8))
    (r (6 4))
    (d (7 3)) (r (1 5)) (d (1 9)) (d (6 2)) (r (10 9)) (d (7 7))
    (d (5 11)) (r (1 1)) (r (0 9)) (r (12 12)) (r (20 30))))



;3. A
;A procedure taxicab-distance that takes two points and returns the sum of
;the absolute differences of their coordinates.
(define taxicab-distance
  (lambda (ls1 ls2)

    (+ (abs (- (car ls1) (car ls2))) (abs (- (car (cdr ls1)) (car (cdr ls2)))))))




;3. B.
;A procedure knn that takes a positive integer k, the coordinates of a house
;whose political affiliate we're trying to classify, a distance function,
;and the training data, and returns a list of the (at most) k nearest
;neighbors to the given house in the training data.
(define knn
  (lambda(k ls distance training-data)
    (let loop([n k] [lsa (knn_helper_killme k ls distance training-data '())])
      (cond
        [(zero? n) '()]
        [else (cons (car lsa) (loop (sub1 n) (cdr lsa)))]))))

(define knn_helper
  (lambda(k ls distance training-data ls1)
    (cond
      [(null? training-data) ls1]
      [else (cons (distance (cadar training-data) ls) (knn_helper k ls distance
                                                        (cdr training-data)
                                                        ls1))])))
(define knn_helper_killme
  (lambda(k ls distance training-data ls2)
    (let loop([ld (quicksort < (knn_helper k ls distance training-data '()))]
              [lsx training-data])
      (cond
        [(null? ld) ls2]
        [(null? lsx) (loop ld training-data)]
        [(equal? (car ld) (distance (cadar lsx) ls)) (cons (car lsx)
                                                       (loop (cdr ld)
                                                         (cdr lsx)))]
        [else (loop ld (cdr lsx))]))))



;3. C.
;A procedure majority that takes a non-empty list of labeled data and a
;non-empty list of labels, and returns the label that occurs most frequently.
(define majority
  (lambda(ls1 ls2)
    (cond
      [(equal? (car (quicksort > (majority_helper_helper ls1 ls2 '())))
         (majority_helper ls1 (car ls2) 0)) (car ls2)]
      [else (majority ls1 (cdr ls2))])))

(define majority_helper
  (lambda(ls1 x n)
    (cond
      [(null? ls1) n]
      [(equal? x (caar ls1)) (add1 (majority_helper (cdr ls1) x n))]
      [else (majority_helper (cdr ls1) x n)])))

(define majority_helper_helper
  (lambda(ls1 ls2 ls3)
    (cond
      [(null? ls2) ls3]
      [else (cons (majority_helper ls1 (car ls2) 0) (majority_helper_helper
                                                      ls1 (cdr ls2) ls3))])))




;3. D.
;a procedure classify that takes an odd positive integer k, the coordinates
;of a new house, a procedure that measures the distance between two houses,
;a list of classes, and some training data, and applies the K-Nearest Neighbors
;algorithm to predict the party affiliation of the new house.
(define classify
  (lambda (k house_point procedure labels data)
    (majority (knn k house_point procedure data) labels)))




;4. A.
;a procedure read-digit that takes a string representing the full path to the
;directory containing your mnist folder of images and an integer in the range
;1 to 10000, inclusive, and returns the result of reading the corresponding
;image in mnist.
(define read-digit
  (lambda (path int)
    (read-image (string-append path "/" (number->string int) ".png"))))




;4. B.
;A procedure read-mnist that takes a string representing the full path to
;the mnist directory and a list of labeled digit data, and returns the list
;with the image number replaced by the corresponding mnist image.

;;Warning: This made no sense to me
(define read-mnist
  (lambda (path ls)
    (cond
      [(null? ls) '()]
      [else (cons (cons (caar ls) (list (read-digit path (cadar ls))))
              (read-mnist path (cdr ls)))]
      )))



;4. C.
;A procedure reduce that takes an association list such as tiny-set and
;returns the result of replacing the images with a list of grayscale values.
(define reduce
  (lambda (ls)
    (map (lambda (x) (list (car x))
           (map (lambda (col) (color-ref col 'red)) (image->list
                                                      (cadr x)))) ls)))




;4. D.
;A procedure digit-distance that takes two lists of integers representing the
;grayscale values in two digit images, and returns the sum of the absolute
;differences of their values. This might not workk.. :)
(define digit-distance
  (lambda (ls1 ls2)
    (cond
      [(or (null? ls1) (null? ls2)) 0]
      [else (+ (+ (car ls1) (car ls2))(digit-distance (cdr ls1) (cdr ls2)))])))




;4. E.
;A procedure side-by-side that takes an association list of new test data
;(where each element is a two-element list containing the known label and
;a list of grayscale values) and an association list of training data.
(define side-by-side
  (lambda (new training)
    (map (lambda (x) (cons (car x)
                       (list (classify 3 (cadr x) digit-distance
                               (iota 10) training)))) new)))



;4. F.
;A procedure accuracy that takes a list of the form returned by side-by-side
;and returns the percentage of matching entries. This def. doesn't work right..
;Partial credit? :)
(define accuracy
  (lambda (ls)
    (let loop
      ([x ls] [matched 0] [checked 0])
      (if (null? x)
          (* (/ matched checked) 100)
          (if (equal? (caar ls) (cadar ls))
              (loop (cdr x) (add1 matched) (add1 checked))
              (loop (cdr x) matched (add1 checked)))))))

;4. G.
;I honestly do not understand what the question is. Help...