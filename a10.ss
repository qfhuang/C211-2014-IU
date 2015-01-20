;;Name - Liam Bolling

;;1. A.
;;a procedure split-randomly that takes a positive integer n and returns a ;;partition of (0 1) into n parts by selecting random numbers that are in ;;the interval (0 1) and then sorting them in increasing order (use ;;insertion sort).
(define pick-one
  (lambda (rel? x y)
    (if (rel? x y)
        x
        y)))

(define insert-in-order
  (lambda (rel? x ls)
    (if (null? ls)
        (cons x ls)
        (if (equal? (pick-one rel? x (car ls)) x)
            (cons x ls)
            (cons (car ls) (insert-in-order rel? x (cdr ls)))))))

(define insertion-sort
  (lambda (rel? ls)
    (if (null? ls)
        '()
        (insert-in-order rel? (car ls) (insertion-sort rel? (cdr ls))))))

(define split-randomly-helper
  (lambda (n ls)
    (let ([randnum (random 1.0)])
      (if (equal? n 1)
          (insertion-sort < (cons 0 ls))
          (split-randomly-helper (sub1 n) (cons randnum ls))))))

(define split-randomly
  (lambda (n)
    (split-randomly-helper n '(1))))



;;1. B.
;;A procedure area-circle-random similar to area-circle except that it uses ;;split-randomly instead of split-equally.

(define area-trapezoid
  (lambda (base1 base2 leg)
    (* leg (/ (+ base1 base2) 2))))

(define circle-y
  (lambda (x)
    (sqrt (- 1 (expt x 2)))))

(define area-circle-random
  (lambda (n)
    (* 4 (area-circle-helper n (split-randomly n)))))

(define area-circle-helper
  (lambda (n ls)
    (if (null? (cdr ls))
        0
        (+ (area-trapezoid (circle-y (car ls)) (circle-y (cadr ls)) (/ 1 n))
          (area-circle-helper n (cdr ls))))))



;;2. A.
;;A procedure sum-digits that takes a number and returns the sum of its ;;digits.
(define sum-digits
  (lambda (n)
    (let ([x (remainder n 10)])
      (if (< n 10)
          x
          (+ x (sum-digits (/ (- n x) 10)))))))




;;2. B.
;;A procedure sum-digits-one that takes a number and calculates the sum of ;;its digits.
(define sum-digits-one
  (lambda (n)
    (let ([sum (sum-digits n)])
      (if (< sum 10)
          sum
          (sum-digits-one sum)))))




;;2. C.
;;A procedure nine-test that takes four numbers representing the dividend, ;;divisor, obtained quotient and obtained remainder of a division which we ;;don't know if its right or wrong.
(define nine-test
  (lambda (divid divis quo rem)
    (let ([D (sum-digits-one divid)] [d (sum-digits-one divis)]
          [q (sum-digits-one quo)] [r (sum-digits-one rem)])
      (let ([E (sum-digits-one (+ (* d q) r))])
        (equal? E D)))))



;;3. A.
;;A procedure merge-adjacent-sequences that takes a binary relation rel? and ;;a list of the form returned by group-sorted-sequences and returns the list ;;after merging adjacent lists together.
(define merge-sort1
  (lambda (rel? ls1 ls2)
    (if (null? ls1)
        (if (not (null? ls2))
            (cons (car ls2) (merge-sort1 rel? ls1 (cdr ls2)))
            '())
        (if (null? ls2)
            (if (not (null? ls1))
                (cons (car ls1) (merge-sort1 rel? (cdr ls1) ls2))
                '())
            (if (rel? (car ls1) (car ls2))
                (cons (car ls1) (merge-sort1 rel? (cdr ls1) ls2))
                (cons (car ls2) (merge-sort1 rel? ls1 (cdr ls2))))))))

(define merge-adjacent-sequences
  (lambda (rel? ls)
    (if (null? ls)
        '()
        (if (null? (cdr ls))
            ls
            (let ([ls1 (car ls)] [ls2 (cadr ls)])
              (cons (merge-sort1 rel? ls1 ls2) (merge-adjacent-sequences rel?
                                                 (cddr ls))))))))


;;3. B.
;;A procedure mergesort that takes a binary relation rel? and and a list ls ;;and returns the result of sorting the elements in ls according to rel? ;;using the Mergesort algorithm described above.
(define group-sorted-sequences
  (lambda (ls)
    (map (lambda (x) (list x)) ls)))

(define merge-sort
  (lambda (rel? ls)
    (if (null? ls)
        ls
        (let ([seq (group-sorted-sequences ls)])
          (if (null? (cdr seq))
              seq
              (merge-helper rel? (merge-adjacent-sequences rel? seq)))))))

(define merge-helper
  (lambda (rel? ls)
    (if (null? (cdr ls))
        ls
        (merge-helper rel? (merge-adjacent-sequences rel? ls)))))




;;4.
;;Write a program that takes a non-negative integer n and returns a list of ;;n random numbers between 0 and 999, inclusive.
(define thunk-1
  (lambda ()
    (insertion-sort < ls4)))

(define thunk-2
  (lambda ()
    (sort < ls4)))

(define thunk-3
  (lambda ()
    (mergesort < ls4)))

(define time-it
  (lambda (thunk)
    (let ([start (cpu-time)])
      (let ([value (thunk)])
        (- (cpu-time) start)))))

(define ls-maker
  (lambda (n)
    (if (zero? n)
        '()
        (cons (random 1000) (ls-maker (sub1 n))))))
;;Results...
;;             insertion-sort      merge-sort         sort
;;ls1  100     1, 0, 0             0, 1, 3            0, 0, 0
;;ls2  1000    48, 43, 43          1, 2, 2            0, 0, 1
;;ls3  5000    1070, 978, 1016     9, 11, 8           2, 1, 1
;;ls4  10000   4021, 3897, 3976    20, 17, 18         2, 3, 3






;;5. A.
;;A procedure pi-leibniz that takes a positive value E and returns the ;;approximation of pi with an error less than E
(define pi-leibniz
  (lambda (E)
    (* 4 (pi-leibniz-helper E 1.0 1))))

(define pi-leibniz-helper
  (lambda (E n neg)
    (if (< (/ 1 n) (/ E 4))
        0
        (+ (/ neg n) (pi-leibniz-helper E (+ 2 n) (* -1 neg))))))


;;5. B.
;;Measure the times needed to compute pi-leibniz and with E = 1e-4.
(define pithunk
  (lambda ()
    (pi-leibniz 1e-4)))
    ;; Answer = 3.1415426535898248 every time.


;;6.
;;A procedure subset-sum? that takes a list ls of non-negative numbers and a ;;target sum (which may be negative), and returns #t if there is some subset ;;of elements from ls that sums to the target, and #f otherwise.
(define subset-sum?
  (lambda (ls sum)
    (if (null? ls)
        (if (= sum 0)
            #t
            #f)
        (if (negative? sum)
            #f
            (if (equal? (car ls) sum)
                #t
                (if (< (car ls) sum)
                    (or (subset-sum? (cdr ls) (- sum (car ls)))
                        (subset-sum? (cdr ls) sum))
                    (subset-sum? (cdr ls) sum)))))))



;;7.
;;A procedure count-shortest-paths that takes two positive integers row and ;;col representing the position of some square on the board, and returns the ;;number of distinct shortest paths between the indicated square and the ;;upper left corner.
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (sub1 n))))))

(define count-shortest-paths
  (lambda (row col)
    (if (and (equal? row 1) (equal? col 1))
        0
        (let ([r (sub1 row)] [c (sub1 col)])
          (/ (factorial (+ r c)) (* (factorial r) (factorial c)))))))




;;8.
;;A procedure height that takes a tree and returns the height of the tree.
(import (c211 tree))
(define tr0 (tree 'a (leaf 5) (empty-tree)))
(define tr1 (tree 4 (empty-tree) (leaf 'b)))
(define tr2 (tree 'd (leaf 'c) (leaf 'e)))
(define tr3 (tree + (leaf 7) (tree - (tree / (leaf 'x) (leaf 5)) (leaf 'y))))
(define tr4 (tree 1
              (tree 2 (empty-tree)
                (tree 3
                  (tree 4 (empty-tree) (leaf 5)) (empty-tree)))
              (tree 2
                (tree 3 (empty-tree) (leaf 4)) (empty-tree))))




; 9. The height of the empty tree is zero. The height of a non-empty tree is the
; number of nodes on the longest root-to-leaf path in the tree. Define a
; procedure height that takes a tree and returns the height of the tree.

(define height
  (lambda (tr)
    (if (empty-tree? tr)
        0
        (if (leaf? tr)
            1
            (if (< (height (right-subtree tr)) (height (left-subtree tr)))
                (add1 (height (left-subtree tr)))
                (add1 (height (right-subtree tr))))))))




;;10.
;;A predicate leaf-member? that takes an item and a binary tree and searches ;;through the tree to determine whether the given item is stored at a leaf ;;of the tree.
(define leaf-member?
  (lambda (item tr)
    (cond
      [(empty-tree? tr) #f]
      [(leaf? tr) (if (equal? item (root-value tr)) #t #f)]
      [else
       (or (leaf-member? item (right-subtree tr))
           (leaf-member? item (left-subtree tr)))])))





;;11.
;;A procedure random-walk that takes a tree and returns a list of values ;;along some root to leaf path in the tree.
(define random-walk
  (lambda (tr)
    [cond
      [(empty-tree? tr) '()]
      [(empty-tree? (left-subtree tr))
       (cons (root-value tr) (random-walk (right-subtree tr)))]
      [(empty-tree? (right-subtree tr))
       (cons (root-value tr) (random-walk (left-subtree tr)))]
      [(< (random 1.0) .5) (cons (root-value tr)
                             (random-walk (left-subtree tr)))]
      [else (cons (root-value tr) (random-walk (right-subtree tr)))]]))




;;12.
;;A predicate same-shape? that takes two trees and returns #t if they have ;;exactly the same shape and #f otherwise.
(define same-shape?
  (lambda (tr1 tr2)
    (cond
      [(and (empty-tree? tr1) (empty-tree? tr2)) #t]
      [(or (and (empty-tree? tr1) (not (empty-tree? tr2)))
           (and (not (empty-tree? tr1)) (empty-tree? tr2))) #f]
      [else (and
                 (same-shape? (left-subtree tr1) (left-subtree tr2))
                 (same-shape? (right-subtree tr1) (right-subtree tr2)))])))