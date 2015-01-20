;;1.
;;A procedure clock-distances that takes two different integers n and m
;;from 1 to 12 representing clock positions and returns a two-element list.
(define clock-distances-helper
  (lambda (n m difference)
    (cond
      [(equal? n m) difference]
      [(equal? n 12) (clock-distances-helper 1 m (add1 difference))]
      [else (clock-distances-helper (add1 n) m (add1 difference))])))
(define clock-distances
  (lambda (n m)
    (cons (clock-distances-helper n m 0)
      (cons (clock-distances-helper m n 0) '()))))




;;2.
;;A procedure leet-speak takes a string and returns the result of
;;changing all s's to fives, all (lowercase) e's to threes, all l's to ones,
;;and all o's to zeros.
(define leet-speak
  (lambda (ls)
    (list->string (map (lambda (x)
           (cond
             [(equal? x #\s) #\5 ]
             [(equal? x #\e) #\3 ]
             [(equal? x #\l) #\1 ]
             [else x])) (string->list ls)))))




;;3.
;;A procedure encrypt that takes three strings: a message to be encrypted
;;and two alphabets, which we will call regular and encrypted.
(define encrypt-helper
  (lambda (char alpha1 alpha2)
    (cond
      [(null? alpha1) char]
      [(equal? char (car alpha1)) (car alpha2)]
      [else (encrypt-helper char (cdr alpha1) (cdr alpha2))])))
(define encrypt
  (lambda (message alpha1 alpha2)
    (let ([alpha-convert (string->list alpha1)][alpha-convert-2
                                                 (string->list alpha2)])
    (list->string (map (lambda (x)
   (encrypt-helper x alpha-convert alpha-convert-2)) (string->list message))))))




;;4. A.
;;A predicate prime? that takes an integer greater than or equal to two and
;;returns #t if and only if the number is a prime.
(define prime?
  (lambda (x)
    (prime?-helper x 2)))
(define prime?-helper
  (lambda (x y)
    (cond
      [(equal? x y) #t]
      [(integer? (/ x y)) #f]
      [else (prime?-helper x (add1 y))])))



;;4. B.
;;A procedure primes that takes an integer n and returns a list of the first
;;n prime numbers.
(define primes
  (lambda (x)
    (primes-helper x 2 0)))
(define primes-helper
  (lambda (x y counter)
    (cond
      [(equal? counter x) '()]
      [(equal? (prime? y) #t) (cons y ( primes-helper x (add1 y)
                                       (add1 counter)))]
      [else ( primes-helper x (add1 y) counter)])))



;;5. A.
;;A predicate cong? that takes two nonnegative integers n and m and a positive
;;integer clock-size and returns #t if and only if the two numbers are the same
;;in a clock of size clock-size.
(define cong?-helper
  (lambda (m clock-size)
    (cond
      [(< m clock-size) m]
      [else (cong?-helper (- m clock-size) clock-size)])))
(define cong?
  (lambda (n m clock-size)
    (let ([real-size (cong?-helper m clock-size)])
    (cond
      [(equal? real-size n) #t]
      [else #f]))))



;;5. B.
;;A procedure exp-clock that takes a positive integer base, a nonnegative
;;integer n and a positve integer clock-size and returns the result of raising
;;the integer base to the power n in a clock of size clock-size.
(define exp-clock-helper
  (lambda (x y)
    (cond
      [(equal? y 1) x]
      [else (* x (exp-clock-helper x (- y 1)))])))
(define exp-clock
  (lambda (base n clock-size)
    (cong?-helper (exp-clock-helper base n) clock-size)))




;;5. C.
;;A procedure fermat-test that tests whether a number is prime or not.
(define fermat-test
  (lambda (base prime?^)
    (let ([clock-thing-that-i-dont-understand (exp-clock base prime?^ prime?^)])
      (cond
        [(equal? base clock-thing-that-i-dont-understand) #t]
        [else #f]))))




;;6.
;;A procedure recover-barcode that takes a three-element list of sensor
;;data and returns the corresponding barcode.
(define recover-barcode-helper
  (lambda (ls1 ls2 ls3)
    (cond
      [(null? ls1) '()]
      [(< (/ (+ (car ls1) (+ (car ls2) (car ls3))) 3) 0.5) (cons 0
                       (recover-barcode-helper (cdr ls1) (cdr ls2) (cdr ls3)))]
      [else (cons 1 (recover-barcode-helper (cdr ls1) (cdr ls2) (cdr ls3)))])))
(define recover-barcode
  (lambda (ls)
    (recover-barcode-helper (car ls) (car (cdr ls)) (car (cddr ls)))))




;;7.
;;A procedure add-parity-bit that takes a list of bits and returns the
;;list with one additional bit in the last position such that the resulting
;;sequence has even parity.
(define add-parity-bit-helper-helper
  (lambda (ls)
     (cond
       [(null? ls) 0]
       [(equal? (car ls) 1) (add1 (add-parity-bit-helper-helper (cdr ls)))]
       [else (add-parity-bit-helper-helper (cdr ls))])))
(define add-parity-bit-helper
  (lambda (ls ls2)
    (cond
      [(null? ls)
       (cond
         [(even? (add-parity-bit-helper-helper ls2)) (cons 0 '())]
         [else (cons 1 '())])]
      [else (cons (car ls) (add-parity-bit-helper (cdr ls) ls2))])))
(define add-parity-bit
  (lambda (ls)
    (add-parity-bit-helper ls ls)))



;;8.
;;A procedure pick-one that takes a relational predicate rel? and two
;;items, x and y, and returns either x or y.
(define pick-one
  (lambda (rel? x y)
    (cond
      [(rel? x y) x]
      [else y])))




;;9. A.
;;A procedure insert-in-order that takes a relation, an item x, and a list
;;of elements that are sorted by the given relation, and returns the result
;;of inserting x into the correct location in the given list such that the
;;result is in sorted order.
(define insert-in-order-helper
  (lambda (pred x ls n)
    (cond
      [(null? ls) '()]
      [(and (equal? 0 n) (pred x (car ls))) (cons x (cons (car ls)
                          (insert-in-order-helper pred x (cdr ls) (add1 n))))]
      [else (cons (car ls) (insert-in-order-helper pred x (cdr ls) n))])))
(define insert-in-order
  (lambda (pred x ls)
    (insert-in-order-helper pred x ls 0)))




;;9. B.
;;A procedure insertion-sort that takes a relation and a list of elements,
;;and returns the result of sorting the list according to the given relation.
(define insertion-sort
  (lambda (pred ls ls2)
    (cond
      [(null? ls) ls2]
      [else (insertion-sort pred (cdr ls) (insert-in-order pred
                                            (car ls) ls2))])))