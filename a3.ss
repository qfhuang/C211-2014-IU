;;Name - Liam Bolling

;; 1 - A
;; What it does - Outputs a random number between 1 - 5.
(define spin-roulette
    (lambda ()
        (+ (random 5) 1)))

;; 1 - B
;; What it does - Rabdomly picks a ball out of a group of 100 potential balls
;;that are red and blue.
(define pick-ball
    (lambda ()
         (cond
           [(> (random 100) 50) "red"]
           [else "blue"])))

;; 2
;; What it does - Returns what you give it.
(define echo
  (lambda (x)
     x))

;; 3
;; What it does - Returns the opposite.
(define toggle
    (lambda (x)
         (cond
           [(equal? x 'l) "o"]
           [else "l"])))


;; 4 - A
;; What it does - Adds bits together wihtout carrying.
(define add-bit-no-carry
    (lambda (bit1 bit2)
      (cond
         [(and (equal? bit1 'l) (equal? bit2 'l))  'o ]
         [(and (equal? bit1 'o) (equal? bit2 'l))  'l ]
         [(and (equal? bit1 '1) (equal? bit2 'o))  'l ]
         [(and (equal? bit1 'o) (equal? bit2 'o))  'o ]
         [else "Error"])))

;; 4 - B
;; What it does - This one confused me and a lot of people on the help center.
(define add-bit-carry
    (lambda (x)
      x))


;; 5
;; What it does - Checks if first is less than second.
(define lt-bit?
    (lambda (bit1 bit2)
      (cond
           [(and (equal? bit1 'o) (equal? bit2 'l)) #t]
           [else #f])))

;; 6
;; What it does - Checks if numbers are in order.
(define pythagorean?
    (lambda (x y z)
      (cond
           [(and (equal? bit1 'o) (equal? bit2 'l)) #t]
           [else #f])))


;; 7
;; What it does - Sees if you are within the circle.
(define in-circle?
    (lambda (x y)
       (cond
         [(<= (sqrt (+ (* x x) (* y y))) 1) #t ]
         [else #f])))


;; 8 A+B
;; What it does - Finds the smallest and smaller numbers.
(define smaller
    (lambda (x y)
       (cond
         [(<= x y) x ]
         [else y])))

(define smallest
    (lambda (x y z)
       (smaller (smaller x y) z)))


;; 9
;; What it does - Returns the median number.
(define median
    (lambda (x y z)
      (cond
       [(and (< x y) (< y z)) y]
       [(and (> x y) (> y z)) y]
       [(and (< y x) (< x z)) x]
       [(and (> y x) (> x z)) x]
       [else z])))


;; 10
;; What it does - Makes the number odd.
(define make-odd
    (lambda (n)
      (cond
       [(equal? (odd? n) #t) n]
       [else (+ n 1)])))

;; 11 - A
;; What it does - Sees if either number is odd.
(define either-odd?
    (lambda (x y)
      (cond
       [(equal? (odd? x) #t) #t]
       [else
        (cond
          [(equal? (odd? y) #t) #t]
          [else #f])
        ])))


;; 11 - B
;; What it does - Sees if both numbers are odd.
(define both-odd?
    (lambda (x y)
      (cond
       [(equal? (odd? x) (odd? y)) #t]
       [else #f])))


;; 12
;; What it does - Does whatever the Collatz sequence does. Makes no sense to me.
(define next-collatz
    (lambda (x)
      (cond
       [(equal? (odd? x) #t) (+ (* x 3) 1)]
       [else (* 0.5 x)])))