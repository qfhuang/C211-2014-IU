;;Name - Liam Bolling


;; Problem - Define a procedure add3 that takes a number x and returns the
;result of adding three to x.
;; What it does - This will take any number and add 3 to it.
(define add3
    (lambda (x)
        (+ 3 x)))


;; Problem - Define a procedure subtract3 that takes a number x and returns
;;the result of subtracting three from x.
;; What it does - This will take your number and subtract 3 from it.
(define subtract3
    (lambda (x)
        (- x 3)))


;; Problem - Define a procedure thousands that takes a positive integer
;;and returns the thousands digit Hint: use the procedures
;;quotient and remainder.
;; What it does - Finds the thousands digit from a given number.
(define thousands
    (lambda (x)
        (quotient (remainder x 10000) 1000)))


;; Problem - Define a procedure ten-magnitude that takes a number and returns
;;ten times its magnitude as shown below. Hint: use the abs procedure.
;; What it does - Turns a number into a positive number and multiplies it by 10.
(define ten-magnitude
    (lambda (x)
        (abs (* x 10))))


;; Problem - Define a procedure ten-magnitude that takes a number and returns
;;ten times its magnitude as shown below. Hint: use the abs procedure.
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define square
    (lambda (x)
        (* x x)))

(define add-squares
    (lambda (x y)
        (+ (square x) (square y))))


;; 4 - A
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define face-diagonal
    (lambda (x)
        (sqrt (+ (* x x)(* x x)))))


;; 4 - B
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define body-diagonal
    (lambda (x)
        (sqrt (+ (* (face-diagonal x) (face-diagonal x))(* x x)))))

;; 4 - B
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define body-diagonal
    (lambda (x)
        (sqrt (+ (* (face-diagonal x) (face-diagonal x))(* x x)))))


;; 5 - A
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define c->f
    (lambda (x)
        (+ (* (/ 9 5) x) 32)))


;; 5 - B
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define f->c
    (lambda (x)
        (* (- x 32) (/ 5 9))))

;; 5 - C
;; If repeated this will appraoch -40


;; 6 - A
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define yd->in
    (lambda (x)
        (* 36 x)))

;; 6 - B
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define in->cm
    (lambda (x)
        (* 2.54 x)))

;; 6 - C
;; What it does - Take the square of two numbers and returns what those
;;numbers are added together.
(define yd->cm
    (lambda (x)
        (in->cm (yd->in x))))

;; 7 - A
;; What it does - Returns left-half of the word.
;;numbers are added together.
(define left-part
    (lambda (inputString)
        (substring inputString 0 (quotient (string-length inputString) 2))))

;; 7 - B
;; What it does - Returns right-half of the word.
;;numbers are added together.
(define right-part
    (lambda (inputString)
        (substring inputString (quotient (string-length inputString) 2)
          (string-length inputString))))

;; 7 - C
;; What it does - Finds the middle of the word.
(define middle
    (lambda (inputString)
        (substring inputString (quotient (string-length inputString) 2)
          (quotient (string-length inputString) 2) )))


;; 8 - A
;; What it does - Randomly returns a letter from the input string.
(define char-at
    (lambda (inputString x)
        (substring inputString x (+ x 1))))

;; 8 - B
;; What it does - Randomly returns a letter from the input string.
(define random-char
    (lambda (inputString)
        (char-at inputString (random (string-length inputString)))))

;; 8 - C
;; What it does - Adds the word as between to words
(define make-simile
    (lambda (wordOne wordTwo)
        (string-append wordOne " as " wordTwo)))

;; 9
;; What it does - returns a randomly selected integer in
;;the closed interval from n to n + p.
(define random-in-range
    (lambda (n p)
        (+ n (random (+ p 1)))))

;; 10
;; What it does - Returns a randomly selected integer in the range of
;;integers that are dist units away from n in either direction.
(define jitter
  (lambda (n dist)
   (+ n (- (random (+ (* dist 2) 1)) dist))))