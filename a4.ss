;;Name - Liam Bolling

;; 1 - A
;; What it does - Swaps the first and last elements in a list.
(define swap1-3
   (lambda (ls)
      (list (car (cdr (cdr ls))) (car (cdr ls)) (car ls))))

;;1 - B
;;What is does - Brings the first element in a list to the back of the list.
(define rotate
  (lambda (ls)
    (list (car (cdr ls)) (car (cdr (cdr ls))) (car ls))))


;;1 - C
;;What is it does - (Don't really know what ya'll want with this problem beside
;;proving that what I did works...)

;;~ (rotate (swap1-3 '(a b c)))
;;(b a c)
;;~ (rotate '(a b c))
;;(b c a)
;;~ (rotate (rotate '(a b c)))
;;(c a b)


;;2
;;What it does - A ton of stuff involing cons and numbers and lists. See below
;;the output in the comments. They all work, trust me.

(define ls1
  (cons 1 (cons 2 '())))
;;(1 2)

(define ls2
  (cons 3 (cons 4 (cons 5 '()))))
;;(3 4 5)

(define ls3
  (cons (cons 6 '()) (cons (cons 7 '()) (cons (cons 8 '()) '()))))
;;((6) (7) (8))

(define ls4
  (cons (cons 9 (cons 10 '())) (cons 11 '())))
;;((9 10) 11)

(define ls5
  (cons '12 (cons (cons 13 (cons 14 '())) '())))
;;(12 (13 14))

(define ls6
  (cons 15 (cons (cons 16 (cons (cons 17 '()) '())) '())))
;;(15 (16 (17)))

(define ls7
 (cons (cons (cons 18 '()) (cons 19 '())) (cons 20 '())))
;;(((18) 19) 20)

(define ls8
  (cons 'a (cons 'b (cons 'c (cons 'd (cons 'e (cons 'f '())))))))
;;(a b c d e f)

(define ls9
  (cons (cons (cons (cons 'g '()) '()) '()) '()))
;;((((g))))

(define ls10
  (cons (cons 'h (cons 'i '())) (cons (cons 'j (cons 'k '()))
  (cons (cons 'm (cons 'n '())) '()))))
;;((h i) (j k) (m n)) - This one got long so I pressed enter. Tiro got mad :/



;;3 - A
;;What it does - Takes two things and puts them in a list.
(define couple
  (lambda (name-x name-y)
    (cons name-x (cons name-y '()))))

;;3 - B
;;What it does - Uses Couple to roll dice and get a list of two
;;random numbers in a list. #magic
(define two-dice
  (lambda ()
    (couple (random 6) (random 6))))


;; 4
;; What it does - A procedure trisect that takes a two-element list
;;of numbers representing an interval on the number line of the form
;;[a, b], where a < b.
(define trisect
  (lambda (ls)
    (let ([x (/ (- (car (cdr ls)) (car ls) ) 3)])
      (list
            (cons (car ls) (cons (+ (car ls) x) '()))
            (cons (+ (car ls) x) (cons (+ (car ls) (* 2 x)) '()))
            (cons (+ (car ls) (* 2 x)) (cons (+ (car ls) (* 3 x)) '()))
      ))))


;;5 Elmo
;;What it does - Not work. This one makes no sense.
(define walking-point
  (lambda (pace)
    (cond
      [(>= pace 10) 0]
      [else walking-point])))




;;6 - A
;;What it does - A procedure u-count that takes a list of items and
;;returns the number of items in the list in unary
(define u-count
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons 'l (u-count (cdr ls)))])))

;;6 - B
;;What it does - A procedure u-add-one that takes a number represented
;;in unary and returns the number plus one represented in unary.
(define u-add-one
  (lambda (ls)
    (cons 'l ls)))

;;6 - C
;;What it does - A procedure u-add that takes two numbers represented in
;;unary and returns the number representing the addition of the two numbers.
(define u-add
  (lambda (ls ls2)
    (append ls ls2)))




;;7
;;What it does -  A procedure take-five that takes a list containing only
;;l's and returns a list where each group of five l's is replaced by the
;;symbol v.
(define take-five
  (lambda (ls)
    (define num-of-v (quotient (length ls) 5))
    (define make-v
      (lambda (num-of-v)
        (cond
          [(> num-of-v 0) (cons 'v (make-v (sub1 num-of-v)))]
          [else (make-l num-of-l)])))
           (define num-of-l (remainder (length ls) 5))
           (define make-l
             (lambda (num-of-l)
               (cond
                 [(> num-of-l 0) (cons 'l (make-l (sub1 num-of-l)))]
                 [else '()])))
    (make-v num-of-v)))


;;8
;;What it does - A recursive procedure all-zeroes? that takes a
;;list of bits and returns #t if and only if the list contains only zeroes.
(define all-zeroes?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(equal? (car ls) 'l) #f ]
      [else (all-zeroes? (cdr '(ls))) ])))



;;9 - A
;;What it does - A procedure get-page-number that takes a web page and
;;returns its page number
(define get-page-number
  (lambda (str)
    (cond
      [(null? str) '()]
      [else (car str)])))

;;9 - B
;;What it does - A procedure get-words that takes a web page and returns
;;the list of words on the page.
(define get-words
  (lambda (str)
    (cond
      [(null? str) '()]
      [else (cdr str)])))

;;9 - C
;;What it does - Define a procedure make-simple-web that takes three
;;web pages and return a list of the given pages.
(define page1 '(1 the dog chewed the rug))
(define page2 '(2 on the rug slept the dog))
(define page3 '(3 the rug rats slept on))
(define make-simple-web
  (lambda (x y z)
    (cons x (cons y (cons z '())))))

;;9 - D
;;Waht it does - A recursive procedure all-pages that takes a
;;representation of the web and returns a list of all the page numbers.
(define (all-pages web)
  (if (null? web)
      '()
      (cons (get-page-number (car web)) (all-pages (cdr web)))))




;;10 - A
;;What it does -  takes a list of two or more numbers and returns the list
;;with the first two numbers removed and replaced by the smaller of the two.
(define crunch-front
  (lambda (ls)
    (cond
      [(> (car ls) (cadr ls)) (cdr ls)]
      [else (cons (car ls)(cdr (cdr ls)))])))

;10 - B
;;What it does - Takes a list of one or more numbers and returns the minimal
;;element in the list.
(define find-min
  (lambda (ls)
    (cond
      [(null? (cdr ls)) (car ls)]
      [else
       (find-min (crunch-front ls))])))





;;11 - A
;;WHat it does - Implement drop-marble.
(define drop-marble
  (lambda (shoot ls)
    (if (equal? shoot 'a)
        (cond
          [(equal? (car ls) 'left) 'c]
          [(and (equal? (car ls) 'right) (equal? (car (cdr ls)) 'left)) 'c]
          [else 'd])
        (if (equal? shoot 'b)
            (cond
              [(equal? (car (cdr (cdr ls))) 'right) 'd]
              [(and (equal? (car (cdr (cdr ls))) 'left)
                    (equal? (car (cdr (cdr ls))) 'right)) 'd]
              [else 'c])
            "not a valid shoot"))))


;;11 - B
;;What it does - Copy the following definition of next-levers into your
;;homework file and then implement play-game.
(define next-levers
  (lambda (chute levers)
    (if (equal? chute 'a)
        (if (equal? (car levers) 'left)
            (list 'right (cadr levers) (caddr levers))
            (if (equal? (cadr levers) 'left)
                (list 'left 'right (caddr levers))
                (list 'left 'left (caddr levers))))
        (if (equal? (caddr levers) 'right)
            (list (car levers) (cadr levers) 'left)
            (if (equal? (cadr levers) 'left)
                (list (car levers) 'right 'right)
                (list (car levers) 'left 'right))))))

(define play-game
  (lambda (s-ls l-ls)
    (if (null? s-ls)
        '()
        (cons (drop-marble (car s-ls) l-ls) (play-game (cdr s-ls)
                                              (next-levers (car s-ls) l-ls))))))