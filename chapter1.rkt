(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (sum-square x y)
  (+ (square x) (square y)))

(define (max-sum-square x y z)
  (cond ((and (>= x z) (>= y z)) (sum-square x y))
        ((and (>= x y) (>= z y)) (sum-square x z))
        ((and (>= y x) (>= z x)) (sum-square y z))))

; a + |b|
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; sqrt via Newton's Method

(define (good-enough? guess x)
  (< (abs (- x (square guess)))
     0.0001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt2 x) (sqrt-iter 1.0 x))

(define (good-enough-diff? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.0001))

(define (better-sqrt-iter guess prev-guess x)
  (if (good-enough-diff? guess prev-guess)
      guess
      (better-sqrt-iter (improve guess x) guess x)))

(define (sqrt3 x) (better-sqrt-iter 1.0 2.0 x))

; Example: Counting Change

(define (change-coins amount)
  (define (cc amount coins)
    (cond ((= amount 0) 1) ; one way to make 0
          ((or (< amount 0) (= coins 0)) 0)
          (else (+ (cc amount 
                       (- coins 1))
                   (cc (- amount 
                          (first-denomination coins)) 
                       coins)))))
  (define (first-denomination coins)
    (cond ((= coins 1) 1)
          ((= coins 2) 5)
          ((= coins 3) 10)
          ((= coins 4) 25)
          ((= coins 5) 50)))
  (cc amount 5))

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
         (* 2 (f (- n 2))) 
         (* 3 (f (- n 3))))))

; Exercise 1.11
; Iterative solution to f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3)

(define (f2 n)
  (define (iter a b c count)
    (if (< count 3) 
        (+ (* a 2) (* b 1))
        (iter (+ a b)
              (+ (* 2 a) c)
              (* 3 a)
              (- count 1))))
  (if (< n 3)
      n
      (iter 1 2 3 (- n 1))))

; Exercise 1.16: iterative exponentiation using successive squaring
; Key is (b^{n/2})^2 = (b^2)^{n/2}
; Invariant: acc * b^n = (init-b)^(init-n)

(define (exp2 init-b init-n)
  (define (iter b n acc)
    (cond ((= n 0) acc)
          ((even? n) (iter (square b)
                           (/ n 2) 
                           acc))
          (else (iter b
                      (- n 1) 
                      (* b acc)))))
  (define (even? n)
    (= (modulo n 2) 0))
  (define (square x)
    (* x x))
  (iter init-b init-n 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.3 Formulating Abstractions with Higher-Order Procedures ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (ident x) x)

(define (inc x) (+ x 1))

; Sum of integers from a to b
(define (sum-int a b)
  (sum ident a inc b))

(define (nth-harmonic n)
  (define (f x) (/ 1 x))
  (sum f 1 inc n))

(define (integral f a b dx)
  (define (next x) (+ x dx))
  (* (sum f 
          (+ a (/ dx 2.0)) 
          next 
          b)
     dx))

; Numerical integration using Simpson's Rule

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (coef x)
    (cond ((= x 0) 1)
          ((= x n) 1)
          (else (if (even? x) 2 4))))
  (define (term x) (* (coef x)
                      (f (+ a (* x h)))))
  (* (/ h 3.0)
     (sum term 0 inc n)))

; Tail recursive summation

(define (sum2 term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (+ (term a) acc))))
  (iter a 0))

; Exercise 1.31

(define (product term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) 
              (* (term a) acc))))
  (iter a 1))

(define (product-int a b)
  (product ident a inc b))

(define (factorial x)
  (product-int 1 x))

(define (approx-half-pi n)
  (define (f i)
    (define 2i (* 2 i))
    (* (/ 2i
          (- 2i 1))
       (/ 2i
          (+ 2i 1))))
  (product f 1 inc n))

(define (approx-pi n)
  (* (approx-half-pi n) 2.0))

; Exercise 1.32

; Linear recursive version of accumulate

(define (accumulate-linear combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner 
                            null-value 
                            term 
                            (next a) 
                            next 
                            b))))

; Tail recursive version of accumulate

(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) 
              (combiner (term a) acc))))
  (iter a null-value))

; Defining summation in terms of accumulate

(define (sum3 term a next b)
  (accumulate + 0 term a next b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.3.3 Procedures as General Methods ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Finding roots by half-interval method
; Start with too points above and below
; and do binary search for root

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (average x y)
  (/ (+ x y) 2.0))

; Wrapper around search that flips the upper and lower
; bounds if necessary. It prints an error if the signs
; are the same.

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (display "Values are not of opposite sign")))))

; Finds fixed point of a function by successive application

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

; Defining sqrt in terms of a fixed point

(define (sqrt3 x)
  (fixed-point (lambda (y) (average y (/ x y))) 
               1.0))

; Exercise 1.37 - Continued fractions

(define (cont-frac n d k)
  (define (helper i)
    (if (> i k)
        0.0
        (/ (n i)
           (+ (d i)
              (helper (+ i 1))))))
  (helper 1))

; Iterative version

(define (cont-frac2 n d k)
  (define (iter k acc)
    (if (< k 1)
        acc
        (iter (- k 1)
              (/ (n k)
                 (+ (d k) acc)))))
  (iter k 0.0))

(define phi (/ (+ 1.0 (sqrt 5)) 2))

; Exercise 1.38

(define (e-approx k)
  (+ (cont-frac2 
      (lambda (i) 1.0)
      (lambda (i)
        (let ((shift (- i 2)))
          (if (= (modulo shift 3) 0)
              (* (+ (/ shift 3) 1) 2)
              1)))
      k)
     2))

; Exercise 1.39

(define (neg x)
  (- 0 x))

(define (tan-cf x k)
  (cont-frac2
   (lambda (i)
     (if (= i 1)
         x
         (neg (square x))))
   (lambda (i)
     (- (* 2 i) 1))
   k))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.3.4 Procedures as Returned Values ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Newton's Method

(define dx 0.0001)

(define (derivative f)
  (lambda (x)
    (/ (- (f (+ x dx)) 
          (f x)) 
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((derivative g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt4 x)
  (newtons-method
   (lambda (y) (- (square y) x))
   1.0))

; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (* a (cube x)) (* b (square x)) c)))

; Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

; Exercise 1.44

(define (smooth f)
  (define (avg3 x y z)
    (/ (+ x y z) 3))
  (lambda (x)
    (avg3 (f (+ x dx))
          (f x)
          (f (- x dx)))
       ))

(define (smooth-n f n)
  (lambda (x)
    ((repeated smooth n) x)))

; Exercise 1.45

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt5 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 
               1.0))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y))))
               1.0))

(define (log-base-n x n)
  (/ (log x) (log n)))

(define (log2 x)
  (log-base-n x 2))

(define (nth-root x n)
  (let ((damp-n (floor (log2 n))))
    (fixed-point ((repeated average-damp damp-n) 
                  (lambda (y) (/ x (exp2 y (- n 1)))))
                 1.0)))

; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (try guess)
      (let ((next-guess (improve guess)))
        (if (good-enough? guess next-guess)
            next-guess
            (try next-guess))))
    (try guess)))

(define (good-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (sqrt6 x)
  ((iterative-improve 
    good-enough?
    (average-damp (lambda (y) (/ x y))))
   1.0))

(define (fixed-point2 f guess)
  ((iterative-improve
    good-enough?
    f)
   guess))

(define (sqrt7 x)
  (fixed-point2 (average-damp (lambda (y) (/ x y)))
                1.0))
