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
