
(define (average x y)
  (/ (+ x y) 2))

(define (make-rat n d)
  (let ((divisor (gcd n d))
        (sign (if (> (* n d) 0) 1 -1)))
    (cons (* sign (/ (abs n) divisor)) (/ (abs d) divisor))))

(define (numer r)
  (car r))

(define (denom r)
  (cdr r))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (display "{")
  (print-point (start-segment s))
  (display ", ")
  (print-point (end-segment s))
  (display "}"))

; Exercise 2.5

(define (make-pair x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (pair-first x)
  (define (iter x acc)
    (if (= (modulo x 2) 0)
        (iter (/ x 2) (+ acc 1))
        acc))
  (iter x 0))

(define (pair-second x)
  (define (iter x acc)
    (if (= (modulo x 3) 0)
        (iter (/ x 3) (+ acc 1))
        acc))
  (iter x 0))

(define (print-pair x)
  (display "(")
  (display (pair-first x))
  (display ", ")
  (display (pair-second x))
  (display ")"))
