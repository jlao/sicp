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
