(define (cube n)
  ( * n n n))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (prod term a next b)
  (display "prod call..")
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a ) (* (term a) result))))
  (iter a 1))

(define (prod-cubes a b)
  (accumulate * 1 cube a inc b))

(define (prod-i a b)
  (accumulate * 1 identity a inc b))

(define (sum-i a b)
  (accumulate + 0 identity a inc b))

(define (inc n) (+ n 1))

(define (identity z) z)

(define (pi-prod n)
  (define (odd? i)
    (= (remainder i 2) 1))
  (define (term i)
    (if (odd? i)
        (/ (+ i 1) (+ i 2))
        (/ (+ i 2) (+ i 1))))
  (* 4 (accumulate * 1 term 1.0 inc n)))

(define (pi-sum a b)
  (define (pi-term z)
    (/ 1.0 (* z (+ z 2))))
  (define (pi-next z)
    (+ z 4))
  (* 8 (accumulate + 0 pi-term a pi-next b)))




(define (accumulate combinder nullval term a next b)
  (define (iter a result)
    (if (> a b) 
        result 
        (iter (next a) (combinder result (term a)))))
  (iter a nullval))

