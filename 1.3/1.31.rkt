(define (cube n)
  ( * n n n))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a ) (* (term a) result))))
  (iter a 1))

(define (prod-cubes a b)
  (prod cube a inc b))

(define (prod-i a b)
  (prod identity a inc b))

(define (inc n) (+ n 1))

(define (identity z) z)

(define (pi-prod prodf n)
  (define (odd? i)
    (= (remainder i 2) 1))
  (define (term i)
    (if (odd? i)
        (/ (+ i 1) (+ i 2))
        (/ (+ i 2) (+ i 1))))
  (* 4 (prodf term 1.0 inc n)))

