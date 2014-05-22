(define (cube n)
  ( * n n n))
(define (sum term a next  b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity z) z)
(define (sum-i a b)
  (sum identity a inc b))
(define (pi-sum a b)
  (define (pi-term z)
    (/ 1.0 (* z (+ z 2))))
  (define (pi-next z)
    (+ z 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a(/ dx 2.0)) add-dx b) dx))


(define (simpson a b)
  (