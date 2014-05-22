(define (smallest-divisor n)
  (find-divisor n 2)) 

(define (find-divisor n td)
  (cond ((> (square td) n) n)
        ((divides? td n) td)
        (else (find-divisor n (+ td 1))))) 

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-milliseconds) start-time))
      false))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (search-for-primes n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (iter n counter)
    (if (< counter 3)
        (if (timed-prime-test n) 
            (iter (+ n 2) (+ counter 1))
            (iter (+ n 2) counter))))
  (if (even? n) 
      (iter (+ n 1) 0)
      (iter (+ n 2) 0)))