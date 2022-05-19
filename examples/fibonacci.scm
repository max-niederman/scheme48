; there is no optimization whatsoever so this O(2^n) function is _very_ slow for large `n`
(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1)) (fib-rec (- n 2)))))

; a much more efficient implementation using accumulators for the last two values
; O(n) time, O(1) space so long as you stay within your CPU's word size
(define (fib-iter n)
    (let
        ((inner (lambda (acc1 acc2 remaining)
            (if (= remaining 0)
                acc2
                (inner 
                    acc2
                    (+ acc1 acc2)
                    (- remaining 1))))))
        (inner 0 1 (- n 1))))
