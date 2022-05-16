(define list (lambda a a))

(define (range n) (range-rec n 1))
(define (range-rec bound i)
    (if (= i bound)
        (cons i '())
        (cons i (range-rec bound (+ i 1)))))

(define (map func lst)
    (if (eqv? lst '())
        '()
        (cons
            (func (car lst))
            (map func (cdr lst)))))

(define (factor? p f) (= (remainder p f) 0))

(define (write-ln a) (list (write a) (newline)))
(define (display-ln a) (list (display a) (newline)))

(define (fizz-buzz n)
    (map
        (lambda (n)
            (if (factor? n 3)
                (if (factor? n 5)
                    (display-ln "Fizz Buzz")
                    (display-ln "Fizz"))
                (if (factor? n 5)
                    (display-ln "Buzz")
                    (write-ln n))))
        (range n)))

(display-ln "Enter upper bound:")
(display "> ")
(fizz-buzz (read))