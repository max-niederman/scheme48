(define (null? obj) (eqv? obj '()))

(define (bubble elem sorted)
    (if (null? sorted)
        (cons elem '())
        (if (<= elem (car sorted))
            (cons elem sorted)
            (cons
                (car sorted)
                (bubble elem (cdr sorted))))))

(define (sort lst)
    (if (null? lst)
        lst
        (bubble
            (car lst)
            (sort (cdr lst)))))

(display "Enter list\n> ")
(write (sort (read)))
(newline)