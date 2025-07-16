#lang racket

(define (linear-cache f)
    (let* (
        [size 1]
        [array (make-vector size (void))]
        ) (lambda (n)
            (let loop ()
                (cond [(>= n size)
                    (set! size (* size 2))
                    (set! array
                        (vector-append
                            array
                            (make-vector size (void))
                        )
                    )
                    (loop)]
                    [else
                        (let ([value (vector-ref array n)])
                            (cond [(void? value)
                                (set! value
                                    (f n)
                                )
                                (vector-set! array n value)
                                value]
                                [else value]
                            )
                        )]
                )
            ))
        )
)

(provide
 (contract-out
    [linear-cache (parametric->/c [A] (-> (-> number? A) (-> number? A)))]
  ))

(module+ test
    (require rackunit)
    (define (with-notary f callback)
        (let* (
            [call-list '()]
            [calls (lambda () call-list)]
            [notary (lambda (x)
                (set! call-list (cons x call-list))
                (f x)
            )])
            (callback notary calls)
        ))
    (with-notary add1 (lambda (notary calls)
        (let ([caching-add1 (linear-cache notary)])
        (check-equal? (caching-add1 2) 3)
        (check-equal? (calls) '(2))
        (check-equal? (caching-add1 2) 3)
        (check-equal? (calls) '(2))
        (check-equal? (caching-add1 3) 4)
        (check-equal? (calls) '(3 2))
        (check-equal? (caching-add1 3) 4)
        (check-equal? (calls) '(3 2))
    )))
)
