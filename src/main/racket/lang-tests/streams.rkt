#lang racket

(define number-stream/c
  (promise/c
   (or/c
    null?
    (cons/c number? (recursive-contract number-stream/c)))))

; until I know how to do a proper contract, have a predicate.
; Note you wouldn't use this as a contract as it defeats the delayed evaluation!
(define (streamof pred)
  (define (the_fun x)
    (match x
      [null #t]
      [(cons head tail)
        (and (pred head) (the_fun tail))
      ]    
      [_ #f]
    )
  )
  the_fun
)

(define sample-number
  82
)

(define sample-number-stream
  (delay (stream 1 2 3))
)

(provide
 (contract-out
    [sample-number number?]
    ; [sample-number-stream number-stream/c]
    [sample-number-stream (streamof number?)]
 ))

(module+ test
  (require rackunit)
  (check-equal?
    (stream->list (stream 2 3 4))
    '(2 3 4)
  )
  (check-equal?
    (stream->list (stream-map sub1 (stream 1 2 3)))
    '(0 1 2)
  )
  (check-equal?
    (stream? (stream 1 2 3))
    #t
  )
  (check-equal?
    (stream? "it ain't")
    #f
  )
  (define flag #f)
  (define test-stream
    (stream (set! flag #t) 7)
  )
  (check-equal?
    flag
    #f
  )
  (check-equal?
    (stream-first (stream-rest test-stream))
    7
  )
  (check-equal?
    flag
    #f
  )
  ; (check-equal?
  ;   (number-stream/c "Stringly")
  ;   #f
  ; )
  ; (check-equal?
  ;   (number-stream/c (stream 0 1 2.3))
  ;   #t
  ; )
  ; (check-equal?
  ;   (number-stream/c (stream 'pig 'dog 'hag))
  ;   #f
  ; )
)
