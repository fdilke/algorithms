#lang racket

(require data/either)

(define (backtrack start-node explore)
    (define (result input-list unprocessed)
        (println (string-append "looping into result..." (~v (length input-list))))
        (stream-lazy
            (cond
                ((stream-empty? unprocessed)
                    (if (null? input-list)
                        (stream)
                        (result (rest input-list)
                            (stream-append unprocessed (explore (first input-list)))
                        )
                    )
                )
                (else                    
                    (let (
                        (an-either (stream-first unprocessed))
                        (remaining (stream-rest unprocessed))
                        )
                        (either 
                            (λ (solution)
                                (println (string-append "found solution: " (~v solution)))
                                (stream-cons solution 
                                    (result input-list remaining))
                            )
                            (λ (node)
                                (result (cons node input-list) remaining)
                            )
                            an-either
                        )
                    )
                )
            )
        )
    )
    (result (list start-node) (stream))
)

(provide
 (contract-out
    [backtrack (parametric->/c [A] (-> A (-> A stream?) stream?))]
 ))

(module+ test
  (require rackunit)
  (check-equal? ; a non starter node that can't be explored
    (stream->list
        (backtrack 0 (lambda (x) (stream))))
    null
  )
;   (check-equal? ; a one-off node that explores to one level
;     (stream->list
;         (backtrack 0 (lambda (x) (stream (success 1)))))
;     '(1)
;   )
)