#lang racket

(define (iterate-to-fixed start-value fn)
    (define iterate
       (fn start-value)
    )
    (cond 
        ((equal? start-value iterate) start-value)
        (else (iterate-to-fixed iterate fn))
    )
)

(define (build-equivalence size relators)
  (define range (stream->list (in-range size)))
  (define array (apply vector range))
  (define (track-up i)
    (iterate-to-fixed i (lambda (j) (vector-ref array j)))
  )
  (define (equate i j)
    (if (not (= i j))
      (vector-set! array i j)
      '()
    )
  )
  (for ([relator relators])
    (match-let ([(list i j) relator])
      (equate (track-up i) (track-up j))
    )
  )
  (for ([i range])
    (vector-set! array i (track-up (vector-ref array i)))
  )
  (vector->list array)
)

(define (class-indices canonicals)
  (define indices (make-hash))
  (define index 0)
  (for/list ([c canonicals])
    (define lookup (hash-ref indices c null))
    (cond
      ((null? lookup) (hash-set! indices c index) (set! index (add1 index)) (sub1 index))
      (else c)
    )
  )
)

(define (build-classes size relators)
  (class-indices
    (build-equivalence size relators)
  )
)

(provide
 (contract-out
  ;; iterate a function on a start value until it's fixed
    [iterate-to-fixed (parametric->/c [A] (-> A (-> A A) A))]
    [build-equivalence (parametric->/c [A] (-> number? (listof (cons/c A A)) (listof A)))]
    [class-indices (parametric->/c [A] (-> (listof A) (listof number?)))]
  )
)
  

(module+ test
  (require rackunit)
  (define (conditional-inc x)
    (if (< x 10) (add1 x) x)
  )
  (check-equal? (iterate-to-fixed 3 conditional-inc) 10)
  (check-equal? (iterate-to-fixed 70 conditional-inc) 70)
  (define (raw-sqrt n) (/ (+ n (/ 120 n)) 2.0))
  (check-equal? (iterate-to-fixed 3 raw-sqrt) 10.954451150103322)
  (check-equal? (build-equivalence 0 '()) '())
  (check-equal? (build-equivalence 1 '()) '(0))
  (check-equal? (build-equivalence 2 '()) '(0 1))
  (check-equal? (build-equivalence 2 '((0 0) (1 1))) '(0 1))
  (check-equal? (build-equivalence 2 '((0 1))) '(1 1))
  (check-equal? (build-equivalence 4 '((1 2))) '(0 2 2 3))
  (check-equal? (build-equivalence 6 '((1 2)(2 3))) '(0 3 3 3 4 5))
  (check-equal? (build-equivalence 10 '((1 2)(7 0)(4 3) (3 7)(6 5)(9 5))) '(0 2 2 0 0 5 5 0 8 5))
  (check-equal? (build-equivalence 4 '((2 0)(3 1)(2 2)(3 3)(1 3)(0 2)(2 2)(3 3))) '(0 1 0 1))
  (check-equal? (build-equivalence 4 '((0 0)(0 1)(0 2)(0 3)(1 0)(1 1)(1 2)(1 3)(2 0)(2 1)(2 2)(2 3)(3 0)(3 1)(3 2)(3 3))) '(3 3 3 3))
  (check-equal? (class-indices '(1 0)) '(0 1))
  (check-equal? (class-indices '(2 3 0)) '(0 1 2))
;;  (check-equal? (class-indices '(2 2 0 0 2 2 1 1)) '(0 0 1 1 0 0 2 2))
;;  (check-equal? (class-indices '(0 2 2 3)) '(0 1 1 2))
;;  (check-equal? (build-classes 4 '((1 2))) '(0 1 1 2))
) 

