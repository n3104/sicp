#lang planet neil/sicp

(define (make-monitored func)
  (define count 0)
  (define (apply-func arg)
    (set! count (+ count 1))
    (func arg))
  (define (how-many-calls?) count)
  (define (reset-count)
    (set! count 0))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (how-many-calls?))
          ((eq? m 'reset-count) (reset-count))
          (else (apply-func m))))
  dispatch)

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
