#lang planet neil/sicp
(#%require (only racket/base for))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-line x)
  (display x)
  (newline))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 以下、追加実装
(define (sign-change-detector v1 v2)
  (cond
    ((and (>= v1 0) (>= v2 0)) (list v1 v2 0))
    ((and (< v1 0) (>= v2 0)) (list v1 v2 -1))
    ((and (>= v1 0) (< v2 0)) (list v1 v2 1))
    ((and (< v1 0) (< v2 0)) (list v1 v2 0))
    ))

(define sense-data
  (cons-stream 1 (cons-stream 2 (cons-stream -2 (cons-stream -1 (cons-stream 0 (cons-stream 1 the-empty-stream)))))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings2
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

; 以下、動作確認
(display-line 'zero-crossings)
(for ([index 6])
  (display-line
   (stream-ref
    zero-crossings
    index)))

(display-line 'zero-crossings2)
(for ([index 6])
  (display-line
   (stream-ref
    zero-crossings2
    index)))