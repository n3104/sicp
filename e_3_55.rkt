#lang planet neil/sicp

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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 以下、追加実装
(define (partial-sums s) (cons-stream
                          (stream-car s)
                          (add-streams (partial-sums s) (stream-cdr s))))

; 以下、動作確認
(define partial-sums-of-integers (partial-sums integers))

(stream-ref partial-sums-of-integers 0)
(stream-ref partial-sums-of-integers 1)
(stream-ref partial-sums-of-integers 2)
(stream-ref partial-sums-of-integers 3)
(stream-ref partial-sums-of-integers 4)
