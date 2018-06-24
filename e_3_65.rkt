#lang planet neil/sicp
(#%require (only racket/base for))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (display-line x)
  (display x)
  (newline))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (partial-sums s) (cons-stream
                          (stream-car s)
                          (add-streams (partial-sums s) (stream-cdr s))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; 以下、追加実装
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))


; 以下、動作確認
(display-line 'ln2)
(for ([i 10])
  (display-line (stream-ref ln2-stream i)))

(display-line 'euler-transformed-ln2)
(for ([i 10])
  (display-line (stream-ref (euler-transform ln2-stream) i)))

(display-line 'accelerated-sequence-euler-transformed-ln2)
(for ([i 10])
  (display-line (stream-ref (accelerated-sequence euler-transform ln2-stream) i)))
