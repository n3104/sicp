#lang planet neil/sicp

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

; 以下、課題箇所
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; 以下、動作確認
(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(display-stream s)

(display-stream (stream-map + s s))
