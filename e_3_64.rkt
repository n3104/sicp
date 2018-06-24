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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) 
                                          (stream-car s2))
                            (mul-series s1 
                                        (stream-cdr s2)))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


; 以下、追加実装
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))


; 以下、動作確認
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display-line 'sqrt-stream)
(for ([i 5])
  (display-line (stream-ref (sqrt-stream 2) i)))

(display-line 'sqrt)
(sqrt 2 1)
(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.001)
(sqrt 2 0.0001)
(sqrt 2 0.00001)
(sqrt 2 0.000001)
