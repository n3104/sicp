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

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

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

(define (integrate-series coeffs)
  (stream-map / coeffs integers))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; 以下、追加実装
; https://wizardbook.wordpress.com/2010/12/20/exercise-3-60/
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) 
                                          (stream-car s2))
                            (mul-series s1 
                                        (stream-cdr s2)))))

; 以下、動作確認
(define cos-square+sin-square
  (add-streams (mul-series cosine-series 
                           cosine-series)
               (mul-series sine-series 
                           sine-series)))

(for ([i 5])
  (display-line (stream-ref cos-square+sin-square i)))

