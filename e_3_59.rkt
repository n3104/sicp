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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; a. integrate-seriesを定義せよ
; 以下、追加実装
(define (integrate-series s)
  (define (integrate-series-internal a den)
    (cons-stream
     (/ (stream-car a) (stream-car den))
     (integrate-series-internal (stream-cdr a) (stream-cdr den))))
  (integrate-series-internal s integers))

; 以下の方がスマート。
; https://wizardbook.wordpress.com/2010/12/20/exercise-3-59/
;(define (integrate-series coeffs)
;  (stream-map / coeffs integers))

; 以下、動作確認
(display-line '(integrate-series ones))
(define ones (cons-stream 1 ones))
(for ([i 5])
  (display-line (stream-ref (integrate-series ones) i)))

(display-line 'exp-series)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(for ([i 5])
  (display-line (stream-ref exp-series i)))


; b. 正弦と余弦の級数を生成する方法を示せ
; 以下、追加実装
; (cosx)' = -sinx
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
; (sinx)' = cosx
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; 以下、動作確認
(display-line 'cosine-series)
(for ([i 6])
  (display-line (stream-ref cosine-series i)))
(display-line 'sine-series)
(for ([i 6])
  (display-line (stream-ref sine-series i)))
