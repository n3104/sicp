#lang planet neil/sicp
(#%require (only racket/shared shared))

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
(define (solve f y0 dt)
  ; Racket の場合 (define dy (stream-map f y)) の y の引数を評価した時点で y: undefined; cannot use before initialization でエラーとなる。
  ; これを回避するには shared を利用して y を定義する必要がある。
  ; https://docs.racket-lang.org/reference/shared.html
  ; 
  ; (define y (integral dy y0 dt))
  ;  (define dy (stream-map f y))
  ;  y)
  (shared ([y (integral (delay dy) y0 dt)]
           [dy (stream-map f y)])
    y))

(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

; 以下、動作確認
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
