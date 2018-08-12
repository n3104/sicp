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

(define (display-line x)
  (display x)
  (newline))

; 以下、追加実装
; 結果を固定した方があとで見やすいので固定する。
(#%require (only racket/base random-seed))
(random-seed 1)

(define (rand-update x) (random (expt 2 31)))
(define (random-init) (rand-update 0))

(define random-numbers
  (cons-stream (random-init) ; (random-init) は関数ではなく実行結果を渡さないと実行時エラーとなる。
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

; 以下、動作確認
(#%require (only racket/base for))
(for ([index 5])
  (display-line
   (stream-ref
    pi
    index)))

(stream-ref pi 1000)

(stream-ref pi 10000)
