#lang planet neil/sicp

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

; unless は関数であるため毎回全ての引数が展開され、停止せずに無限ループする。
;(factorial 5)