#lang planet neil/sicp

; https://wizardbook.wordpress.com/2010/12/15/exercise-3-16/
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l31 (list 'a 'b 'c))
(count-pairs l31)

; cons で書いてみる。
(define l41 (list 'a))
(define l42 (cons l41 nil))
(define l43 (cons l41 l42))
(count-pairs l43)

; cons で書いてみる。
(define l71 (list 'a))
(define l72 (cons l71 l71))
(define l73 (cons l72 l72))
(count-pairs l73)