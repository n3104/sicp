#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")
(set! make-stack make-stack-with-monitor)

(include "./ch5-syntax.rkt")
(include "./ch5-compiler.rkt")
(include "./ch5-5-eceval-support.rkt")
(include "./ch5-eceval-compiler.rkt")

;a.
;> (compile-and-go
; '(define (factorial n)
;    (if (= n 1)
;        1
;        (* (factorial (- n 1)) n))))
;
;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(factorial 2)
;
;(total-pushes = 13 maximum-depth = 5)
;;;; EC-Eval value:
;2
;
;;;; EC-Eval input:
;(factorial 3)
;(total-pushes = 19 maximum-depth = 8)
;;;; EC-Eval value:
;6
;
;;;; EC-Eval input:
;(factorial 4)
;(total-pushes = 25 maximum-depth = 11)
;;;; EC-Eval value:
;24
;
;;;; EC-Eval input:
;(factorial 5)
;
;(total-pushes = 31 maximum-depth = 14)
;;;; EC-Eval value:
;120
