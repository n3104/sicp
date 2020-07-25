#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")


(define (display-compile-result result)
  (for-each (lambda (x) (display x) (newline))
            (cons (car result) (cons (cadr result) (caddr result)))))

(display-compile-result
 (compile
  '(define (f x)
     (+ x (g (+ x 2))))
  'val
  'next))
