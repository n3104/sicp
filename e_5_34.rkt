#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")


(define (display-compile-result result)
  (for-each (lambda (x) (display x) (newline))
            (cons (car result) (cons (cadr result) (caddr result)))))

(display-compile-result
 (compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1))
  'val
  'next))
