#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")


(define (display-compile-result result)
  (for-each (lambda (x) (display x) (newline))
            (cons (car result) (cons (cadr result) (caddr result)))))

(display 'factorial)
(newline)
(display-compile-result
 (compile
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
  'val
  'next))

(newline)
(display 'factorial-alt)
(newline)
(display-compile-result
 (compile
  '(define (factorial-alt n)
     (if (= n 1)
         1
         (* n (factorial-alt (- n 1)))))
  'val
  'next))
