#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")


(display 'factorial)
(newline)
(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)

(display 'factorial-alt)
(newline)
(compile
 '(define (factorial-alt n)
    (if (= n 1)
        1
        (* n (factorial-alt (- n 1)))))
 'val
 'next)
