#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")


(display '((f) 'x 'y))
(newline)
(compile
 '(f 'x 'y)
 'val
 'next)


(newline)
(display '((f) 'x 'y))
(newline)
(compile
 '((f) 'x 'y)
 'val
 'next)

(newline)
(display '(f (g 'x) y))
(newline)
(compile
 '(f (g 'x) y)
 'val
 'next)

(newline)
(display '(f (g 'x) 'y))
(newline)
(compile
 '(f (g 'x) 'y)
 'val
 'next)
