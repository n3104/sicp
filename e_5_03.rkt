#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define factorial-machine
  (make-machine
   '(x guess t)
   (list (list 'square square) (list '- -) (list 'abs abs) (list '< <) (list '/ /) (list 'average average))
   '(sqrt-iter
     (assign guess (const 1.0))
     sqrt-iter-loop
     (assign t (op square) (reg guess))
     (assign t (op -) (reg t) (reg x))
     (assign t (op abs) (reg t))
     (test (op <) (reg t) (const 0.001))
     (branch (label sqrt-iter-done))
     (assign t (op /) (reg x) (reg guess))
     (assign t (op average) (reg guess) (reg t))
     (assign guess (reg t))
     (goto (label sqrt-iter-loop))
     sqrt-iter-done)))

(set-register-contents! factorial-machine 'x 2)

(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'guess) 1.4142156862745097)
(display 'ok)
