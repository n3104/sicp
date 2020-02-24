#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

(define factorial-machine
  (make-machine
   '(n product counter)
   (list (list '> >) (list '* *) (list '+ +))
   '(factorial
     (assign product (const 1))
     (assign counter (const 1))
     test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label factorial-done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))
     factorial-done)))

(set-register-contents! factorial-machine 'n 5)
(set-register-contents! factorial-machine 'product 1)
(set-register-contents! factorial-machine 'counter 1)

(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'product) 120)
(display 'ok)