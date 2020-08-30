#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")
(set! make-stack make-stack-with-monitor)

(include "./ch5-syntax.rkt")
(include "./ch5-eceval-support.rkt")

(define (lexical-address-lookup address env)
  (let ((frame-number (car address)) (displacement-number (cadr address)))
    (define (lookup-frame counter env)
      (if (eq? env the-empty-environment)
          (error "Unbound variable" address)
          (let ((frame (first-frame env)))
            (if (eq? counter frame-number)
                frame
                (lookup-frame (+ counter 1) (enclosing-environment env))))))
    (define (lookup-value counter vars vals)
      (cond ((null? vars)
             (error "Unbound variable" address))
            ((eq? counter displacement-number)
             (car vals))
            (else
             (lookup-value (+ counter 1) (cdr vars) (cdr vals)))))
    (let ((frame (lookup-frame 0 env)))
      (let ((value (lookup-value 0 (frame-variables frame) (frame-values frame))))
        (if (eq? value '*unassigned*)
            (error "Unbound variable" address)
            value)))))


