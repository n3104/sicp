#lang planet neil/sicp


(define (find-variable var env)
  (define (env-loop frame-number env)
    (define (scan displacement-number vars env)
      (cond ((null? vars)
             (env-loop (+ frame-number 1) (cdr env)))
            ((eq? var (car vars))
             (list frame-number displacement-number))
            (else (scan (+ displacement-number 1) (cdr vars) env))))
    (if (null? env)
        'not-found
        (scan 0 (car env) env)))
  (env-loop 0 env))

; 動作確認
(#%require (only rackunit check-equal?))
(check-equal? (find-variable 'c '((y z) (a b c d e) (x y))) '(1 2))
(check-equal? (find-variable 'x '((y z) (a b c d e) (x y))) '(2 0))
(check-equal? (find-variable 'w '((y z) (a b c d e) (x y))) 'not-found)
(display 'done)