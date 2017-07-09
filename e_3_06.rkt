#lang planet neil/sicp
; neil/sicp は random-seed をロードしていないので個別に追加する。
; http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt
(#%require (only racket/base random-seed))

(define initial-seed 317)
(define (rand-update-seed x)
  (random (expt 2 31)))
 
(define (random-init-seed)
  (rand-update-seed (random-seed initial-seed)))
 
(define rand-seed
  (let ((x (random-init-seed)))
    (lambda (action)
      (cond ((eq? action 'generate) 
             (set! x (rand-update-seed x))
             x)
            ((eq? action 'reset) 
             (lambda (new-seed) 
               (set! x (random-init-seed))
               (set! x (rand-update-seed x))
               x))))))

(rand-seed 'generate)
(rand-seed 'generate)
(rand-seed 'generate)
(rand-seed 'generate)
 
((rand-seed 'reset) 'generate)
(rand-seed 'generate)