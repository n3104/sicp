#lang planet neil/sicp

(define (count-pairs x)
  (let ((counted-pairs nil))
    
    (define (contains? pairs pair)
      (cond ((null? pairs) false)
            ((eq? (car pairs) pair) true)
            (else (contains? (cdr pairs) pair))))

    (define (count pairs)
      (cond ((not (pair? pairs)) 0)
            ((contains? counted-pairs pairs) 0)
            (else 
             (set! counted-pairs (append counted-pairs (list pairs)))
             (+ (count (car pairs))
                (count (cdr pairs))
                1))))

    (count x)))

; https://wizardbook.wordpress.com/2010/12/15/exercise-3-17/
(define l31 (list 'a 'b 'c))
 
(define l41 (list 'b 'c))
(define l42 (list 'a))
(set-car! l41 l42)
(set-car! (cdr l41) l42)
 
(define l71 (list 'c))
(define l72 (list 'b))
(define l73 (list 'a))
(set-car! l72 l73)
(set-cdr! l72 l73)
(set-car! l71 l72)
(set-cdr! l71 l72)
 
(define linf (list 'a 'b 'c))
(set-cdr! (cdr (cdr linf)) linf)
 
(count-pairs l31)
(count-pairs l41)
(count-pairs l71)
(count-pairs linf)