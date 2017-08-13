#lang planet neil/sicp

(define (is-circularly-linked-list? x)
  (let ((seen nil))
    
    (define (is-circular? y)
      (cond ((not (pair? y)) false)
            ((memq y seen) true)
            (else 
             (set! seen (cons y seen))
             (is-circular? (cdr y)))))

    (is-circular? x)))

; https://wizardbook.wordpress.com/2010/12/16/exercise-3-18/
(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cdr (cdr l2)) l2)
(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))
(define l4 (list 'a 'b 'c 'd 'e))
(set-car! (cdddr l4) (cddr l4))

(is-circularly-linked-list? l1)
(is-circularly-linked-list? l2)
(is-circularly-linked-list? l3)
(is-circularly-linked-list? l4)