#lang planet neil/sicp

; https://wizardbook.wordpress.com/2010/12/16/exercise-3-19/
(define (has-cycle? xs)
  (define (seen-last-pair? x)
    (or (null? x) (null? (cdr x))))
  (define (chase turtle rabbit)
    (cond ((or (null? turtle) (null? rabbit)) #f)
          ; car の比較は pair の比較になっていなので修正した。car の比較の場合後述の l41 が #t になってしまう。
          ;((eq? (car turtle) (car rabbit)) #t)
          ((eq? turtle rabbit) #t)
          ((seen-last-pair? (cdr rabbit)) #f)
          (else (chase (cdr turtle) (cddr rabbit)))))
  (if (seen-last-pair? xs)
      #f
      (chase xs (cdr xs))))
 
(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cdr (cdr l2)) l2)
(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))
(define l4 (list 'a 'b 'c 'd 'e))
(set-car! (cdddr l4) (cddr l4))
 
(has-cycle? l1)
(has-cycle? l2)
(has-cycle? l3)
(has-cycle? l4)

; car で比較している場合は #t になる。
(define l41 (list 'b 'c))
(define l42 (list 'a))
(set-car! l41 l42)
(set-car! (cdr l41) l42)
(has-cycle? l41)