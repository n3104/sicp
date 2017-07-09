#lang planet neil/sicp

; https://wizardbook.wordpress.com/2010/12/14/exercise-3-8/
(define f1
  (let ((seen-zero? #f))
    (lambda (x)
      (cond (seen-zero? 0)
            (else (set! seen-zero? #t) 
                  x)))))
 
(define f
  (let ((seen-zero? #f))
    (lambda (x)
      (cond (seen-zero? 0)
            (else (set! seen-zero? #t) 
                  x)))))
(+ (f 0) (f 1)) 
(+ (f1 1) (f1 0))

; f を関数として宣言すると評価時にそれぞれ別の関数となり seen-zero? が共有されない。そのため f は関数ではなく変数として宣言する必要がある。
(define (f2 x)
  (let ((seen-zero? #f))
    (cond (seen-zero? 0)
          (else (set! seen-zero? #t) 
                x))))

(+ (f2 0) (f2 1)) 
