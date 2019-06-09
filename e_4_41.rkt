#lang racket

(define (flatmap proc seq)
  (foldr append 
         null 
         (map proc seq)))

; https://sicp.iijlab.net/fulltext/x223.html#index967
(define (permutations s)
  (if (null? s)                    
      (list null)                   
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
 
(define (present-solution solution)
  (map list 
       '(baker cooper fletcher miller smith)
       solution))
 
; https://wizardbook.wordpress.com/2011/01/12/exercise-4-41/
(define (multiple-dwelling)
     
  (define (invalid-solution permutation)
    (let ((baker (first permutation))
          (cooper (second permutation))
          (fletcher (third permutation))
          (miller (fourth permutation))
          (smith (fifth permutation)))
      (and (not (= baker 5))
           (not (= cooper 1))
           (not (= fletcher 5))
           (not (= fletcher 1))
           (> miller cooper)
           (not (= (abs (- smith fletcher)) 1))
           (not (= (abs (- fletcher cooper)) 1)))))
   
  (map present-solution ; 単なる表示用
       (filter invalid-solution
               (permutations (list 1 2 3 4 5))))) ; ここで総当たりの組み合わせを生成している。

(time (multiple-dwelling))