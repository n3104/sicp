#lang planet neil/sicp

; https://wizardbook.wordpress.com/2010/12/14/exercise-3-5/
(define (rand-update x) (random (expt 2 31)))
(define (random-init) (rand-update 0))
 
(define rand
  (let ((x (random-init)))
    (lambda ()
      (set! x (rand-update x))
      x)))
 
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
 
(define (square x) (* x x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
 
(define (estimate-integral x1 y1 x2 y2 pred? trials)
  (monte-carlo trials 
               (lambda () 
                 (pred? (random-in-range x1 x2)
                        (random-in-range y1 y2)))))
 
 
(define (estimate-integral-pi trials)
  (define radius 1000)
  ; (x - 5)**2 + (y - 7)**2 ≤ 3**2 をテストする関数
  (define (circle-test x y) 
    (<= (+ (square x)
           (square y))
        (square radius)))
   
  (exact->inexact
   ; πr**2 = π * 1/2 * 1/2 = estimate-integral -> π = 4 * estimate-integral
   ; estimate-integral 関数の戻り値は四角形の中の円の領域の確率であり、単位円の場合は面積 1 を掛けることになるため、半径は 1/2 となる。
   (* 4
      (estimate-integral 
       (- radius) (- radius) radius radius
       circle-test 
       trials))))

(estimate-pi 1000)
;(estimate-pi 10000)
;(estimate-pi 100000)
;(estimate-pi 1000000)

(estimate-integral-pi 1000)
;(estimate-integral-pi 10000)
;(estimate-integral-pi 100000)
;(estimate-integral-pi 1000000)