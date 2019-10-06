#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; last-pair の実装
; https://wizardbook.wordpress.com/2011/01/30/exercise-4-62/
(run-query '(assert!
             (rule (last-pair (?x) (?x)))))
(run-query '(assert!
             (rule (last-pair (?x . ?y) (?z))
                   (last-pair ?y (?z)))))
 

; テスト
(run-query '(last-pair (3) ?x))
(run-query '(last-pair (1 2 3) ?x))
(run-query '(last-pair (2 ?x) (3)))