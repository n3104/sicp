#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; 4.4.1節のwheel規則
(run-query #cs'(assert!
                (rule (wheel ?person)
                      (and (supervisor ?middle-manager ?person)
                           (supervisor ?x ?middle-manager)))))

; テスト
; デバッグに使える
;(run-query #cs'(supervisor ?middle-manager ?person))
;(run-query #cs'(and (supervisor ?middle-manager ?person)
;                           (supervisor ?x ?middle-manager)))

(run-query #cs'(wheel ?who))
