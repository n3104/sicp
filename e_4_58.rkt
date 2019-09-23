#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; rule 黒幕[big shot]
; https://wizardbook.wordpress.com/2011/01/26/exercise-4-58/
(run-query '(assert! (rule (big-shot ?name ?division)
                           (and (job ?name (?division . ?title))
                                (supervisor ?name ?boss)
                                (job ?boss (?division-2 . ?title-2))
                                (not (same ?division ?division-2))))))

; 動作確認
(run-query #cs'(big-shot ?name ?division))
