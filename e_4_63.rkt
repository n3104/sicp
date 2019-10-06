#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; データの登録
(run-query #cs'(assert! (son Adam Cain)))
(run-query #cs'(assert! (son Cain Enoch)))
(run-query #cs'(assert! (son Enoch Irad)))
(run-query #cs'(assert! (son Irad Mehujael)))
(run-query #cs'(assert! (son Mehujael Methushael)))
(run-query #cs'(assert! (son Methushael Lamech)))
(run-query #cs'(assert! (wife Lamech Ada)))
(run-query #cs'(assert! (son Ada Jabal)))
(run-query #cs'(assert! (son Ada Jubal)))

; grandson
(run-query '(assert!
             (rule (grandson (?s) (?g))
                   (and (son ?f ?s)
                        (son ?g ?f)))))


; son-of-husband
(run-query '(assert!
             (rule (son-of-husband (?s) (?m))
                   (and (wife ?m ?w)
                        (son ?w ?s)))))

; テスト
(run-query '(grandson ?s ?g))
(run-query '(son-of-husband ?s ?g))
