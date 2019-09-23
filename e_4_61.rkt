#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; next-to の登録
(run-query #cs'(assert! (rule (?x next-to ?y in (?x ?y . ?u)))))
(run-query #cs'(assert! (rule (?x next-to ?y in (?v . ?z))
                              (?x next-to ?y in ?z))))

; テスト
(run-query #cs'(?x next-to ?y in (1 (2 3) 4)))
(run-query #cs'(?x next-to 1 in (2 1 3 1)))
