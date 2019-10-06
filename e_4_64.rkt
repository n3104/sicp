#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; outranked-by
; 元のコード
;(run-query #cs'(assert!
;                (rule (outranked-by ?staff-person ?boss)
;                      (or (supervisor ?staff-person ?boss)
;                          (and (supervisor ?staff-person ?middle-manager)
;                               (outranked-by ?middle-manager ?boss))))))

; Louis Reasonerの誤ったコード
(run-query #cs'(assert!
                (rule (outranked-by ?staff-person ?boss)
                      (or (supervisor ?staff-person ?boss)
                          (and (outranked-by ?middle-manager ?boss) ; 次の supervisor のクエリと順序が入れ替わっている。
                               (supervisor ?staff-person ?middle-manar))))))


; テスト
; 無限ループ
;(run-query #cs'(outranked-by (Bitdiddle Ben) ?who))
