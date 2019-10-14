#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; append-to-form の登録
(run-query #cs'(assert!
                (rule (append-to-form () ?y ?y))))
(run-query #cs'(assert!
                (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                      (append-to-form ?v ?y ?z))))

; reverse の実装
; https://wizardbook.wordpress.com/2012/04/19/exercise-4-68/
; 再帰する際の停止条件
(run-query '(assert! (rule (reverse () ()))))
; and を利用して再帰的に reserve した値を append-to-form で結合する
(run-query '(assert! (rule (reverse (?h . ?t) ?y)
                           (and (reverse ?t ?reversed-t)
                                (append-to-form ?reversed-t (?h) ?y)))))

; テスト
(run-query #cs'(reverse (1 (2 3) 4) ?x))
(run-query #cs'(reverse (2 1 3 1) ?x))

; 逆は動作しない。無限ループになる
;(run-query #cs'(reverse ?x (2 1 3 1)))
