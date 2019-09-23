#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; lives-near の登録
(run-query #cs'(assert! (rule (lives-near ?person-1 ?person-2)
                              (and (address ?person-1 (?town . ?rest-1))
                                   (address ?person-2 (?town . ?rest-2))
                                   (not (same ?person-1 ?person-2))))))

;  Alyssa P. Hackerは自分の近くに住む人が探せ 
(run-query #cs'(lives-near (Hacker Alyssa P) ?person))

; 住む人同士の対すべてを探そうとした時, 彼女は互いに近くに住む人の対のそれぞれが二度ずつリストされることに気づいた;
(run-query #cs'(lives-near ?person-1 ?person-2))
