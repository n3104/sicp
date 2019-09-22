#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; a. Ben Bitdiddleに監督されている人すべて;
(run-query #cs'(supervisor ?x (Bitdiddle Ben)))

; b. 経理部門[accounting division]のすべての人の名前と担当;
(run-query #cs'(job ?x (accounting . ?y)))

; c. Slumervilleに住む人すべての名前と住所.
(run-query #cs'(address ?name (Slumerville . ?street-address)))