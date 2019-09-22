#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; a. Ben Bitdiddleが監督している人すべての名前とその住所;
(run-query #cs'(and (supervisor ?person (Bitdiddle Ben))
                    (address ?person ?where)))

; b. 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料;
(run-query #cs'(and (salary (Bitdiddle Ben) ?ben-salary)
                    (salary ?person ?amount)
                    (lisp-value < ?amount ?ben-salary)))

; c. 計算機部門にいない人が監督している人すべてと, その監督者の名前と担当.
(run-query #cs'(and (supervisor ?person ?supervisor)
                    (job ?supervisor ?supervisor-job)
                    (not (job ?supervisor (computer . ?type)))))
