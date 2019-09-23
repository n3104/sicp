#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; rule replace
(run-query #cs'(assert! (rule (replace ?person-1 ?person-2)
                              (and (job ?person-1 ?person-1-job)
                                   (job ?person-2 ?person-2-job)
                                   (can-do-job ?person-1-job ?job1-can-do)
                                   (or (same ?person-1-job ?person-2-job)
                                       (same ?job1-can-do ?person-2-job))
                                   (not (same ?person-1 ?person-2))))))

; a. Cy D. Fectに代れる人すべて;
(run-query #cs'(replace ?x (Fect Cy D)))

; b. 誰かに代れて, その誰かの方が多くの給料を貰っている人すべてと, 両者の給料.
(run-query #cs'(and (replace ?person-1 ?person-2)
                    (salary ?person-1 ?person-1-salary)
                    (salary ?person-2 ?person-2-salary)
                    (lisp-value > ?person-1-salary ?person-2-salary)))
