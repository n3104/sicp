#lang planet neil/sicp

(#%require (only racket include))
(include "./c_4_4.rkt")

; 会合の登録
(run-query #cs'(assert! (meeting accounting (Monday 9am))))
(run-query #cs'(assert! (meeting administration (Monday 10am))))
(run-query #cs'(assert! (meeting computer (Wednesday 3pm))))
(run-query #cs'(assert! (meeting administration (Friday 1pm))))
(run-query #cs'(assert! (meeting whole-company (Wednesday 4pm))))

; a. 金曜の朝, Benはその日にあるすべての会合について, 
(run-query #cs'(meeting ?divison (Friday ?time)))

; b. ある人の会合はwhole-companyの会合すべてと, その人の部門の会合のすべてを含むとする規則を設計した.
(run-query #cs'(assert! (rule (meeting-time ?person ?day-and-time)
                              (or (meeting whole-company ?day-and-time)
                                  (and (job ?person (?division . ?title))
                                       (meeting ?division ?day-and-time))))))

; c. Alyssaは水曜の朝, 仕事場に着き, その日どの会合に出なければならないか考えた. 上の規則を定義したとして, これを見つけるのに彼女はどういう質問をすべきか.
(run-query #cs'(and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
                    (meeting ?division (Wednesday ?time))))
