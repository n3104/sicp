#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; a. 再帰的べき乗:
(define recursive-expt-machine
  (make-machine
   '(b n continue val)
   (list (list '= =) (list '- -) (list '* *))
   '(expt
     (assign continue (label expt-done))     ; set up final return address
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))   ; valに b^(n-1) がある
     (goto (reg continue))                   ; 呼出し側に戻る
     base-case
     (assign val (const 1))                  ; base case: 0^ = 1
     (goto (reg continue))                   ; return to caller
     expt-done)))

; b. 反復的べき乗:
(define iterative-expt-machine
  (make-machine
   '(b n continue counter product)
   (list (list '= =) (list '- -) (list '* *))
   '(expt
     (assign counter (reg n))
     (assign product (const 1))
     expt-loop
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-loop))
     expt-done)))

; 動作確認
(set-register-contents! recursive-expt-machine 'b 3)
(set-register-contents! recursive-expt-machine 'n 4)
(start recursive-expt-machine)
(set-register-contents! iterative-expt-machine 'b 3)
(set-register-contents! iterative-expt-machine 'n 4)
(start iterative-expt-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents recursive-expt-machine 'val) 81)
(check-equal? (get-register-contents iterative-expt-machine 'product) 81)
(display 'ok)
