#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

(define factorial-machine
  (make-machine
   '(n continue val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; Fib(n-1)を計算するよう設定
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; nの昔の値を退避
     (assign n (op -) (reg n) (const 1)); nを n-1 に変える
     (goto (label fib-loop))            ; 再帰呼出しを実行
     afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
     (restore n)
     ;; Fib(n-2)を計算するよう設定
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)                         ; Fib(n-1)を退避
     (goto (label fib-loop))
     afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
     (assign n (reg val))               ; nにはFib(n-2)がある
     (restore val)                      ; valにはFib(n-1)がある
     (restore continue)
     (assign val                        ; Fib(n-1)+Fib(n-2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
     immediate-answer
     (assign val (reg n))               ; 基底の場合: Fib(n)=n
     (goto (reg continue))
     fib-done)))

; 動作確認
(set-register-contents! factorial-machine 'n 8)
(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'val) 21)
(display 'ok)
