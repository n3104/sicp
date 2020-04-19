#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.14
; まず、make-stack を 5.2.4 のモニター付きに差し替えておく。
(set! make-stack make-stack-with-monitor)

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *) (list 'read read) (list 'newline newline) (list 'print display))
   '(controller
     (assign continue (label fact-done))     ; 最終帰り番地設定
     fact-init
     (perform (op initialize-stack))
     (perform (op newline))
     (assign n (op read))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;;nとcontinueを退避し再帰呼出しを設定する.
     ;; 再帰呼出しから戻る時after-fact}から
     ;; 計算が続行するようにcontinueを設定
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; valに n(n-1)!がある
     (goto (reg continue))                   ; 呼出し側に戻る
     base-case
     (assign val (const 1))                  ; 基底の場合: 1!=1
     (goto (reg continue))                   ; 呼出し側に戻る
     fact-done
     (perform (op print) (reg val))
     (perform (op print-stack-statistics))
     (goto (label fact-init)))))

; 動作確認
(start fact-machine)
