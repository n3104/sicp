#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.9
; まずは実際にラベルに演算を許している計算機を作成する。
(define test-machine
  (make-machine
   '(val)
   (list (list 'cons cons))
   '(controller
     (assign val (op cons) (label controller-done) (label controller-done))
     controller-done)))

(start test-machine)
(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents test-machine 'val) (cons nil nil))

; make-operation-exp 関数内でラベルの場合はエラーにする。
(define (make-operation-exp-unsupported-lable exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "op instruction dose not support lable -- ASSEMBLE" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(set! make-operation-exp make-operation-exp-unsupported-lable)

; ラベルを演算 (op) に利用するとエラーになることを確認する。
(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "op instruction dose not support lable -- ASSEMBLE {label controller-done}\n"))])
  ; 動作確認するため再度計算機を作成する
  (define test-machine-unsupported-lable
    (make-machine
     '(val)
     (list (list 'cons cons))
     '(controller
       (assign val (op cons) (label controller-done) (label controller-done))
       controller-done)))
  ; 実行するためのダミーの処理
  (true) )

; デグレの確認として recursive-expt-machine は正常に動作することを確認する。
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
(set-register-contents! recursive-expt-machine 'b 3)
(set-register-contents! recursive-expt-machine 'n 4)
(start recursive-expt-machine)
(check-equal? (get-register-contents recursive-expt-machine 'val) 81)
(display 'ok)
