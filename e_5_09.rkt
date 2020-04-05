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

; make-primitive-exp 関数内でラベルを処理して箇所をコメントアウトする。
(define (make-primitive-exp-unsupported-lable exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ;        ((label-exp? exp)
        ;         (let ((insts
        ;                (lookup-label labels
        ;                              (label-exp-label exp))))
        ;           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(set! make-primitive-exp make-primitive-exp-unsupported-lable)

; ラベルを演算 (op) に利用するとエラーになることを確認する。
(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Unknown expression type -- ASSEMBLE {label controller-done}\n"))])
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

; 副作用として (assign ⟨register-name⟩ (label ⟨label-name⟩)) についても内部で make-primitive-exp を利用していたため、アセンブル時にエラーになっていることを確認する。
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Unknown expression type -- ASSEMBLE {label expt-done}\n"))])
  ; 動作確認するため再度計算機を作成する
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
  (true) )

; (assign ⟨register-name⟩ (label ⟨label-name⟩)) については make-assign 関数内部で直接ラベルの展開処理を実装することで動作するように修正する。
(define (make-assign-with-make-label-exp inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               ; make-primitive-exp に実装されていたラベル展開の処理を直接記述した。
               (if (label-exp? (car value-exp))
                   (let ((insts
                          (lookup-label labels
                                        (label-exp-label (car value-exp)))))
                     (lambda () insts))
                   ; reg と const の場合は今まで通り make-primitive-exp を利用する。
                   (make-primitive-exp
                    (car value-exp) machine labels)))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(set! make-assign make-assign-with-make-label-exp)

; 正常に動作することを確認する。
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
