#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.11
; b. (restore y)は, スタックに退避した最後の値を, それがyから退避された時だけ, yに置き, それ以外はエラーとする. シミュレータをこのように振舞うよう修正せよ. スタックに値と一緒にレジスタ名を置くよう, saveを変更しなければならない.

; まずは初期状態の確認する。変数に共通のスタックであるため最後に save した x の値が y に保存されている。
(define test-machine
  (make-machine
   '(x y)
   (list nil)
   '(controller
     (assign x (const 1))
     (assign y (const 2))
     (save y)
     (save x)
     (assign y (const 3))
     (restore y)
     controller-done)))

(start test-machine)
(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents test-machine 'y) 1)

; make-save で値を保存する際に (reg-name value) の pair に変更する。
(define (make-save-with-reg-name inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

; make-restore で値を取得する際に reg-name が一致しているかチェック処理を追加する。
(define (make-restore-with-reg-name-validation inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((entry (pop stack)))
        (if (not (eq? (car entry) (stack-inst-reg-name inst)))
            (error "Invalid register name to restore. register:" (stack-inst-reg-name inst))
            (set-contents! reg (cdr entry)))
        (advance-pc pc)))))

(set! make-save make-save-with-reg-name)
(set! make-restore make-restore-with-reg-name-validation)

; save 時のレジスタ名と restore 時のレジスタが一致しない際にエラーになることを確認する。
(define test-machine-with-reg-name-validation-error
  (make-machine
   '(x y)
   (list nil)
   '(controller
     (assign x (const 1))
     (assign y (const 2))
     (save y)
     (save x)
     (assign y (const 3))
     (restore y)
     controller-done)))

(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Invalid register name to restore. register: y\n"))])
  (start test-machine-with-reg-name-validation-error)
  )

; save 時のレジスタ名と restore 時のレジスタが一致する場合は正常に動作することを確認する。
(define test-machine-with-reg-name-validation-success
  (make-machine
   '(x y)
   (list nil)
   '(controller
     (assign x (const 1))
     (assign y (const 2))
     (save y)
     (save x)
     (assign y (const 3))
     (restore x) ; この行を追加して一致位するようにした。
     (restore y)
     controller-done)))

(start test-machine-with-reg-name-validation-success)
(check-equal? (get-register-contents test-machine-with-reg-name-validation-success 'x) 1)
(check-equal? (get-register-contents test-machine-with-reg-name-validation-success 'y) 2)

(display 'ok)
