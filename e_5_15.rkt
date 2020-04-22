#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.15
(define (make-new-machine-with-number-instructions)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (number-instructions 0) ; 命令計数(instruction counting)用
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ; 命令回数の値を印字し, 回数を零に設定するメッセージの実装
                 (list 'initialize-number-instructions
                       (lambda () (set! number-instructions 0)))
                 (list 'print-number-instructions
                       (lambda ()
                         (begin
                           (newline)
                           (display (list 'number-instructions  '= number-instructions)))))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (set! number-instructions (+ 1 number-instructions)) ; execute が実行される都度カウントアップする。
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(set! make-new-machine make-new-machine-with-number-instructions)

; 問題 5.14 のfact-machineをベースにした。
(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *) (list 'read read) (list 'newline newline) (list 'print display))
   '(controller
     (assign continue (label fact-done))     ; 最終帰り番地設定
     fact-init
     (perform (op initialize-number-instructions))
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
     (perform (op print-number-instructions))
     (goto (label fact-init)))))

; 動作確認
(start fact-machine)
