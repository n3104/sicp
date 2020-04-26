#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.18
(define (make-register-with-tracing name)
  (let ((contents '*unassigned*)
        (trace-enabled false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (begin
                 ; レジスタをトレースする時は, レジスタへの値の代入では, レジスタ名, レジスタの古い内容と代入する新しい内容を印字する.
                 (cond (trace-enabled
                        (newline)
                        (display (list 'register-name '= name 'old-value '= contents 'new-value '= value))))
                 (set! contents value))))
            ; レジスタはトレースを開始と停止するメッセージを受け入れる. 
            ((eq? message 'enable-tracing)
             (set! trace-enabled true))
            ((eq? message 'disable-tracing)
             (set! trace-enabled false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(set! make-register make-register-with-tracing)

(define (make-new-machine-with-register-tracing)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
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
              ; 計算機モデルのインターフェースを拡張し, 指示した計算機レジスタのトレースの開始と停止が出来るようにせよ.
              ((eq? message 'enable-register-tracing)
               (lambda (name) ((lookup-register name) 'enable-tracing)))
              ((eq? message 'disable-register-tracing)
               (lambda (name) ((lookup-register name) 'disable-tracing)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(set! make-new-machine make-new-machine-with-register-tracing)

; 動作確認
(define factorial-machine
  (make-machine
   '(n product counter)
   (list (list '> >) (list '* *) (list '+ +))
   '(factorial
     (assign product (const 1))
     (assign counter (const 1))
     test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label factorial-done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))
     factorial-done)))

; まずは product レジスタのトレースを開始して、挙動を確認する。
((factorial-machine 'enable-register-tracing) 'product)
(set-register-contents! factorial-machine 'n 5)
(set-register-contents! factorial-machine 'product 1)
(set-register-contents! factorial-machine 'counter 1)
(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'product) 120)

; 次に product レジスタのトレースを停止し、代わり counter レジスタのトレースを開始して、挙動を確認する。
((factorial-machine 'disable-register-tracing) 'product)
((factorial-machine 'enable-register-tracing) 'counter)
(set-register-contents! factorial-machine 'n 5)
(set-register-contents! factorial-machine 'product 1)
(set-register-contents! factorial-machine 'counter 1)
(start factorial-machine)

(check-equal? (get-register-contents factorial-machine 'product) 120)
(display 'ok)
