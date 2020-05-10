#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; https://sicp.iijlab.net/fulltext/x223.html#index934
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; https://sicp.iijlab.net/fulltext/x223.html#index968
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; 問題 5.19
(define (extract-labels2 text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (begin
                                ; ラベル位置を取得できるように、ラベルも命令行として追加する。
                                (let ((next-insts (cons (make-instruction (list 'label next-inst))
                                                        insts))) 
                                  (receive next-insts
                                           (cons (make-label-entry next-inst
                                                                   next-insts)
                                                 labels))))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(set! extract-labels extract-labels2)

(define (make-execution-procedure2 inst labels machine
                                   pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ; ラベルの場合は何もせずに次の命令に進める。
        ((eq? (car inst) 'label)
         (lambda ()(advance-pc pc)))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(set! make-execution-procedure make-execution-procedure2)

(define (make-new-machine-with-breakpoint)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ; ブレークポイントを管理するため現在のラベルとラベルとブレークポイントの距離を管理する変数を追加する。
        (current-lable '*unassigned*)
        (current-lable-counter 0)
        (breakpoint-table '())
        (breaking #f)
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
      ; ブレークポイントの操作用関数
      (define (add-breakpoint label n)
        (let ((breakpoint-entry (assoc label breakpoint-table)))
          (if breakpoint-entry
              (set! breakpoint-table
                    (cons (list label (cons n (cadr breakpoint-entry)))
                          breakpoint-table))
              (set! breakpoint-table
                    (cons (list label (list n))
                          breakpoint-table))))
        'breakpoint-added)
      (define (cancel-breakpoint label n)
        (let ((breakpoint-entry (assoc label breakpoint-table)))
          (if breakpoint-entry
              (set! breakpoint-table
                    (cons (list label (remove n (cadr breakpoint-entry)))
                          breakpoint-table))))
        'breakpoint-deleted)
      (define (breakpoint?)
        (let ((breakpoint-entry (assoc current-lable breakpoint-table)))
          (if (and (not breaking) breakpoint-entry)
              (if (member current-lable-counter (cadr breakpoint-entry))
                  true
                  false)
              false)))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ; ラベルとブレークポイントの距離の管理
                (if (not breaking)
                    (if (eq? (caaar insts) 'label)
                        (begin
                          (set! current-lable-counter 0)
                          (if (not (eq? (cadaar insts) current-lable))
                              (set! current-lable (cadaar insts))))
                        (set! current-lable-counter (+ current-lable-counter 1))))
                (if (breakpoint?)
                    ; ラベルとブレークポイントの距離を印字し, 命令の実行を停止する
                    (begin
                      (set! breaking true)
                      (newline)
                      (display (list current-lable '= current-lable-counter)))
                    (begin
                      (set! breaking false)
                      ((instruction-execution-proc (car insts)))
                      (execute)))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'proceed-machine)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'add-breakpoint) add-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (lambda () (set! breakpoint-table '())))
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(set! make-new-machine make-new-machine-with-breakpoint)

(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n)
  'done)

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n)
  'done)

(define (cancel-all-breakpoints machine)
  ((machine 'cancel-all-breakpoints))
  'done)

(define (proceed-machine machine)
  (machine 'proceed-machine)
  'done)

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

(set-register-contents! factorial-machine 'n 6)
(set-breakpoint factorial-machine 'test-counter 5) ; ループ毎の product と counter のレジスタの値を参照する。
(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'product) 1)
(check-equal? (get-register-contents factorial-machine 'counter) 2)

(proceed-machine factorial-machine)
(check-equal? (get-register-contents factorial-machine 'product) 2)
(check-equal? (get-register-contents factorial-machine 'counter) 3)

(set-breakpoint factorial-machine 'test-counter 4) ; ブレークポイントを2つ登録できることを確認する。
(proceed-machine factorial-machine)
(check-equal? (get-register-contents factorial-machine 'product) 6)
(check-equal? (get-register-contents factorial-machine 'counter) 3) ; 距離が 4 になったので counter の値がインクリメント前の値になっている。

(proceed-machine factorial-machine)
(check-equal? (get-register-contents factorial-machine 'product) 6)
(check-equal? (get-register-contents factorial-machine 'counter) 4) ; 距離が 5 の位置になったので counter の値がインクリメント後の値になっている。

(cancel-breakpoint factorial-machine 'test-counter 4) ; 複数のブレークポイントから1つを削除できることを確認する。
(proceed-machine factorial-machine)
(check-equal? (get-register-contents factorial-machine 'product) 24)
(check-equal? (get-register-contents factorial-machine 'counter) 5)

(cancel-breakpoint factorial-machine 'test-counter 5) ; 1だけのブレークポイントを削除できることを確認する。
(proceed-machine factorial-machine)
(check-equal? (get-register-contents factorial-machine 'product) 720) ; ブレークポイントがすべて削除されたので最後まで処理が進み n = 7 => 720 の結果になる。
(check-equal? (get-register-contents factorial-machine 'counter) 7)


(define factorial-machine2
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

(set-register-contents! factorial-machine2 'n 6)
(set-breakpoint factorial-machine2 'factorial 2)
(start factorial-machine2)

(set-register-contents! factorial-machine2 'product 2) ; ブレークポイント中にレジスタの値を書き換えられることを確認する。
(set-breakpoint factorial-machine2 'test-counter 5) ; 複数のラベルを扱えることを確認する。
(proceed-machine factorial-machine2)
(check-equal? (get-register-contents factorial-machine2 'product) 2) ; レジスタの値を 1 -> 2 に書き換えていたので 1 * 2 => 2 になっている。
(check-equal? (get-register-contents factorial-machine2 'counter) 2)

(cancel-all-breakpoints factorial-machine2) ; すべてのブレークポイントを削除できることを確認する。
(proceed-machine factorial-machine2)
(check-equal? (get-register-contents factorial-machine2 'product) 1440) ; 1 * 720 => 1440 になっている。
(check-equal? (get-register-contents factorial-machine2 'counter) 7)

(display 'ok)
