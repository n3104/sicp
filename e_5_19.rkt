#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.19
(define (extract-labels2 text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (begin
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
        ((eq? (car inst) 'label)
         (lambda ()(advance-pc pc)))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(set! make-execution-procedure make-execution-procedure2)

(define (make-new-machine-with-breakpoint)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (current-lable '*unassigned*)
        (current-lable-counter 0)
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
                (if (eq? (caaar insts) 'label)
                    (begin
                      (set! current-lable-counter 0)
                      (if (not (eq? (cdaar insts) current-lable))
                          (set! current-lable (cdaar insts))))
                    (set! current-lable-counter (+ current-lable-counter 1)))
                (newline)
                (display (list current-lable '= current-lable-counter (car insts)))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(set! make-new-machine make-new-machine-with-breakpoint)

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
     factorial-done
     (assign counter (const 1))
     )))

(set-register-contents! factorial-machine 'n 5)
(set-register-contents! factorial-machine 'product 1)
(set-register-contents! factorial-machine 'counter 1)
(start factorial-machine)

(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'product) 120)
(display 'ok)
