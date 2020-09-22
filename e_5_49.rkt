#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")
(set! make-stack make-stack-with-monitor)

(include "./ch5-syntax.rkt")
(include "./ch5-compiler.rkt")
(include "./ch5-5-eceval-support.rkt")
;(include "./ch5-eceval-compiler.rkt")

;;;;;;;;
;ch5-eceval-compiler.rkt
;;;;;;;;

;; any old value to create the variable so that
;;  compile-and-go and/or start-eceval can set! it.
(define the-global-environment '())

;;; Interfacing compiled code with eceval machine
;;; From section 5.5.7
(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

;; Modification of section 4.1.4 procedure
;; **replaces version in syntax file
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))


(define (compile-and-run expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
;    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))


;;**NB. To [not] monitor stack operations, comment in/[out] the line after
;; print-result in the machine controller below
;;**Also choose the desired make-stack version in regsim.scm

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)			;used by eceval

   ;;used by compiled code
   (list 'list list)
   (list 'cons cons)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'false? false?)		;for compiled code
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment)

   ;;for compiled code (also in eceval-support.scm)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)

   (list 'compile-and-run compile-and-run)
   ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev
	 )
   eceval-operations
  '(
;;*next instruction supports entry from compiler (from section 5.5.7)
  (branch (label external-entry))
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (perform (op compile-and-run) (reg exp))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;*support for entry from compiler (from section 5.5.7)
external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
   )))


'(EXPLICIT CONTROL EVALUATOR FOR COMPILER LOADED)


(start-eceval)

; 実行結果
;;;; EC-Eval input:
;(define (factorial n)
;    (if (= n 1)
;        1
;        (* (factorial (- n 1)) n)))
;
;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(factorial 5)
;
;(total-pushes = 26 maximum-depth = 14)
;;;; EC-Eval value:
;120
;
;;;; EC-Eval input:
;(define (factorial-5)
;    (factorial 5))
;
;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(factorial-5)
;
;(total-pushes = 26 maximum-depth = 14)
;;;; EC-Eval value:
;120
