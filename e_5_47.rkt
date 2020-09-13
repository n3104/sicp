#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")
(set! make-stack make-stack-with-monitor)

(include "./ch5-syntax.rkt")
(include "./ch5-compiler.rkt")
(include "./ch5-5-eceval-support.rkt")
(include "./ch5-eceval-compiler.rkt")


(define (compile-procedure-call-with-interpreted-procedures target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compound-branch (make-label 'compound-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (make-instruction-sequence '(proc) '()
        `((test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compound-branch
          (compound-proc-appl target compiled-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage linkage
           (make-instruction-sequence '(proc argl)
                                      (list target)
            `((assign ,target
                      (op apply-primitive-procedure)
                      (reg proc)
                      (reg argl))))))))
       after-call))))

(set! compile-procedure-call compile-procedure-call-with-interpreted-procedures)

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (save continue)
             (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          `((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(compile-and-go
 '(define (f)
    (g)))

; 以下動作確認手順とその結果
;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(define (g) 3)
;
;(total-pushes = 3 maximum-depth = 3)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(f)
;
;(total-pushes = 4 maximum-depth = 3)
;;;; EC-Eval value:
;3
