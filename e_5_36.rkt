#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")

(define (construct-arglist-right-to-left operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(set! construct-arglist construct-arglist-right-to-left)

(define (display-compile-result result)
  (for-each (lambda (x) (display x) (newline))
            (cons (car result) (cons (cadr result) (caddr result)))))

(display-compile-result
 (compile
  '(define (f x)
     (+ x (g (+ x 2))))
  'val
  'next))
