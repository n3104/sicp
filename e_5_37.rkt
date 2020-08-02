#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-5-syntax.rkt")
(include "./ch5-compiler.rkt")

(define (preserving-always regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(set! preserving preserving-always)

(define (display-compile-result result)
  (for-each (lambda (x) (display x) (newline))
            (cons (car result) (cons (cadr result) (caddr result)))))

(display-compile-result
 (compile
  '(define (f x)
     (+ x (g (+ x 2))))
  'val
  'next))
