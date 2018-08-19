#lang planet neil/sicp

; https://sicp.iijlab.net/fulltext/x412.html
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; https://sicp.iijlab.net/fulltext/x411.html
(define (eval exp env)
  ; 今回の動作確認用
  (display exp)
  (newline)
  (cond ((self-evaluating? exp) exp)
; 今回と関係ない部分はコメントアウトした。
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
;        ((begin? exp) 
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp)
;         (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; 以下、追加実装
(define (list-of-values-left exps env)
  (if (no-operands? exps)
      '()
      (let ((left-exps (eval (first-operand exps) env)))
        (cons left-exps
              (list-of-values-left (rest-operands exps) env)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      '()
      (let ((right-exps (list-of-values-right (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              right-exps))))


; 以下、動作確認
(list-of-values-left '(1 2 3 4) nil)
(list-of-values-right '(1 2 3 4) nil)