#lang planet neil/sicp

; https://sicp.iijlab.net/fulltext/x411.html
; 組合せ
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply-eval-procedure (eval (operator exp) env)
                               (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; apply
; apply のままだとRacket本体の apply と競合するため名前を変更した。
(define (apply-eval-procedure procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'eval-assignment)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'eval-definition)


; https://sicp.iijlab.net/fulltext/x412.html

; 自己評価式
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; 変数
(define (variable? exp) (symbol? exp))

; クォート式
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; 代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; 仮パラメタ
                   (cddr exp)))) ; 本体

; lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; 導出された式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; else節なし
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; https://sicp.iijlab.net/fulltext/x413.html

; 述語のテスト
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; 手続きの表現
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


; https://sicp.iijlab.net/fulltext/x414.html
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list 'eq? eq?)
        ;⟨基本手続きが続く⟩
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

; setup-environment 内部で primitive-procedure-names と primitive-procedure-objects を利用するため実行タイミングをここにした。
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

; let
; https://wizardbook.wordpress.com/2010/12/26/exercise-4-6/
(define (let-initials exp) 
  (map cadr (cadr exp)))
(define (let-parameters exp)
  (map car (cadr exp)))
(define (let-body exp)
  (cddr exp))
   
; a let is syntactic sugar for
;   ((lambda (params) (body)) values)
(define (let->combination exp)
  (cons (make-lambda (let-parameters exp) 
                     (let-body exp))
        (let-initials exp)))
 
(define (let? exp) (tagged-list? exp 'let))


; 以下、追加実装
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             ; a. 見つけた値が記号 *unassigned* ならエラーとする。
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b.
; https://wizardbook.wordpress.com/2011/01/03/exercise-4-16/
(define (scan-out-defines body)
  (let ((defined-vars (definitions body)))
    (if (null? defined-vars)  
        body
        (list 
         (make-let-seq  
          (unassigned-definitions defined-vars)
          (unassigned-initialisations defined-vars)
          (scanned-body body))))))
 
(define (definitions exp)
  (define (scan-iter body definitions-complete)
    (cond ((null? body) nil)
          ((definition? (car body))
           (if definitions-complete
               (error "define cannot appear in an expression context - DEFINITIONS" exp)
               (cons (car body) 
                     (scan-iter (cdr body) #f))))
          (else (scan-iter (cdr body) #t))))
  (scan-iter exp #f))
 
(define (make-let-seq unassigned-vars initial-values body)
  (append (list 'let unassigned-vars)
          initial-values 
          body))
 
(define (unassigned-definitions define-list)
  (map (lambda (def)  
         (list (definition-variable def) 
               '(quote *unassigned*)))
       define-list))
 
(define (unassigned-initialisations define-list)
  (map (lambda (def)  
         (list 'set! (definition-variable def) 
               (definition-value def)))
       define-list))
 
(define (scanned-body body)
  (cond ((null? body) body)
        ((definition? (car body)) (scanned-body (cdr body))) 
        (else (cons (car body) 
                    (scanned-body (cdr body))))))

; c. scan-out-defines を組み込む。
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; 以下、動作確認
; 回帰テスト
(#%require (only rackunit check-equal?))
(check-equal? (eval '(define x 3) the-global-environment) 'eval-definition)
(check-equal? (eval '(+ x 2) the-global-environment) 5)
(check-equal? (eval '(quote x) the-global-environment) 'x)
(check-equal? (eval '(set! x 5) the-global-environment) 'eval-assignment)
(check-equal? (eval '(+ x 2) the-global-environment) 7)
(check-equal? (eval '(if true x) the-global-environment) 5)
(check-equal? (eval '(begin (+ x 2)(- x 2)) the-global-environment) 3)
(check-equal? (eval '((lambda (y) (+ y y)) x) the-global-environment) 10)
(check-equal? (eval '(cond ((eq? x 5) x)(else false)) the-global-environment) 5)
(check-equal? (eval '(cond ((eq? x 4) x)(else false)) the-global-environment) false)
(check-equal? (eval '(let ((y x)) (+ y y)) the-global-environment) 10)
(check-equal? (eval '(define (sum a b) (+ a b)) the-global-environment) 'eval-definition)
(check-equal? (eval '(eq? (sum 2 3) 5) the-global-environment) true)

; 追加実装箇所
; a. 見つけた値が記号 *unassigned* ならエラーとなることを確認する。 
(check-equal? (eval '(set! x '*unassigned*) the-global-environment) 'eval-assignment)
(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Unassigned variable x\n"))])
  (eval '(eq? x '*unassigned*) the-global-environment))

; b. scan-out-defines の変換結果が期待通りになることを確認する。
; まず変換後の式が実際に動作することを確認しておく。
(check-equal? (eval '(let ((a '*unassigned*) (b '*unassigned*)) (set! a 2) (set! b 3) (+ a b)) the-global-environment) 5)
(check-equal? (scan-out-defines '((define a 2) (define b 3) (+ a b)))
              '((let ((a '*unassigned*) (b '*unassigned*)) (set! a 2) (set! b 3) (+ a b))))

; c. まとめて動作するか確認する。
; 式の変換結果が期待通りになることを確認する。
(check-equal? (eval '(lambda () (define a 2) (define b 3) (+ a b)) the-global-environment)
              (list 'procedure (list) '((let ((a '*unassigned*) (b '*unassigned*)) (set! a 2) (set! b 3) (+ a b))) the-global-environment))
; 実際に動作することを確認する。
(check-equal? (eval '(define (five) (define a 2) (define b 3) (+ a b)) the-global-environment) 'eval-definition)
(check-equal? (eval '(eq? (five) 5) the-global-environment) true)

(display 'ok)