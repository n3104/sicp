#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")
(set! make-stack make-stack-with-monitor)

(include "./ch5-syntax.rkt")
(include "./ch5-eceval-support.rkt")

(define (lexical-address-lookup address env)
  (define (lookup-frame frame-number env)
    (define (frame-itr counter env)
      (if (eq? env the-empty-environment)
          (error "Unbound variable" address)
          (let ((frame (first-frame env)))
            (if (eq? counter frame-number)
                frame
                (frame-itr (+ counter 1) (enclosing-environment env))))))
    (frame-itr 0 env))
  (define (lookup-value displacement-number frame)
    ; 変位数は変数がFrameに追加された順番になっているが、実際に探索する際は最後に追加された変数から探索するためインデックスに変換している。
    (let ((displacement-index (- (length (frame-variables frame)) displacement-number 1)))
      (define (variable-itr counter vars vals)
        (cond ((null? vars)
               (error "Unbound variable" address))
              ((eq? counter displacement-index)
               (let ((value (car vals)))
                 (if (eq? value '*unassigned*)
                     (error "Unbound variable" address)
                     value)))
              (else
               (variable-itr (+ counter 1) (cdr vars) (cdr vals)))))
      (variable-itr 0 (frame-variables frame) (frame-values frame))))
  (let ((frame-number (car address)) (displacement-number (cadr address)))
    (let ((frame (lookup-frame frame-number env)))
      (lookup-value displacement-number frame))))

(define (lexical-address-set! address val env)
  (define (lookup-frame frame-number env)
    (define (frame-itr counter env)
      (if (eq? env the-empty-environment)
          (error "Unbound variable" address)
          (let ((frame (first-frame env)))
            (if (eq? counter frame-number)
                frame
                (frame-itr (+ counter 1) (enclosing-environment env))))))
    (frame-itr 0 env))
  (define (set-value displacement-number frame)
    (let ((displacement-index (- (length (frame-variables frame)) displacement-number 1)))
      (define (variable-itr counter vars vals)
        (cond ((null? vars)
               (error "Unbound variable" address))
              ((eq? counter displacement-index)
               (set-car! vals val))
              (else
               (variable-itr (+ counter 1) (cdr vars) (cdr vals)))))
      (variable-itr 0 (frame-variables frame) (frame-values frame))))
  (let ((frame-number (car address)) (displacement-number (cadr address)))
    (let ((frame (lookup-frame frame-number env)))
      (set-value displacement-number frame))))

; 動作確認
(#%require (only rackunit check-equal?))
; Frameが1つあるケース
(define frame-a (extend-environment '() '() the-empty-environment))
(define-variable! 'a 1 frame-a)
(define-variable! 'b 2 frame-a)
(define-variable! 'c 3 frame-a)

(check-equal? (lookup-variable-value 'a frame-a) 1)
(check-equal? (lookup-variable-value 'b frame-a) 2)
(check-equal? (lookup-variable-value 'c frame-a) 3)

(check-equal? (lexical-address-lookup '(0 0) frame-a) 1)
(check-equal? (lexical-address-lookup '(0 1) frame-a) 2)
(check-equal? (lexical-address-lookup '(0 2) frame-a) 3)

(lexical-address-set! '(0 0) 11 frame-a)
(lexical-address-set! '(0 1) 12 frame-a)
(lexical-address-set! '(0 2) 13 frame-a)

(check-equal? (lookup-variable-value 'a frame-a) 11)
(check-equal? (lookup-variable-value 'b frame-a) 12)
(check-equal? (lookup-variable-value 'c frame-a) 13)

; Frameが2つあるケース
(define frame-b (extend-environment '() '() frame-a))
(define-variable! 'a 1 frame-b)
(define-variable! 'b 2 frame-b)
(define-variable! 'c 3 frame-b)

(check-equal? (lookup-variable-value 'a frame-b) 1)
(check-equal? (lookup-variable-value 'b frame-b) 2)
(check-equal? (lookup-variable-value 'c frame-b) 3)

(check-equal? (lexical-address-lookup '(0 0) frame-b) 1)
(check-equal? (lexical-address-lookup '(0 1) frame-b) 2)
(check-equal? (lexical-address-lookup '(0 2) frame-b) 3)
(check-equal? (lexical-address-lookup '(1 0) frame-b) 11)
(check-equal? (lexical-address-lookup '(1 1) frame-b) 12)
(check-equal? (lexical-address-lookup '(1 2) frame-b) 13)

(lexical-address-set! '(0 0) 11 frame-b)
(lexical-address-set! '(0 1) 12 frame-b)
(lexical-address-set! '(0 2) 13 frame-b)
(lexical-address-set! '(1 0) 111 frame-b)
(lexical-address-set! '(1 1) 112 frame-b)
(lexical-address-set! '(1 2) 113 frame-b)

(check-equal? (lookup-variable-value 'a frame-b) 11)
(check-equal? (lookup-variable-value 'b frame-b) 12)
(check-equal? (lookup-variable-value 'c frame-b) 13)
(check-equal? (lookup-variable-value 'a frame-a) 111)
(check-equal? (lookup-variable-value 'b frame-a) 112)
(check-equal? (lookup-variable-value 'c frame-a) 113)

; '*unassigned* のテスト
(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))

(define-variable! 'd '*unassigned* frame-b)
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Unbound variable {0 4}\n"))])
  (lexical-address-lookup '(0 4) frame-b))