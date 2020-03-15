#lang planet neil/sicp

(#%require (only racket include))
(include "./ch5-regsim.rkt")

; 問題 5.8
(define factorial-machine
  (make-machine
   '(a)
   nil
   '(controller
     start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

; これまでに書いたシミュレータでは, 制御がthereに達した時レジスタa の内容はどうなるか
(start factorial-machine)
(#%require (only rackunit check-equal?))
(check-equal? (get-register-contents factorial-machine 'a) 3)

; extract-labels手続きを修正し, 同じラベル名が二つの異る場所を指すように使われたら, エラーとするようにせよ
(define (extract-labels-with-check text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              ; ラベルの重複チェック
                              (if (assoc next-inst labels)
                                  (error "Multiply defined lable:" next-inst)
                                  (receive insts
                                           (cons (make-label-entry next-inst
                                                                   insts)
                                                 labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))
; 動作確認するため定義を差し替える
(set! extract-labels extract-labels-with-check)

(#%require (only racket/base with-handlers))
(#%require (only racket/base exn:fail?))
(#%require (only racket/exn exn->string))
(with-handlers ([exn:fail? (lambda (exn) (check-equal? (exn->string exn) "Multiply defined lable: here\n"))])
  ; 動作確認するため再度計算機を作成する
  (define factorial-machine-with-lable-duplicaiotn-error
    (make-machine
     '(a)
     nil
     '(controller
       start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there)))
  ; 実行するためのダミーの処理
  (true) )

; ラベル名が異なる場合はエラーになならないことを確認する
(define factorial-machine-without-error
  (make-machine
   '(a)
   nil
   '(controller
     start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     everywhere ; 変更した
     (assign a (const 4))
     (goto (label there))
     there)))
(display 'ok)
