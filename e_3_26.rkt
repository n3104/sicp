#lang planet neil/sicp

; null を使っていたので定義した。
(define null nil)

; https://wizardbook.wordpress.com/2010/12/16/exercise-3-26/
(define (make-table)
  (define local-table null)
  (define make-record cons)
  (define key-record car)
  (define value-record cdr)
   
  (define (make-tree entry left right) (list entry left right))
  (define entry car)
  (define left-branch cadr)
  (define right-branch caddr)
   
  (define key=? equal?)

  (define (key<? key1 key2)
    ; 型毎に優先順位をつけて比較する。
    (cond ((and (string? key1)
                (string? key2)) (string<? key1 key2))
          ((and (number? key1)
                (number? key2)) (< key1 key2))
          ((and (char? key1)
                (char? key2)) (char<? key1 key2))
          (else (error "Unsupported key types -- KEY<?" key1 key2))))
   
  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x null null))
          ((key=? (key-record x) (key-record (entry set))) set)
          ((key<? (key-record x) (key-record (entry set)))
           ; 既存のtreeを作り直す際に adjoin-set を呼び出すことで、再帰的に新しい要素を適切な位置に格納している。
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          (else 
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))
   
  (define (lookup key records)
    (if (null? records) 
        false
        (let* ((record (entry records))
               (key-entry (key-record record)))
          (cond ((key=? key key-entry) (value-record record))
                ((key<? key key-entry) (lookup key (left-branch records)))
                (else (lookup key (right-branch records)))))))
   
   
  (define (insert! key value)
    (set! local-table 
          (adjoin-set (cons key value)
                      local-table)))
   
; 以下は不要だった
;  (define (list<? l1 l2)
;    (andmap key<? l1 l2))
;
;  (define (element-of-set? x set)
;    (cond ((null? set) false)
;          ((key=? (key-record x) (key-record (entry set))) true)
;          ((key<? (key-record x) (key-record (entry set)))
;           (element-of-set? x (left-branch set)))
;          (else
;           (element-of-set? x (right-branch set))))) 

  (define (dispatch m)
    (cond ((eq? m 'lookup)  (lambda (key) 
                              (lookup key local-table)))
          ((eq? m 'insert!) insert!)
          ((eq? m 'print)   local-table)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch) 


(define t-num (make-table))
(define put-num! (t-num 'insert!))
(define get-num (t-num 'lookup))
(put-num! 100 "100")
(put-num! 300 "300")
(put-num! 900 "900")
(put-num! 700 "700")
(put-num! 200 '(200))
(put-num! 800 '(800))
(put-num! 400 '(400))
(put-num! 600 '(600))
 
(get-num 300)
(get-num 800)
(get-num 700)
(get-num 500)
 
(define t-char (make-table))
(define put-char! (t-char 'insert!))
(define get-char (t-char 'lookup))
(put-char! #\v "v")
(put-char! #\a "a")
(put-char! #\r "r")
 
(get-char #\r)
(get-char #\v)
(get-char #\s)
 
(define t-string (make-table))
(define put-string! (t-string 'insert!))
(define get-string (t-string 'lookup))
(put-string! "key1" "key1")
(put-string! "key7" "key7")
(put-string! "key3" '(4))
 
(get-string "none")
(get-string "key3")
(get-string "key1")
