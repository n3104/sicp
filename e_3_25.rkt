#lang planet neil/sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (sub-lookup keys table)
      (if (not (pair? table))
          false ; keyが実際に登録されているテーブルよりも次元が多い場合の入力チェック
          (if (null? (cdr keys))
              (let ((record (assoc (car keys) table)))
                (if record
                    (cdr record)
                    false))
              (let ((subtable (assoc (car keys) table)))
                (if subtable
                    (sub-lookup (cdr keys) (cdr subtable))
                    false)))))
    (define (lookup keys)
      (sub-lookup keys (cdr local-table)))
    (define (sub-insert! keys value table)
      (if (null? (cdr keys))
          (let ((record (assoc (car keys) (cdr table))))
            (if record
                (set-cdr! record value)
                (set-cdr! table
                          (cons (cons (car keys) value)
                                (cdr table)))))
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (sub-insert! (cdr keys) value subtable)
                (set-cdr! table (cons (cons (car keys) (make-subtables (cdr keys) value)) (cdr table)))))))
    (define (make-subtables keys value)
      (if (null? keys)
          value
          (cons (cons (car keys) (make-subtables (cdr keys) value)) nil)))
    (define (insert! keys value)
      (sub-insert! keys value local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(a x) 1)
(get '(a x))
(get '(b x))
(get '(a y))
(get '(a))
(get '(a x y))

(put '(a b c) 2)
(get '(a b c))
(get '(a x))

(put '(a) 3)
(get '(a))
(get '(a x)) ; a がテーブルからレコードに上書きされたため false になる。
(get '(a x y))
