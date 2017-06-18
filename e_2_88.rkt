#lang planet neil/sicp

; http://sicp.iijlab.net/fulltext/x333.html
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
;      'ok
      )    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; https://wizardbook.wordpress.com/2010/12/08/exercise-2-78/
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) 
      contents
      (cons type-tag contents)))
 
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
 
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) cdr datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) zero?)
  (put 'raise  '(scheme-number) (lambda (n) (make-rational n 1)))
  
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-rational-package)
   ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x) (zero? (numer x)))
  (define (real->complex r)
    (define (rational->real r) (/ (numer r) (denom r)))
    (make-complex-from-real-imag (rational->real r) 0)) 
  (define (rational->scheme-number r) 
    (make-scheme-number (truncate (/ (numer r) (denom r)))))

   ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) real->complex)
  (put 'project '(rational) (lambda (x) (rational->scheme-number x)))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)
;(make-rational 3 2)

(define (install-complex-package)
   ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

   ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z1) (zero? (magnitude z1)))
  (define (real->rational r)
    (let ((exact (inexact->exact r)))
      (cond ((integer? exact)  (make-rational exact 1))
            ((rational? exact) (make-rational (numerator exact) (denominator exact)))
            (else (make-rational (truncate exact) 1)))))
  (define (complex->real z1) (real-part z1))

   ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) (lambda (x) (real->rational (complex->real x))))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; http://sicp.iijlab.net/fulltext/x114.html#index151
(define (square x) (* x x))

; http://sicp.iijlab.net/fulltext/x243.html
(define (install-rectangular-package)
   ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
   ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

(define (type-level z) (apply-generic 'type-level z))
(put 'type-level '(scheme-number) (lambda (x) 1))
(put 'type-level '(rational) (lambda (x) 2))
(put 'type-level '(complex) (lambda (x) 3))

; https://wizardbook.wordpress.com/2010/12/08/exercise-2-85/
(define (apply-generic op . args)
   
  ; only certain operations will result in an answer that can be
  ; projected e.g. it makes no sense to project the answer to zero?
  (define (reduce-type x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          (else x)))
   
  ; find the highest type level of a list of arguments
  (define (highest-type-level args)
    (if (null? args) 
        0
        (let ((level (type-level (car args)))
              (highest (highest-type-level (cdr args))))
          (if (> level highest)
              level
              highest))))
   
  ; raise arg to the same level as target-type-level
  (define (raise-to arg target-type-level)
    (define (raise-iter current-arg)   
      (let ((arg-level (type-level current-arg)))
        (cond ((= arg-level target-type-level) current-arg)
              ((< arg-level target-type-level) (raise-iter (apply-generic 'raise current-arg)))
              (else (error "Cannot raise argument to a lower type target" arg target-type-level)))))
    (raise-iter arg))
   
  ; raise all args to a common type (the highest in the tower of types)
  ; and apply the operator to them 
  (define (apply-with-raised-types args)
    (let ((target-type-level (highest-type-level args)))
      (apply apply-generic 
             op 
             (map (lambda (arg)
                    (raise-to arg target-type-level))
                  args))))
   
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc 
        (reduce-type (apply proc (map contents args)))
        (apply-with-raised-types args))))


(define (project z)    (apply-generic 'project z))
(define (drop z)
  ; drop が 'polynomial の場合も呼ばれるので drop 対象の型でない場合はそのまま返す処理を追加した。これを入れないと無限ループになるようでメモリ不足で異常終了した。
  (if (not (or (eq? (type-tag z) 'scheme-number)
               (eq? (type-tag z) 'rational)
               (eq? (type-tag z) 'complex)))
      z
      (if (= (type-level z) 1) 
          z
          (let ((projected (project z)))
        
            (if (equ? z (raise projected))
                (drop projected)
                z)))))

; http://sicp.iijlab.net/fulltext/x253.html
(define (install-polynomial-package)
   ;; 内部手続き
   ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; ⟨2.3.2節の手続きsame-variable?とvariable?⟩
  ; http://sicp.iijlab.net/fulltext/x232.html
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; 項と項リストの表現
  ;⟨以下の本文にある手続きadjoin-term ...coeff⟩
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ; term-listの全coeffが zero? を満たすかチェックするようにした。
  (define (=zero-poly? p)
    (define (=zero-term-list? L)
      (if (empty-termlist? L)
          true ; 全要素が zero? を満たした際は true を返す。
          (if (zero? (coeff (first-term L)))
              (=zero-term-list? (rest-terms L)) ; 後続の要素を再帰的にチェックする。
              false))) ; zero? を満たさない要素があればその時点で処理を打ち切り false を返す。
    (=zero-term-list? (term-list p)))
  
   ;; システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ; 問題 2.88
  ; polynomialについてはパッケージ内部の関数を多く利用するのでパッケージに記述した。
  (define (negate-term-list L)
    (if (empty-termlist? L)
        (the-empty-termlist)
    (let ((t (first-term L)))
      (adjoin-term
        (make-term (order t) (negate (coeff t)))
        (negate-term-list (rest-terms L))))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-term-list (term-list p))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  'done)

(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; 問題 2.88
; 各パッケージに記述する内容だが分散するのでまとめてトップレベルに記述した。
(define (negate x) (apply-generic 'negate x))
(put 'negate '(scheme-number) (lambda (n) (* n -1)))
(define (negate-rat r)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (make-rational (negate (numer r)) (denom r)))
(put 'negate '(rational) (lambda (r) (negate-rat r)))
(define (negate-complex c)
  (make-complex-from-real-imag (negate (real-part c)) (negate (imag-part c))))
(put 'negate '(complex) (lambda (r) (negate-complex r)))


; 動作確認
(equ? (negate 3) -3)
(equ? (negate -3) 3)
(equ? (negate (make-rational 3 2)) (make-rational -3 2))
(equ? (negate (make-rational -3 2)) (make-rational 3 2))
(equ? (negate (make-rational 3 -2)) (make-rational -3 -2))
(equ? (negate (make-rational -3 -2)) (make-rational 3 -2))
(equ? (negate (make-complex-from-real-imag 3 2)) (make-complex-from-real-imag -3 -2))
(equ? (negate (make-complex-from-real-imag -3 -2)) (make-complex-from-real-imag 3 2))
; equ? は未実装なのでそのまま出力する。
(negate (make-polynomial 'x '((1 1)(0 3))))
(negate (make-polynomial 'x '((1 -1)(0 -3))))
(sub (make-polynomial 'x '((1 2)(0 2))) (make-polynomial 'x '((1 1)(0 3))))