#lang planet neil/sicp
(#%require (only racket/base for))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-line x)
  (display x)
  (newline))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car 
                            (merge-weighted (stream-cdr s1) 
                                            s2 
                                            weight))
               (cons-stream s2car 
                            (merge-weighted s1 
                                            (stream-cdr s2) 
                                            weight)))))))
 
(define (weighted-pairs s t weight) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

; 以下、追加実装
(define (square-numbers)
  (define (sum-squared x)
    (let ((i (car x)) (j (cadr x)))
      (+ (* i i ) (* j j ))))
  (define (squares all-sum-squares)
    (let* ((current (stream-car all-sum-squares))
           (next (stream-car (stream-cdr all-sum-squares)))
           (after-the-next (stream-car (stream-cdr (stream-cdr all-sum-squares))))
           (square-candidate (sum-squared current)))
      (cond ((and
              (= square-candidate (sum-squared next))
              (= square-candidate (sum-squared after-the-next))
              )
             (cons-stream (list square-candidate current next after-the-next)
                          (squares (stream-cdr (stream-cdr all-sum-squares)))))
            (else (squares (stream-cdr all-sum-squares))))))
  (squares (weighted-pairs integers 
                           integers 
                           sum-squared)))

; 以下、動作確認
(for ([index 6])
  (display-line
   (stream-ref
    (square-numbers)
    index)))