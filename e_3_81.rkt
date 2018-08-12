#lang planet neil/sicp

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

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 以下、追加実装
; https://wizardbook.wordpress.com/2010/12/23/exercise-3-81/
(define initial-seed 317)
(define (rand-update)
  (random (expt 2 31)))
 
(#%require (only racket/base random-seed))
(define (random-init seed)
  (random-seed seed)
  (rand-update))

(define (random-stream seed)
  (define random-from
    (cons-stream (random-init seed)
                 (stream-map (lambda (x) (rand-update)) random-from)))
  random-from)

(define (rand seed requests)
  (define (rand-iter randoms actions)
    (if (stream-null? actions) 
        the-empty-stream
        (let ((request (stream-car actions)))
          (cond ((eq? 'generate request)
                 (cons-stream (stream-car randoms)
                              (rand-iter (stream-cdr randoms) 
                                         (stream-cdr actions))))
                ((eq? 'reset request)
                 (let ((new-randoms (random-stream (random-init seed)))) 
                   (cons-stream (stream-car new-randoms)
                                (rand-iter (stream-cdr new-randoms) 
                                           (stream-cdr (stream-cdr actions))))))
                (else (error "RAND -- unknown request" request))))))
  (rand-iter (random-stream (random-init seed)) 
             requests))

; 以下、動作確認
(#%require (only racket/base for))
(#%require (only racket/stream stream))
(define seed 1)
(define (list->stream items)
  (if (null? items)
      the-empty-stream
      (cons-stream
       (car items)
       (list->stream (cdr items)))))

(display-stream (rand seed (list->stream (list 'generate 'generate 'generate))))
(display-stream (rand seed (list->stream (list 'generate 'generate 'generate 'reset 'generate 'generate 'generate))))
