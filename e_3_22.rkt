#lang planet neil/sicp

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))

    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" dispatch))
            (else
             (set-front-ptr! (cdr front-ptr))
             dispatch))) 

    (define (print-queue)
      (display front-ptr)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else (error "Unknown request -- make-queue"
                         m))))
    
    dispatch))



(define q1 (make-queue))
((q1 'insert-queue!) 'a)
(q1 'print-queue)
((q1 'insert-queue!) 'b)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
