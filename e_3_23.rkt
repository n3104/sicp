#lang planet neil/sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (front-insert-queue! queue item)
  (let ((new-front (cons (cons item nil) (front-ptr queue))))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-front)
           (set-rear-ptr! queue new-front)
           queue)
          (else
           (set-cdr! (car (front-ptr queue)) new-front)
           (set-front-ptr! queue new-front)
           queue))))

(define (rear-insert-queue! queue item)
  (let ((new-rear (cons (cons item (rear-ptr queue)) nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-rear)
           (set-rear-ptr! queue new-rear)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-rear)
           (set-rear-ptr! queue new-rear)
           queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         (if (not (null? (front-ptr queue)))
             (set-cdr! (car (front-ptr queue)) nil))
         queue))) 

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-rear-ptr! queue (cdar (rear-ptr queue)))
         (if (null? (rear-ptr queue))
             (set-front-ptr! queue nil)
             (set-cdr! (rear-ptr queue) nil))
         queue))) 

(define (print-queue queue)
  (define (print-end) (display ")") (newline))
  (display "(")
  (define (print-next next)
    (cond ((null? next) (print-end))
          ((null? (cdr next)) (display (caar next))
                              (print-end))
          (else (display (caar next))
                (display " ")
                (print-next (cdr next)))))
  (print-next (front-ptr queue)))
 
(define q (make-queue))
(print-queue (front-insert-queue! q 'a))
(print-queue (front-insert-queue! q 'b))
(print-queue (rear-insert-queue! q 'c))
(print-queue (rear-insert-queue! q 'd))
(print-queue (front-insert-queue! q 'e))
(print-queue (rear-insert-queue! q 'f))
(print-queue (front-delete-queue! q))
(print-queue (front-delete-queue! q))
(print-queue (rear-delete-queue! q))
(print-queue (rear-delete-queue! q))
(print-queue (front-delete-queue! q))
(print-queue (rear-delete-queue! q))
