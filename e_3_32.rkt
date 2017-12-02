#lang planet neil/sicp

; 確認するためdequeに差し替えた。
; https://github.com/n3104/sicp/blob/master/e_3_23.rkt
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (caar (front-ptr queue))))
(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (caar (rear-ptr queue))))
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

; http://sicp.iijlab.net/fulltext/x334.html
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'make-wire-done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'call-each-done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'propagate-done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 ;                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      ; LIFOとなるように同じ時間区分の先頭に新しい手続きを登録する。
      (front-insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        ; LIFOとなるように同じ時間区分の先頭に新しい手続きを登録する。
        (front-insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    ; first-agenda-item に合わせて同じ時間区分の新しい登録された手続きを削除する。
    (front-delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        ; LIFOとなるように同じ時間区分の新しい登録された手続きを返す。
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'half-adder-ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'full-adder-ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'inverter-ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'and-gate-ok)

(define (logical-and s1 s2)
  (cond ((not (or (= s1 0) (= s1 1)))
         (error "Invalid signal" s1))
        ((not (or (= s2 0) (= s2 1)))
         (error "Invalid signal" s2))
        ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

(define (logical-or s1 s2)
  (cond ((not (or (= s1 0) (= s1 1)))
         (error "Invalid signal" s1))
        ((not (or (= s2 0) (= s2 1)))
         (error "Invalid signal" s2))
        ((and (= s1 0) (= s2 0)) 0)
        (else 1)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'or-gate-ok)


; 以下、自作
(define (ripple-carry-adder a-list b-list s-list c-out)
  (define (setup a-list b-list s-list c-in c-out-local)
    (cond ((null? (cdr a-list))
           (full-adder (car a-list) (car b-list) c-in (car s-list) c-out)
           'ok)
          (else
           (full-adder (car a-list) (car b-list) c-in (car s-list) c-out-local)
           (setup (cdr a-list) (cdr b-list) (cdr s-list) c-out-local (make-wire)))))
  ; a-list, b-list, s-list の要素数が一致するかの入力チェックは省いた。
  (setup a-list b-list s-list (make-wire) (make-wire)))


; 以下、動作確認
(define in-a1 (make-wire))
(define in-a2 (make-wire))
(define in-b1 (make-wire))
(define in-b2 (make-wire))
(define out-sum1 (make-wire))
(define out-sum2 (make-wire))
(define out-carry (make-wire))
(ripple-carry-adder (list in-a1 in-a2) (list in-b1 in-b2) (list out-sum1 out-sum2) out-carry)

; 0b11 + 0b11 -> 0b110
(set-signal! in-a2 1)
(set-signal! in-a1 1)
(set-signal! in-b2 1)
(set-signal! in-b1 1)
(propagate)
(get-signal out-carry)
(get-signal out-sum2)
(get-signal out-sum1)

; 0b00 + 0b00 -> 0b000
(set-signal! in-a2 0)
(set-signal! in-a1 0)
(set-signal! in-b2 0)
(set-signal! in-b1 0)
(propagate)
(get-signal out-carry)
(get-signal out-sum2)
(get-signal out-sum1)

; 0b10 + 0b01 -> 0b011
(set-signal! in-a2 1)
(set-signal! in-a1 0)
(set-signal! in-b2 0)
(set-signal! in-b1 1)
(propagate)
(get-signal out-carry)
(get-signal out-sum2)
(get-signal out-sum1)

; 0b01 + 0b10 -> 0b011
(set-signal! in-a2 0)
(set-signal! in-a1 1)
(set-signal! in-b2 1)
(set-signal! in-b1 0)
(propagate)
(get-signal out-carry)
(get-signal out-sum2)
(get-signal out-sum1)
