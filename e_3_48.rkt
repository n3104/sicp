#lang planet neil/sicp

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer number balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'number) number)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ; 本来 if で十分だが、両方の分岐を通ることを確認するため cond + display を利用した。
    (cond ((< (account1 'number) (account2 'number))
           (display 'serializer1->serializer2)
           (newline)
           ((serializer1 (serializer2 exchange))
            account1
            account2))
          (else
           (display 'serializer2->serializer1)
           (newline)
           ((serializer2 (serializer1 exchange))
            account1
            account2)))))


; 以下、動作確認
(define account1 (make-account-and-serializer 1 200))
(define account2 (make-account-and-serializer 2 100))

; 口座蛮行にアクセスできることの確認
(account1 'number)
(account2 'number)

; とりあえず serialized-exchange が実行できることの確認。実際にデッドロックを回避できるかはスレッドの利用が必要になるが、そこまでやるのは面倒なので確認しない。
(serialized-exchange account1 account2)
(serialized-exchange account2 account1)