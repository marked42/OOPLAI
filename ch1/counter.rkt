#lang racket

(require rackunit)

(define counter
  (let ([count 0] [step 1])
    (let ([methods
           (list
            (cons 'inc (lambda ()
                         (set! count (+ count step))
                         count))
            (cons 'dec (lambda ()
                         (set! count (- count step))
                         count))
            (cons 'reset (lambda () (set! count 0)))
            (cons 'step! (lambda (v) (set! step v))))])
      (lambda (msg . args)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) args)
              (error "message not understood: " msg))))
      )))

(check-eq? (counter 'inc) 1)
(check-eq? (counter 'step! 2) (void))
(check-eq? (counter 'inc) 3)
(check-eq? (counter 'dec) 1)
(check-eq? (counter 'reset) (void))
(check-eq? (counter 'dec) -2)
(check-exn exn:fail? (lambda () (counter 'hello)))
