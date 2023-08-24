#lang racket

(require rackunit)

(define counter
  (letrec ([self
            (let ([count 0]
                  [step 1])
              (let ([methods
                     (list
                      (cons 'inc (lambda () (set! count (+ count step))
                                   count))
                      (cons 'dec (lambda () (set! count (- count step))
                                   count))
                      (cons 'reset (lambda () (set! count 0)))
                      (cons 'step! (lambda (v) (set! step v)))
                      (cons 'inc-by! (lambda (n) (self 'step! n)
                                       (self 'inc))))])
                (lambda (msg . args)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) args)
                        (error "message not understood: " msg))))))])
    self))

(check-eq? (counter 'inc) 1)
(check-eq? (counter 'inc-by! 20) 21)
(check-eq?  (counter 'inc) 41)
