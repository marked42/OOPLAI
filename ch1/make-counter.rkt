#lang racket

(require rackunit "object.rkt")

(provide (all-defined-out))

(define (make-counter [init-count 0] [init-step 1])
  (OBJECT ([field count init-count]
           [field step init-step])
          ([method inc () (set! count (+ count step)) count]
           [method dec () (set! count (- count step)) count]
           [method reset () (set! count 0)]
           [method step! (v) (set! step v)])))

(check-eq?
 (let ([c1 (make-counter)] [c2 (make-counter 10 5)])
   (+ (-> c1 inc) (-> c2 inc)))
 16
 )
