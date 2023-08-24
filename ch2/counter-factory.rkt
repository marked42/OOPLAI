#lang racket

(require "make-counter.rkt" rackunit)

(define counter-factory
  (OBJECT
   ([field default-count 0]
    [field default-step 1])
   ([method df-count! (v) (set! default-count v)]
    [method df-step! (v) (set! default-step v)]
    [method make ()
            (OBJECT
             ([field count default-count]
              [field step  default-step])
             ([method inc () (set! count (+ count step)) count]
              [method dec () (set! count (- count step)) count]
              [method reset () (set! count 0)]
              [method step! (v) (set! step v)]
              [method inc-by! (v) (→ self step! v) (→ self inc)]))])))


(define c1 (→ counter-factory make))
(check-eq? (→ c1 inc) 1)

(→ counter-factory df-count! 10)
(→ counter-factory df-step! 5)
(check-eq? (→ c1 inc) 2)

(define c2 (→ counter-factory make))
(check-eq? (→ c2 inc) 15)
(check-eq? (→ c1 inc) 3)
(check-eq? (→ c2 inc) 20)
