#lang racket

(define Counter
  (CLASS extends Root
         ([field count 0]
          [field step  1])
         ([method inc () (! count (+ (? count) (? step))) (? count)]
          [method dec () (! count (- (? count) (? step))) (? count)]
          [method reset () (! count 0)]
          [method step! (v) (! step v)]
          [method inc-by! (v) (→ self step! v) (→ self inc)])))
(define ReactiveCounter
  (CLASS extends Counter () ()))
