#lang racket

(require rackunit "make-counter.rkt" "object.rkt")

(define (inc-all lst)
  (map (lambda (x) (-> x inc)) lst))

(check-equal?
 (inc-all (list (make-counter)
                (make-counter 10 5)
                (OBJECT () ((method inc () "hola")))))
 '(1 15 "hola")
 )
