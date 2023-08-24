#lang racket

(require "make-counter.rkt")

(define odd-even
  (OBJECT ()
          ([method even (n) (match n
                              [0 #t]
                              [1 #f]
                              [else (→ self odd (- n 1))])]
           [method odd  (n) (match n
                              [0 #f]
                              [1 #t]
                              [else (→ self even (- n 1))])])))


(→ odd-even odd 15)
(→ odd-even odd 14)
(→ odd-even even 1423842)
