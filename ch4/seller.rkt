#lang racket

(require "../ch2/make-counter.rkt")

(provide (all-defined-out))

(define seller
  (OBJECT ()
          ([method price (prod)
                   (* (case prod
                        ((1) (→ self price1))
                        ((2) (→ self price2)))
                      (→ self unit))]
           [method price1 () 100]
           [method price2 () 200]
           [method unit () 1])))

(define broker
  (OBJECT
   ([field provider seller])
   ([method price (prod) (→ provider price prod)])))

; delegation
(→ broker price 2)
