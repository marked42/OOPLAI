#lang racket

(require "delegation.rkt")

(define (make-point x-init y-init)
  (OBJECT-DEL point
              ([field x x-init]
               [field y y-init])
              ([method x? () x]
               [method y? () y])))

(define point
  (OBJECT-DEL root ()
              ([method above (p2)
                       (if (> (→ p2 y?) (→ self y?))
                           p2
                           self)]
               [method add (p2)
                       (make-point (+ (→ self x?)
                                      (→ p2 x?))
                                   (+ (→ self y?)
                                      (→ p2 y?)))])))

(define random-point
  (OBJECT-DEL point ()
              ([method x? () (* 10 (random))]
               [method y? () (→ self x?)])))

(define 1D-point
  (OBJECT-DEL point
              ([field x 5])
              ([method x? () x]
               [method x! (nx) (set! x nx)])))

(define (make-point-shared y-init)
  (OBJECT-DEL 1D-point
              ([field y y-init])
              ([method y? () y]
               [method y! (ny) (set! y ny)])))

(define p1 (make-point-shared 2))
(define p2 (make-point-shared 4))
; 5
(→ p1 x?)

; 5
(→ p2 x?)

(→ 1D-point x! 10)
; 10
(→ p1 x?)

; 10
(→ p2 x?)
