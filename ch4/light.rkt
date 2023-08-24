#lang racket

(require "delegation.rkt")

(define true
  (OBJECT-DEL root ()
              ([method ifTrueFalse (t f) (t)])))

(define false
  (OBJECT-DEL root ()
              ([method ifTrueFalse (t f) (f)])))

(define light
  (OBJECT-DEL root
              ([field on false])
              ([method turn-on () (set! on true)]
               [method turn-off () (set! on false)]
               [method on? () on])))

; "light is off"
(→ (→ light on?) ifTrueFalse (λ () "light is on")
   (λ () "light is off"))

(→ light turn-on)

; "light is on"
(→ (→ light on?) ifTrueFalse (λ () "light is on")
                               (λ () "light is off"))
