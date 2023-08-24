#lang racket

(define empty '())

(define (insert set val)
  (if (not (contains? set val))
      (cons val set)
      set))

(define (contains? set val)
  (if (null? set)
      #f
      (if (eq? (car set) val)
          #t
          (contains? (cdr set) val))))

(define x empty)
(define y (insert x 3))
(define z (insert y 5))
(contains? z 2)
(contains? z 5)
