#lang plai

(define-type Set
  [mtSet]
  [aSet (val number?) (next Set?)])

(define empty (mtSet))

(define (insert set val)
  (if (not (contains? set val))
      (aSet val set)
      set))

(define (contains? set val)
  (type-case Set set
    [mtSet () #f]
    [aSet (v next)
          (if (eq? v val)
              #t
              (contains? next val))]))

(define x empty)
(define y (insert x 3))
(define z (insert y 5))
(contains? z 2)
(contains? z 5)
