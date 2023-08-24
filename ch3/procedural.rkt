#lang racket

(define empty (lambda (n) #f))

(define (insert set val)
  (lambda (n)
    (or (eq? n val)
        (contains? set n))))

(define (contains? set val)
  (set val))

(define x empty)
(define y (insert x 3))
(define z (insert y 5))
(contains? z 2)
(contains? z 5)


(define even (lambda (n) (even? n)))
(define random (lambda (n) (> (random) 0.5)))
