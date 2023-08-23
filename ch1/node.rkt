#lang racket

(require rackunit "object.rkt")

(define (make-node l r)
  (OBJECT ([field left l]
           [field right r])
          ([method sum () (+ (-> left sum) (-> right sum))])))

(define (make-leaf v)
  (OBJECT ([field value v])
          ([method sum () value])))

(check-eq?
 (let ([tree (make-node
              (make-node (make-leaf 3)
                         (make-node (make-leaf 10)
                                    (make-leaf 4)))
              (make-leaf 1))])
   (-> tree sum))
 18)
