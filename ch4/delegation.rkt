#lang racket

(require play)

(provide (all-defined-out))

(defmac (OBJECT-DEL parent
                    ([field fname fval] ...)
                    ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method
  #:captures self
  (let ([fname fval] ...)
    (let ([methods
           (list (cons 'mname (λ (self mparam ...) mbody ...)) ...)])
      (λ (current)
        (λ (msg . args)
          (let ([found (assoc msg methods)])
            (if found
                (apply (cdr found) (cons current args))
                (apply (parent current) msg args))))))))

(defmac (→ o m arg ...)
  (let ([obj o])
    ((obj obj) 'm arg ...)))

(define root
  (λ (msg . args)
    (error "message not understood" msg)))

(define seller
  (OBJECT-DEL root ()
              ([method price (prod)
                       (* (case prod
                            [(1) (→ self price1)]
                            [(2) (→ self price2)])
                          (→ self unit))]
               [method price1 () 100]
               [method price2 () 200]
               [method unit () 1])))
(define broker
  (OBJECT-DEL seller ()
              ([method unit () 2])))

; 100
(→ seller price 1)

; 200  open recursion works
(→ broker price 1)
