#lang racket

(require play "seller.rkt" "../ch2/make-counter.rkt")

(defmac (OBJECT-FWD target
                    ([field fname fval] ...)
                    ([method mname mparams mbody ...] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname fval] ...)
              (let ([methods (list (cons 'mname (λ mparams mbody ...)) ...)])
                (λ (msg . args)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) args)
                        (apply target msg args))))))])
    self))

(define broker
  (OBJECT-FWD seller () ()))

; 200
(→ broker price 2)
; 1
(→ broker unit)
