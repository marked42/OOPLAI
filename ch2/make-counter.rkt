#lang racket

(require play)

(provide (all-defined-out))

(defmac (OBJECT ([field fname fval] ...)
                ([method mname mparams mbody ...] ...))
  #:keywords field method
  #:captures self
  (letrec
      ([self
        (let ([fname fval] ...)
          (let ([methods (list (cons 'mname (λ mparams mbody ...)) ...)])
            (λ (msg . args)
              (let ([found (assoc msg methods)])
                (if found
                    (apply (cdr found) args)
                    (error "message not understood:" msg))))))])
    self))

(defmac (→ o m arg ...)
  (o 'm arg ...))

(define (make-counter [init-count 0] [init-step 1])
  (OBJECT
   ([field count init-count]
    [field step  init-step])
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    [method inc-by! (v) (→ self step! v) (→ self inc)])))

(check-eq?
 (let ([c (make-counter)])
   (→ c inc-by! 20))
 20)
