#lang racket

(require play)

(provide (all-defined-out))

(defmac (OBJECT ([field fname fval] ...)
                ([method mname mparams mbody ...] ...))
  #:keywords field method
  (let ([fname fval] ...)
    (let ([methods (list (cons 'mname (lambda mparams mbody ...)) ...)])
      (lambda (msg . args)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) args)
              (error "message not understood: " msg))))
      )))

(defmac (-> o m args ...)
  (o 'm args ...))

(define counter
  (OBJECT
   ([field count 0]
    [field step 1])
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)])))

(check-eq? (-> counter inc) 1)
(check-eq? (-> counter step! 2) (void))
(check-eq? (-> counter inc) 3)
(check-eq? (-> counter dec) 1)
(check-eq? (-> counter reset) (void))
(check-eq? (-> counter dec) -2)
(check-exn exn:fail? (lambda () (-> counter hello)))
