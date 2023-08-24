#lang racket

(require play)

(struct obj (class values))

(defmac (CLASS ([field fname fval] ...)
               ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method
  #:captures self
  (let ([methods
         (local [(defmac (? f) #:captures self
                   (dict-ref (obj-values self) 'f))
                 (defmac (! f v) #:captures self
                   (dict-set! (obj-values self) 'f v))]
           (list (cons 'mname (lambda (self mparam ...) mbody ...)) ...))])
    (letrec
        ([class
             (lambda (msg . args)
               (match msg
                 ['-create
                  (obj class (make-hash (list (cons 'fname fval) ...)))]
                 ['-lookup
                  (let ([found (assoc (first args) methods)])
                    (if found
                        (cdr found)
                        (error "message not understood: " (second args))))]))])
      class)))

(defmac (-> o m arg ...)
  (let ([obj o])
    (((obj-class o) '-lookup 'm) obj arg ...)))

(define (new c)
  (c '-create))
