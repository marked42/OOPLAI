#lang racket

(require play)

(struct obj (class values))

(defmac (CLASS ([field fname fval] ...)
               ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method
  #:captures self
  (let ([methods
         (list (cons 'mname (lambda (self mparam ...) mbody ...)) ...)])
    (letrec
        ([class
             (lambda (msg .args)
               (match msg
                 ['-create (obj class (make-hash (list (cons 'fname fval) ...)))]
                 ['-read (dict-ref (obj-values (first args)) (second args))]
                 ['-write (obj-values (first args)) (second args) (third args)]
                 ['-invoke
                  (let ([found (assoc (second args) methods)])
                    (if found
                        (apply (cdr found) (rest args))
                        (error "message not understood: " (second args))))]))])
      class)))

(defmac (-> o m arg ...)
  (let ([obj o])
    ((obj-class o) '-invoke 'm obj arg ...)))
