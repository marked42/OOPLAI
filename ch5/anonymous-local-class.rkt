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
             (lambda (msg . args)
               (match msg
                 ['-create
                  (let ([o (obj class (make-hash (list (cons 'fname fval) ...)))])
                    (when (not (empty? args))
                      (let ([found (assoc 'initialize methods)])
                        (if found
                            (apply (cdr found) (cons o args))
                            (error "initialize not implemented in: " class))
                        )
                      )
                    o)]
                 ['-lookup
                  (let ([found (assoc (first args) methods)])
                    (if found
                        (cdr found)
                        (error "message not understood: " (second args))))]))])
      class)))

(defmac (-> o m arg ...)
  (let ([obj o])
    (((obj-class o) '-lookup 'm) obj arg ...)))

(defmac (? f) #:captures self
  (dict-ref (obj-values self) 'f))

(defmac (! f v) #:captures self
  (dict-set! (obj-values self) 'f v))

(define (new class . init-vals)
  (apply class (cons '-create init-vals))
  )

(define doubleton
  (let ([cls (CLASS ([field x 0])
                    ([method initialize (v) (! x v)]
                     [method x? () (? x)]))])
    (cons (new cls 4) (new cls 8))))

(+ (-> (car doubleton) x?) (-> (cdr doubleton) x?))
