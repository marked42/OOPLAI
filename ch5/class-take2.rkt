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

(defmac (? f) #:captures self
  (dict-ref (obj-values self) 'f))

(defmac (! f v) #:captures self
  (dict-set! (obj-values self) 'f v))

(define (new c)
  (c '-create))

(define Counter
  (CLASS
   ([field count 0]
    [field step 1])
   ([method inc () (! count (+ (? count) (? step))) (? count)]
    [method dec () (! count (- (? count) (? step))) (? count)]
    [method reset () (! count 0)]
    [method step! (v) (! step v)]
    [method inc-by! (v) (-> self step! v) (-> self inc)]))
  )

(define c1 (new Counter))
(define c2 (new Counter))
(-> c1 inc-by! 10)
(-> c2 inc-by! 20)
