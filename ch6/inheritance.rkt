#lang racket

(require play)

(struct obj (class values))

(defmac (CLASS extends scls-exp
               ([field fname fval] ...)
               ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method extends
  #:captures self ? !
  (let ([scls scls-exp]
        [methods
         (local ([defmac (? fd) #:captures self
                   (dict-ref (obj-values self) 'fd)]
                 [defmac (! fd v) #:captures self
                   (dict-set! (obj-values self) 'fd v)])
           (list (cons 'mname (lambda (self mparam ...) mbody ...)) ...))])
    (letrec ([class
                 (lambda (msg . args)
                   (match msg
                     ['-create
                      (obj class (make-hash (list (cons 'fname fval) ...)))]
                     ['-lookup
                      (let ([found (assoc (first args) methods)])
                        (if found
                            (cdr found)
                            (scls '-lookup (first args))))]))])
      class)))

(defmac (-> o m arg ...)
  (let ([obj o])
    (((obj-class o) '-lookup 'm) obj arg ...)))

(define Root
  (λ (msg . args)
    (match msg
      ['-lookup (error "message not understood:" (first args))])))

(define A
  (CLASS extends Root ()
         ([method foo () "foo"]
          [method bar () "bar"])))
(define B
  (CLASS extends A ()
         ([method bar () "B bar"])))

(define (new c)
  (c '-create))

(define b (new B))
; "foo"
(-> b foo)

; "B bar"
(-> b bar)
