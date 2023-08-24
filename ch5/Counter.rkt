#lang racket

(struct obj (class values))

(define Counter
  (let ([methods
         (list
          (cons 'inc (lambda (self)
                       ((obj-class self) '-write self 'count
                                         (+ ((obj-class self) '-read self 'count)
                                            ((obj-class self) '-read self 'step)))
                       ((obj-class self) '-read self 'count)))
          (cons 'step! (lambda (self v) ((obj-class self) '-write self 'step v)))
          (cons 'inc-by! (lambda (self n)
                           ((obj-class self) '-invoke 'step! self n)
                           ((obj-class self) '-invoke 'inc self))))])
    (letrec ([class
                 (lambda (msg . args)
                   (match msg
                     ['-create (let ([values (make-hash '((count . 0) (step . 1)))])
                                 (obj class values))]
                     ; read self key
                     ['-read (dict-ref (obj-values (first args))
                                       (second args))]
                     ; write self key value
                     ['-write (dict-set! (obj-values (first args))
                                         (second args)
                                         (third args))]
                     ; invoke mname self . args
                     ['-invoke (let ([found (assoc (first args) methods)])
                                 (println (rest args))
                                 (if found
                                     (apply (cdr found) (rest args))
                                     (error "message nout understood: " (first args))))]))])
      class)))

(let ([c (Counter '-create)])
  ((obj-class c) '-invoke 'inc c)
  )
