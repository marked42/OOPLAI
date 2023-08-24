#lang racket

(define make-counter
  (let ([methods
         (list
          (cons 'inc (lambda (self)
                       (self '-write 'count (+ (self '-read 'count) (self '-read 'step)))
                       (self '-read 'count)))
          (cons 'step! (lambda (self v) (self '-write 'step v)))
          (cons 'inc-by! (lambda (self n) (self 'step! n) (self 'inc))))])
    (lambda ([init-count 0] [init-step 1])
      (letrec
          ([self (let ([fields (make-hash (list (cons 'count init-count)
                                                (cons 'step init-step)))])
                   (lambda (msg . args)
                     (match msg
                       ['-read (dict-ref fields (first args))]
                       ['-write (dict-set! fields (first args) (second args))]
                       [_ (let ([found (assoc msg methods)])
                            (if found
                                (apply (cdr found) (cons self args))
                                (error "message not understood: " msg)))])))])
        self))))

(let ([c (make-counter)])
  (c 'inc-by! 10))
