;; Utilities
(define-syntax print-exp
  (syntax-rules ()
    ((_ exp) (print (string-append "input  = "
                                   (format "~s" 'exp)
                                   "\noutput = "
                                   (format "~s" exp))))))

(define-syntax reduct
  (syntax-rules ()
    ((_ f1 f2 f3 ...) (reduct (f1 f2) f3 ...))
    ((_ f1 f2) (f1 f2))
    ((_ f) f)))

;; Church Boolean
(define tru
  (lambda (t)
    (lambda (f)
      t)))


(define fls
  (lambda (t)
    (lambda (f)
      f)))

(define test
  (lambda (l)
    (lambda (m)
      (lambda (n)
        (reduct l m n)))))

(define _and
  (lambda (b)
    (lambda (c)
      (reduct b c fls))))

(define _or
  (lambda (b)
    (lambda (c)
      (reduct b tru c))))

(define not
  (lambda (b)
    (reduct b fls tru)))

;;; e.g.

(print-exp (reduct test tru 'v 'w))
(print-exp (reduct test fls 'v 'w))
(print-exp (reduct _and tru tru 'v 'w))
(print-exp (reduct _and tru fls 'v 'w))
(print-exp (reduct _or fls tru 'v 'w))
(print-exp (reduct _or fls fls 'v 'w))
(print-exp (reduct not tru 'v 'w))
(print-exp (reduct not fls 'v 'w))


;; Pair
(define pair
  (lambda (f)
    (lambda (s)
      (lambda (b)
        (reduct b f s)))))

(define fst
  (lambda (p)
    (p tru)))

(define snd
  (lambda (p)
    (p fls)))

;;; e.g.
(print-exp (fst (reduct pair 'v 'w)))
(print-exp (snd (reduct pair 'v 'w)))


