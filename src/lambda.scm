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


;; Church Number
(define c0
  (lambda (s)
    (lambda (z)
      z)))

(define c1
  (lambda (s)
    (lambda (z)
      (s z))))

(define c2
  (lambda (s)
    (lambda (z)
      (s (s z)))))

(define c3
  (lambda (s)
    (lambda (z)
      (s (s (s z))))))

(define c4
  (lambda (s)
    (lambda (z)
      (s (s (s (s z)))))))

(define scc
  (lambda (n)
    (lambda (s)
      (lambda (z)
        (s (reduct n s z))))))

(define plus
  (lambda (m)
    (lambda (n)
      (lambda (s)
        (lambda (z)
          (reduct m s (reduct n s z)))))))

(define times
  (lambda (m)
    (lambda (n)
      (reduct m (plus n) c0))))

;; TODO
(define times2
  (lambda (m)
    (lambda (n)
      m)))

(define iszero
  (lambda (m)
    (reduct m (lambda (x) fls) tru)))

(define zz
  (reduct pair c0 c0))

(define ss
  (lambda (p)
    (reduct pair (snd p) (reduct plus c1 (snd p)))))

(define prd
  (lambda (m)
      (fst (reduct m ss zz))))

(define subtract
  (lambda (m)
    (lambda (n)
      (reduct n prd m))))

(define equal
  (lambda (m)
    (lambda (n)
      (reduct _and
              (iszero (reduct subtract m n))
              (iszero (reduct subtract n m))))))

;;; e.g.
(print-exp (reduct iszero c1 'v 'w))
(print-exp (reduct iszero (reduct plus c0 c0) 'v 'w))
(print-exp (reduct iszero (reduct times c0 c2) 'v 'w))
(print-exp (reduct iszero (reduct prd c1) 'v 'w))
(print-exp (reduct iszero (reduct prd c2) 'v 'w))
(print-exp (reduct iszero (reduct subtract c1 c1) 'v 'w))
(print-exp (reduct iszero (reduct subtract c2 c1) 'v 'w))
(print-exp (reduct equal c3 c3 'v 'w))
(print-exp (reduct equal c3 c1 'v 'w))

