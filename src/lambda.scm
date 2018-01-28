;; Utilities
(define-syntax print-exp
  (syntax-rules ()
    ((_ exp) (print (string-append "input  = "
                                   (format "~s" 'exp)
                                   "\noutput = "
                                   (format "~s" exp))))))

(define-macro (lambda-curry args . body)
  (let ((arg (car args))
        (rest-args (cdr args)))
    (if (null? rest-args)
        `(lambda (,arg) ,@body)
        `(lambda (,arg)
           (lambda-curry ,rest-args ,@body)))))

(define-syntax reduct
  (syntax-rules ()
    ((_ f a1 a2 ...) (reduct (f a1) a2 ...))
    ((_ f a) (f1 a))
    ((_ f) f)))

(define-syntax ->
  (syntax-rules ()
    ((_ arg f1 f2 ...) (-> (f1 arg) f2 ...))
    ((_ arg f) (f arg))
    ((_ f) f)))


;; Church Boolean
(define tru
  (lambda-curry (t f) t))


(define fls
  (lambda-curry (t f)
    f))

(define test
  (lambda-curry (l m n)
    (reduct l m n)))

(define _and
  (lambda-curry (b c)
                (reduct b c fls)))

(define _or
  (lambda-curry (b c)
                (reduct b tru c)))

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
  (lambda-curry (f s b)
                (reduct b f s)))

(define fst
  (lambda (p)
    (p tru)))

(define snd
  (lambda (p)
    (p fls)))

;;; e.g.
(print-exp (-> (reduct pair 'v 'w) fst))
(print-exp (-> (reduct pair 'v 'w) snd))


;; Church Number
(define c0
  (lambda-curry (s z)
                z))

(define c1
  (lambda-curry (s z)
                (-> z s)))

(define c2
  (lambda-curry (s z)
                (-> z s s)))

(define c3
  (lambda-curry (s z)
                (-> z s s s)))

(define c4
  (lambda-curry (s z)
                (-> z s s s s)))

(define scc
  (lambda-curry (n s z)
                (s (reduct n s z))))

(define plus
  (lambda-curry (m n s z)
                (reduct m s (reduct n s z))))

(define times
  (lambda-curry (m n)
                (reduct m (plus n) c0)))

;; TODO
(define times2
  (lambda-curry (m n)
                m))

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
      (-> (reduct m ss zz) fst)))

(define subtract
  (lambda-curry (m n)
                (reduct n prd m)))

(define equal
  (lambda-curry (m n)
                (reduct _and
                        (iszero (reduct subtract m n))
                        (iszero (reduct subtract n m)))))

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


;; List (foldr)
(define nil
  (lambda-curry (c n)
                n))

(define cons
  (lambda-curry (h t c n)
                (reduct c h (reduct t c n))))

(define isnil
  (lambda (l)
    (reduct l (lambda-curry (h t) fls) tru)))

(define head
  (lambda (l)
    (reduct l tru fls)))

(define tail
  (lambda (l)
    (fst (reduct l
                 (lambda-curry (x p)
                               (reduct pair (snd p) (reduct cons x (snd p))))
                 (reduct pair nil nil)))))

(define nil_s
  (reduct pair tru tru))

(define cons_s
  (lambda-curry (h t)
      (reduct pair fls (reduct pair h t))))

(define head_s
  (lambda (z)
    (-> z snd fst)))

(define tail_s
  (lambda (z)
    (-> z snd snd)))

(define isnil_s
  fst)

;;; e.g.
(print-exp (reduct nil 'v 'w))
(print-exp (reduct isnil nil 'v 'w))
(print-exp (reduct isnil (reduct cons 'w nil) 'v 'w))
(print-exp (-> (reduct cons 'v (reduct cons 'w nil)) head))
(print-exp (-> (reduct cons 'v (reduct cons 'w nil)) tail head))
(print-exp (reduct (-> (reduct cons 'v (reduct cons 'w nil)) tail tail) 'v 'w))

(print-exp nil_s)
(print-exp (reduct fst nil_s 'v 'w))
(print-exp (reduct snd nil_s 'v 'w))
(print-exp (reduct isnil_s nil_s))
(print-exp (reduct isnil_s (reduct cons_s 'w nil_s)))
(print-exp (-> (reduct cons_s 'v (reduct cons_s 'w nil_s)) head_s))
(print-exp (-> (reduct cons_s 'v (reduct cons_s 'w nil_s)) tail_s head_s))
(print-exp (-> (reduct cons_s 'v (reduct cons_s 'w nil_s)) tail_s tail_s))

