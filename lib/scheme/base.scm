;; Requires base.tt output

(define (liftBinaryTT op)
  (lambda (x)
    (lambda (y)
      (op x y))))

(define fix (lambda (f) (lambda (x) ((f (fix f)) x))))

;;;;; Booleans ;;;;;

(define Sfalse #f)
(define Strue #t)

(define Sif
  (lambda (b)
    (lambda (x)
      (lambda (y)
    (if b
        (x top)
        (y top))))))

;; Can't use liftBinaryTT because these are builtins.
(define Snot not)
(define Sand (lambda (x) (lambda (y) (and x y))))
(define Sor (lambda (x) (lambda (y) (or x y))))


;;;;; Lists ;;;;;

(define Snil '())

(define Scons
  (lambda (x)
    (lambda (xs)
      (cons x xs))))

(define (int_foldr x f xs)
  (if (null? xs)
      x
      ((f (car xs)) (int_foldr x f (cdr xs)))))

(define Sfoldr
  (lambda (x)
    (lambda (f)
      (lambda (xs)
        (int_foldr f x xs)))))


;;;;; Reals ;;;;;

(define Szero 0)
(define Sone 1)
(define Stwo 2)
(define Sthree 3)
(define Sfour 4)
(define Sfive 5)
(define Ssix 6)
(define Sseven 7)
(define Seight 8)
(define Snine 9)
(define Sten 10)

(define Sadd (liftBinaryTT +))
(define Ssub (liftBinaryTT -))
(define Smult (liftBinaryTT *))
(define Sdiv (liftBinaryTT /))


(define Seq (liftBinaryTT =))
(define Slt (liftBinaryTT <))
(define Sgt (liftBinaryTT >))
(define Slte (liftBinaryTT <=))
(define Sgte (liftBinaryTT >=))


;;;;; IO ;;;;;

(define (runIO io)
  (io '()))

(define println
  (lambda (x)
    (lambda (w)
      (begin (display x)
             (display "\n")
             ((pair w) top)))))
