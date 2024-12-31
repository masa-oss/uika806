(test-begin "6.2 Numbers")

;  (cond-expand
;    [exact-complex (test #f (real? -2.5+0.0i))]
;   [else])
(test #t (real? #e1e10))

(test #t (exact? #e3.0))
(test #t (inexact? 3.))

(test #f (finite? 3.0+inf.0i))

(test #t (infinite? 3.0+inf.0i))

(test #t (nan? +nan.0+5.0i))


;; From R7RS 6.2.6 Numerical operations:
;;
;; These predicates are required to be transitive.
;;
;; _Note:_ The traditional implementations of these predicates in
;; Lisp-like languages, which involve converting all arguments to inexact
;; numbers if any argument is inexact, are not transitive.

;; Example from Alan Bawden
#|
(let ((a (- (expt 2 1000) 1))
      (b (inexact (expt 2 1000))) ; assuming > single-float-epsilon
      (c (+ (expt 2 1000) 1)))
  (test #t (if (and (= a b) (= b c))
               (= a c)
               #t)))
|#
;; From CLtL 12.3. Comparisons on Numbers:
;;
;;  Let _a_ be the result of (/ 10.0 single-float-epsilon), and let
;;  _j_ be the result of (floor a). ..., all of (<= a j), (< j (+ j
;;  1)), and (<= (+ j 1) a) would be true; transitivity would then
;;  imply that (< a a) ought to be true ...

;; Transliteration from Jussi Piitulainen
#|
(define single-float-epsilon
  (do ((eps 1.0 (* eps 2.0)))
      ((= eps (+ eps 1.0)) eps)))

(let* ((a (/ 10.0 single-float-epsilon))
       (j (exact a)))
  (test #t (if (and (<= a j) (< j (+ j 1)))
               (not (<= (+ j 1) a))
               #t)))
|#



(test 2.0 (denominator (inexact (/ 6 4))))
(test 11.0 (numerator 5.5))
(test 2.0 (denominator 5.5))
(test 5.0 (numerator 5.0))
(test 1.0 (denominator 5.0))




(test 1/3 (rationalize (exact .3) 1/10))
(test #i1/3 (rationalize .3 1/10))



(test 12.0 (log 4096 2))


(test 1.5707963267948966  (asin 1))



(test 0.0 (atan 0.0 1.0))
(test -0.0 (atan -0.0 1.0))
(test  0.7853981633974483    (atan 1.0 1.0))
(test 1.5707963267948966   (atan 1.0 0.0))
(test 2.356194490192345    (atan 1.0 -1.0))
(test 3.141592653589793    (atan 0.0 -1.0))
(test -3.141592653589793   (atan -0.0 -1.0)) ;
(test -2.356194490192345   (atan -1.0 -1.0))
(test -1.5707963267948966  (atan -1.0 0.0))
; (test -0.7853981633974483  (atan -1.0 1.0))
(test  -0.7853981633974483  (atan -1.0 1.0))
;; (test undefined (atan 0.0 0.0))



(test '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))

(test '(2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))




;  (test 1.10714871779409 (angle 1+2i))


(test-end)
