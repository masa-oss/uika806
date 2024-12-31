
(test-begin "Numeric syntax")

;; Numeric syntax adapted from Peter Bex's tests.
;;
;; These are updated to R7RS, using string ports instead of
;; string->number, and "error" tests removed because implementations
;; are free to provide their own numeric extensions.  Currently all
;; tests are run by default - need to cond-expand and test for
;; infinities and -0.0.

(define-syntax test-numeric-syntax
  (syntax-rules ()
    ((test-numeric-syntax str expect strs ...)
     (let* ((z (read (open-input-string str)))
            (out (open-output-string))
            (z-str (begin (write z out) (get-output-string out))))
       (test expect (values z))
       (test #t (and (member z-str '(str strs ...)) #t))))))

;; Each test is of the form:
;;
;;   (test-numeric-syntax input-str expected-value expected-write-values ...)
;;
;; where the input should be eqv? to the expected-value, and the
;; written output the same as any of the expected-write-values.  The
;; form
;;
;;   (test-numeric-syntax input-str expected-value)
;;
;; is a shorthand for
;;
;;   (test-numeric-syntax input-str expected-value (input-str))


;; Simple
(test-numeric-syntax "1" 1)
(test-numeric-syntax "+1" 1 "1")
(test-numeric-syntax "-1" -1)
(test-numeric-syntax "#i1" 1.0 "1.0" "1.")
(test-numeric-syntax "#I1" 1.0 "1.0" "1.")
(test-numeric-syntax "#i-1" -1.0 "-1.0" "-1.")
;; Decimal
(test-numeric-syntax "1.0" 1.0 "1.0" "1.")
(test-numeric-syntax "1." 1.0 "1.0" "1.")
(test-numeric-syntax ".1" 0.1 "0.1" "100.0e-3")
(test-numeric-syntax "-.1" -0.1 "-0.1" "-100.0e-3")
;; Some Schemes don't allow negative zero. This is okay with the standard
(test-numeric-syntax "-.0" -0.0 "-0." "-0.0" "0.0" "0." ".0")
(test-numeric-syntax "-0." -0.0 "-.0" "-0.0" "0.0" "0." ".0")
(test-numeric-syntax "#i1.0" 1.0 "1.0" "1.")
(test-numeric-syntax "#e1.0" 1 "1")
(test-numeric-syntax "#e-.0" 0 "0")
(test-numeric-syntax "#e-0." 0 "0")


;; Decimal notation with suffix
(test-numeric-syntax "1e2" 100.0 "100.0" "100.")
(test-numeric-syntax "1E2" 100.0 "100.0" "100.")
(test-numeric-syntax "1s2" 100.0 "100.0" "100.")
(test-numeric-syntax "1S2" 100.0 "100.0" "100.")
(test-numeric-syntax "1f2" 100.0 "100.0" "100.")
(test-numeric-syntax "1F2" 100.0 "100.0" "100.")
(test-numeric-syntax "1d2" 100.0 "100.0" "100.")
(test-numeric-syntax "1D2" 100.0 "100.0" "100.")
(test-numeric-syntax "1l2" 100.0 "100.0" "100.")
(test-numeric-syntax "1L2" 100.0 "100.0" "100.")
;; NaN, Inf
(test-numeric-syntax "+nan.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "+NAN.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "+inf.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "+InF.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "-inf.0" -inf.0 "-inf.0" "-Inf.0")
(test-numeric-syntax "-iNF.0" -inf.0 "-inf.0" "-Inf.0")
(test-numeric-syntax "#i+nan.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "#i+inf.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "#i-inf.0" -inf.0 "-inf.0" "-Inf.0")


;; Exact ratios
(test-numeric-syntax "1/2" (/ 1 2))
(test-numeric-syntax "#e1/2" (/ 1 2) "1/2")
(test-numeric-syntax "10/2" 5 "5")
(test-numeric-syntax "-1/2" (- (/ 1 2)))
(test-numeric-syntax "0/10" 0 "0")
(test-numeric-syntax "#e0/10" 0 "0")
(test-numeric-syntax "#i3/2" (/ 3.0 2.0) "1.5")

;; Exact complex
(cond-expand
 [exact-complex
  (test-numeric-syntax "1+2i" (make-rectangular 1 2))
  (test-numeric-syntax "1+2I" (make-rectangular 1 2) "1+2i")
  (test-numeric-syntax "1-2i" (make-rectangular 1 -2))
  (test-numeric-syntax "-1+2i" (make-rectangular -1 2))
  (test-numeric-syntax "-1-2i" (make-rectangular -1 -2))
  (test-numeric-syntax "+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
  (test-numeric-syntax "0+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
  (test-numeric-syntax "0+1i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
  (test-numeric-syntax "-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
  (test-numeric-syntax "0-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
  (test-numeric-syntax "0-1i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
  (test-numeric-syntax "+2i" (make-rectangular 0 2) "2i" "+2i" "0+2i")
  (test-numeric-syntax "-2i" (make-rectangular 0 -2) "-2i" "0-2i")]
 [else])


;; Decimal-notation complex numbers (rectangular notation)
(test-numeric-syntax "1.0+2i" (make-rectangular 1.0 2) "1.0+2.0i" "1.0+2i" "1.+2i" "1.+2.i")
(test-numeric-syntax "1+2.0i" (make-rectangular 1 2.0) "1.0+2.0i" "1+2.0i" "1.+2.i" "1+2.i")
(test-numeric-syntax "1e2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
(test-numeric-syntax "1s2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
(test-numeric-syntax "1.0+1e2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
(test-numeric-syntax "1.0+1s2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
;; Fractional complex numbers (rectangular notation)
(cond-expand
 [exact-complex
  (test-numeric-syntax "1/2+3/4i" (make-rectangular (/ 1 2) (/ 3 4)))]
 [else])
;; Mixed fractional/decimal notation complex numbers (rectangular notation)
(test-numeric-syntax "0.5+3/4i" (make-rectangular 0.5 (/ 3 4))
  "0.5+0.75i" ".5+.75i" "0.5+3/4i" ".5+3/4i" "500.0e-3+750.0e-3i")


;; Complex NaN, Inf (rectangular notation)
;;(test-numeric-syntax "+nan.0+nan.0i" (make-rectangular the-nan the-nan) "+NaN.0+NaN.0i")
(test-numeric-syntax "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0) "+Inf.0+Inf.0i")
(test-numeric-syntax "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0) "-Inf.0+Inf.0i")
(test-numeric-syntax "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0) "-Inf.0-Inf.0i")
(test-numeric-syntax "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0) "+Inf.0-Inf.0i")
;; Complex numbers (polar notation)
;; Need to account for imprecision in write output.
;;(test-numeric-syntax "1@2" -0.416146836547142+0.909297426825682i "-0.416146836547142+0.909297426825682i")
;; Base prefixes
(test-numeric-syntax "#x11" 17 "17")
(test-numeric-syntax "#X11" 17 "17")
(test-numeric-syntax "#d11" 11 "11")
(test-numeric-syntax "#D11" 11 "11")
(test-numeric-syntax "#o11" 9 "9")
(test-numeric-syntax "#O11" 9 "9")
(test-numeric-syntax "#b11" 3 "3")
(test-numeric-syntax "#B11" 3 "3")
(test-numeric-syntax "#o7" 7 "7")
(test-numeric-syntax "#xa" 10 "10")
(test-numeric-syntax "#xA" 10 "10")
(test-numeric-syntax "#xf" 15 "15")


(test-numeric-syntax "#x-10" -16 "-16")
(test-numeric-syntax "#d-10" -10 "-10")
(test-numeric-syntax "#o-10" -8 "-8")
(test-numeric-syntax "#b-10" -2 "-2")
;; Combination of prefixes
(test-numeric-syntax "#e#x10" 16 "16")
(test-numeric-syntax "#i#x10" 16.0 "16.0" "16.")
;; (Attempted) decimal notation with base prefixes
(test-numeric-syntax "#d1." 1.0 "1.0" "1.")
(test-numeric-syntax "#d.1" 0.1 "0.1" ".1" "100.0e-3")
(test-numeric-syntax "#x1e2" 482 "482")
(test-numeric-syntax "#d1e2" 100.0 "100.0" "100.")
;; Fractions with prefixes
(test-numeric-syntax "#x10/2" 8 "8")
(test-numeric-syntax "#x11/2" (/ 17 2) "17/2")
(test-numeric-syntax "#d11/2" (/ 11 2) "11/2")
(test-numeric-syntax "#o11/2" (/ 9 2) "9/2")
(test-numeric-syntax "#b11/10" (/ 3 2) "3/2")
;; Complex numbers with prefixes
;;(test-numeric-syntax "#x10+11i" (make-rectangular 16 17) "16+17i")

(test-numeric-syntax "#d1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")
(cond-expand
 [exact-complex
  (test-numeric-syntax "#d10+11i" (make-rectangular 10 11) "10+11i")]
 [else])
;;(test-numeric-syntax "#o10+11i" (make-rectangular 8 9) "8+9i")
;;(test-numeric-syntax "#b10+11i" (make-rectangular 2 3) "2+3i")
;;(test-numeric-syntax "#e1.0+1.0i" (make-rectangular 1 1) "1+1i" "1+i")
;;(test-numeric-syntax "#i1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")

(test-end)

