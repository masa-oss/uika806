;; このファイルは (scheme base) に読み込まれる事に注意。
;;
;; また、schemeの仕様書のPDFからコードをコピペすると、文字化けするので注意
;; define-values やめ
;; letrec* やめ

(define-syntax begin
      (syntax-rules ()
        ((begin exp ...)
         ((lambda () exp ...)))))


(define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f))))

(define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...))))))


(define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))

(define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))


(define-syntax let
      (syntax-rules ()
  ((let ((name val) ...) body1 body2 ...)
   ((lambda (name ...) body1 body2 ...)
    val ...))
  ((let tag ((name val) ...) body1 body2 ...)
   ((letrec ((tag (lambda (name ...)
                    body1 body2 ...)))
        tag)
      val ...))))


(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
           body1 body2 ...)
         (let ((name1 val1))
           (let* ((name2 val2) ...)
             body1 body2 ...)))))



(define-syntax cond
    (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
            (if temp
                temp
                (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))

(define-syntax case
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) => result))
         (if (memv key '(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

#|
(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
                         (lambda args #f))))
    ((define-values (var) expr)
         (define var expr))
    ((define-values (var0 var1 ... varn) expr)
         (begin
           (define var0
             (call-with-values (lambda () expr) list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v)) ...
           (define varn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values (var0 var1 ... . varn) expr)
         (begin
           (define var0
             (call-with-values (lambda () expr) list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v)) ...
           (define varn
             (let ((v (cdr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values var expr)
         (define var
           (call-with-values (lambda () expr)
                             list)))))
|#

(define map1 (lambda (fn list1)
     (letrec ((map*
              (lambda (proc list)
                (if (pair? list)
                     (cons (proc (car list)) (map* proc (cdr list)))
                     ()
                 )
             )))
       (map* fn list1)
    )))

(define map2 (lambda (proc list1 list2)  (list "map2" list1 list2)))


(define map (lambda (proc . args)
              (if (pair? (cdr args))
                  (map2 proc (car args) (cadr args))
                  (map1 proc (car args)))))

#|
(define-syntax letrec*
   (syntax-rules ()
     ((letrec* ((var1 init1) ...) body1 body2 ...)
      (let ((var1 #f) ...)
         (set! var1 init1)
         ...
         (let () body1 body2 ...)))))
|#

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))

    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
        (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

(define string-map
    (lambda (fn stri)
        (letrec ((map*
                     (lambda (proc str)
                         (if    (zero? (string-length  str ))
                                ""
                                (string-append
                                     (string (proc (string-ref  str  0)))
                                     (map*  proc   (string-copy  str 1)))     ))))
            (map* fn stri)
        )))


(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))


(define (make-parameter init . o)
  (let* ((converter
          (if (pair? o) (car o) (lambda (x) x)))
         (value (converter init)))
    (lambda args
      (cond
       ((null? args)
        value)
       ((equal? (car args) "<param-set!>")
        (set! value (cadr args)))
       ((equal? (car args) "<param-convert>")
        converter)
       (else
        (error "bad parameter syntax"))))))



(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
               (lambda (handler-k)
                 (guard-k
                  (lambda ()
                    (let ((var condition))
                      (guard-aux
                        (handler-k
                          (lambda ()
                            (raise-continuable condition)))
                        clause ...))))))))
          (lambda ()
            (call-with-values
             (lambda () e1 e2 ...)
             (lambda args
               (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))


(define (call-with-port port proc)
  (let ((ret (proc port)))
    (close-port port)
    ret))



(define (vector-map proc . lists)
        (letrec ((vector-map-range
             (lambda (lists start end acc)
                     (if (< start end)
                         (begin (vector-set! acc start (apply proc (map (lambda (list) (vector-ref list start)) lists)))
                                (vector-map-range lists (+ start 1) end acc))
                        acc)))
             (len (apply min (map vector-length lists))))
        (vector-map-range lists 0 len (make-vector len))))



(define (for-each proc . lists)
        (if (memq '() lists)
            #t
            (begin (apply proc (map car lists))
                   (apply for-each proc (map cdr lists)))))

(define (vector-for-each proc . lists)
        (letrec ((vector-for-each-range
                  (lambda (lists start end)
                     (if (< start end)
                        (begin (apply proc (map (lambda (list) (vector-ref list start)) lists))
                                (vector-for-each-range lists (+ start 1) end))
                         #t))))
        (vector-for-each-range lists 0 (apply min (map vector-length lists)))))



(define (string-for-each proc . lists)
        (letrec ((string-for-each-range
                  (lambda (lists start end)
                     (if (< start end)
                        (begin (apply proc (map (lambda (list) (string-ref list start)) lists))
                                (string-for-each-range lists (+ start 1) end))
                         #t))))
        (string-for-each-range lists 0 (apply min (map string-length lists)))))


(define-syntax cond-expand
  ;; Extend this to mention all feature ids and libraries
  (syntax-rules (and or not else library)
    ((cond-expand)
     (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...)
                  more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand ((library (name ...))
                   body ...)
                  more-clauses ...)
       (if (library-exists? '(name ...))
           (begin body ...)
           (cond-expand more-clauses ...)))
    ((cond-expand (feature-id body ...)
                  more-clauses ...)
       (if (memq 'feature-id (features))
           (begin body ...)
           (cond-expand more-clauses ...)))))
           
#|
(define-syntax guard
   (syntax-rules ()
     ((guard (var clause ...) e1)
      (with-exception-handler
          (lambda (var) (case var clause ...))
          e1))
    ))
|#

(define let-syntax let)
(define letrec-syntax letrec)


;; ----------- Composable syntax-rules macros via the CK abstract machine

(define-syntax c-cons
  (syntax-rules (quote)
    ((c-cons s 'h 't) (ck s '(h . t)))))


(define-syntax c-append
  (syntax-rules (quote)
    ((c-append s '() 'l2)       (ck s 'l2))   ; return a value
    ((c-append s '(h . t) 'l2)  (ck s (c-cons 'h (c-append 't 'l2))))
))


(define-syntax ck
  (syntax-rules (quote)
    ((ck () 'v) v)			; yield the value on empty stack

    ((ck (((op ...) ea ...) . s) 'v)	; re-focus on the other argument, ea
      (ck s "arg" (op ... 'v) ea ...))

    ((ck s "arg" (op va ...))		; all arguments are evaluated,
      (op s va ...))			; do the redex

    ((ck s "arg" (op ...) 'v ea1 ...)	; optimization when the first ea
      (ck s "arg" (op ... 'v) ea1 ...)) ; was already a value

    ((ck s "arg" (op ...) ea ea1 ...)	; focus on ea, to evaluate it
      (ck (((op ...) ea1 ...) . s) ea))

    ((ck s (op ea ...))			; Focus: handling an application;
      (ck s "arg" (op) ea ...))		; check if args are values
))


(define-syntax c-quote
  (syntax-rules (quote)
    ((c-quote s 'x) (ck s ''x))))


(define-syntax c-map
  (syntax-rules (quote)
    ((c-map s 'f '())      (ck s '()))
    ((c-map s '(f ...) '(h . t)) 
      (ck s (c-cons (f ... 'h) (c-map '(f ...) 't))))
))


(define-syntax c-concatMap
  (syntax-rules (quote)
    ((c-concatMap s 'f '())      (ck s '()))
    ((c-concatMap s '(f ...) '(h . t)) 
      (ck s (c-append (f ... 'h) (c-concatMap '(f ...) 't))))
))


; We now solve Dan Friedman's problem, by transliterating the Haskell
; code for all permutations.

(define-syntax c-perm
  (syntax-rules (quote)
    ((c-perm s '()) (ck s '(())))
    ((c-perm s '(h . t)) (ck s (c-concatMap '(c-ins 'h) (c-perm 't))))))

(define-syntax c-ins
  (syntax-rules (quote)
    ((c-ins s 'x '()) (ck s '((x))))
    ((c-ins s 'x '(h . t)) 
      (ck s (c-cons '(x h . t) (c-map '(c-cons 'h) (c-ins 'x 't)))))))

; The following macro is a syntactic sugar to invoke c-perm
(define-syntax perm
  (syntax-rules ()
    ((perm . args) (ck () (c-quote (c-perm 'args))))))


