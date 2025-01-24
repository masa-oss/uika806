;; このファイルは (scheme base) に読み込まれる事に注意。
;;

(define version "202501")

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



(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))



(define-syntax --let
  (syntax-rules ()
    ((_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((_ tag ((name val) ...) body1 body2 ...)
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


(define-syntax --letrec*
  (syntax-rules ()
    ((_ ((var1 init1) ...) body1 body2 ...)
     (let ((var1 #<Undef>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))

(define-syntax --letrec
  (syntax-rules ()
    ((_ ((var1 init1) ...) body ...)
     (-letrec "generate_temp_names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((_ "generate_temp_names"
       ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     (let ((var1 #<Undef>) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((_ "generate_temp_names"
       (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (-letrec "generate_temp_names"
       (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))



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




(define (map proc . lists)
        (letrec ((simple-map-acc (lambda (proc lsts start end)
                                     (if (null? lsts)
                                         start
                                         (begin (set-cdr! end (list (proc (car lsts))))
                                                (simple-map-acc proc (cdr lsts) start (cdr end))))))
                 (simple-map (lambda (proc list)
                                     (if (null? list)
                                         '()
                                         (let ((p (cons (proc (car list)) '())))
                                              (simple-map-acc proc (cdr list) p p)))))
                 (bad-map (lambda (lsts start end)
                                  (if (memq '() lsts)
                                      start
                                      (begin (set-cdr! end (list (apply proc (simple-map car lsts))))
                                                 (bad-map (simple-map cdr lsts) start (cdr end)))))))
                (if (memq '() lists)
                    '()
                    (let ((p (cons (apply proc (simple-map car lists)) '())))
                         (bad-map (simple-map cdr lists) p p)))))


(define (vector-map proc . lists)
        (letrec ((vector-map-range
             (lambda (lists start end acc)
                     (if (< start end)
                         (begin (vector-set! acc start (apply proc (map (lambda (list) (vector-ref list start)) lists)))
                                (vector-map-range lists (+ start 1) end acc))
                        acc)))
             (len (apply min (map vector-length lists))))
        (vector-map-range lists 0 len (make-vector len))))


(define (string-map proc . lists)
        (letrec ((string-map-range
             (lambda (lists start end acc)
                     (if (< start end)
                         (begin (string-set! acc start (apply proc (map (lambda (list) (string-ref list start)) lists)))
                                (string-map-range lists (+ start 1) end acc))
                        acc)))
             (len (apply min (map string-length lists))))
        (string-map-range lists 0 len (make-string len))))

; umaku ugokanai
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


;; ----------------------------------------------------
;; Schemeで展開順序の問題を回避して
;; マクロを組み合わせる方法(CKマクロ)
;; https://zenn.dev/niyarin/articles/24b579bcfc6694


(define-syntax reverse-lambda
  (syntax-rules ()
    ((_ (args ...) bodies ...)
     (ck () (macro-cons*-ck 'lambda
                            (macro-reverse-ck '(args ...))
                            (macro-reverse-ck '(bodies ...)))))))

(define-syntax macro-reverse-ck
  (syntax-rules (quote)
    ((_ s '(x ...)) (macro-reverse-ck s "progress" (x ...) ()))
    ((_  s "progress" () (res ...)) (ck s '(res ...)))
    ((_ s "progress" (x rest ...) (res ...))
     (macro-reverse-ck s "progress" (rest ...) (x res ...)))))

(define-syntax macro-cons*-ck
  (syntax-rules (quote)
      ((_ s 'args ...)
       (macro-cons*-ck s "progress" (args ...) ()))
      ((_ s "progress" (arg) (res ...))
       (ck s '(res ... . arg)))
      ((_ s "progress" (arg1 args2 ...) (res ...))
       (macro-cons*-ck s "progress" (args2 ...) (res ... arg1)))))


;; 3.3. 予約語の使用
;; https://www.shido.info/lisp/scheme_syntax.html

(define-syntax my-cond
  (syntax-rules (else)
    ((_ (else e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...))
     (when e1 e2 ...))
    ((_ (e1 e2 ...) c1 ...)
     (if e1 
	 (begin e2 ...)
	 (cond c1 ...)))))


