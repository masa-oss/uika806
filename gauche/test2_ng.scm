gauscheでやってみる

(define (means ton) (letrec* ((mean (lambda (f g) (f (/ (sum g ton) n)))) (sum (lambda (g ton) (if (null? ton) (+) (if (number? ton) (g ton) (+ (sum g (car ton)) (sum g (cdr ton))))))) (n (sum (lambda (x) 1) ton))) (values (mean values values) (mean exp log) (mean / /))))

(let*-values (((a b c) (means '(8 5 99 1 22))))
  (test 27 a)

