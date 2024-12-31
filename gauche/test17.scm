(import (uika806 test))
(import (scheme read))


(test-begin "Read syntax")

;; check reading boolean followed by eof
(test #t (read (open-input-string "#t")))
(test #t (read (open-input-string "#true")))
(test #f (read (open-input-string "#f")))
(test #f (read (open-input-string "#false")))
(define (read2 port)
  (let* ((o1 (read port)) (o2 (read port)))
    (cons o1 o2)))
;; check reading boolean followed by delimiter
(test '(#t . (5)) (read2 (open-input-string "#t(5)")))
(test '(#t . 6) (read2 (open-input-string "#true 6 ")))
(test '(#f . 7) (read2 (open-input-string "#f 7")))
;          (test '(#f . "8") (read2 (open-input-string "#false\"8\"")))

(test '() (read (open-input-string "()")))
(test '(1 2) (read (open-input-string "(1 2)")))
(test '(1 . 2) (read (open-input-string "(1 . 2)")))
(test '(1 2) (read (open-input-string "(1 . (2))")))
(test '(1 2 3 4 5) (read (open-input-string "(1 . (2 3 4 . (5)))")))
;     (test '1 (cadr (read (open-input-string "#0=(1 . #0#)"))))


;   (test '(1 2 3) (cadr (read (open-input-string "(#0=(1 2 3) #0#)"))))

(test '(quote (1 2)) (read (open-input-string "'(1 2)")))
;  (test '(quote (1 (unquote 2))) (read (open-input-string "'(1 ,2)")))
;  (test '(quote (1 (unquote-splicing 2))) (read (open-input-string "'(1 ,@2)")))
(test '(quasiquote (1 (unquote 2))) (read (open-input-string "`(1 ,2)")))

(test #() (read (open-input-string "#()")))
(test #(a b) (read (open-input-string "#(a b)")))

(test #u8() (read (open-input-string "#u8()")))
(test #u8(0 1) (read (open-input-string "#u8(0 1)")))

(test 'abc (read (open-input-string "abc")))
(test 'abc (read (open-input-string "abc def")))
(test 'ABC (read (open-input-string "ABC")))
;      (test 'Hello (read (open-input-string "|H\\x65;llo|")))




(test 'abc (read (open-input-string "#!fold-case ABC")))
(test 'ABC (read (open-input-string "#!fold-case #!no-fold-case ABC")))

(test 'def (read (open-input-string "#; abc def")))
(test 'def (read (open-input-string "; abc \ndef")))
(test 'def (read (open-input-string "#| abc |# def")))
(test 'ghi (read (open-input-string "#| abc #| def |# |# ghi")))
(test 'ghi (read (open-input-string "#; ; abc\n def ghi")))
(test '(abs -16) (read (open-input-string "(#;sqrt abs -16)")))
(test '(a d) (read (open-input-string "(a #; #;b c d)")))
(test '(a e) (read (open-input-string "(a #;(b #;c d) e)")))
(test '(a . c) (read (open-input-string "(a . #;b c)")))
;  (test '(a . b) (read (open-input-string "(a . b #;c)")))

(define (test-read-error str)
;  (test-assert str
  (test str
      (guard (exn (else #t))
        (read (open-input-string str))
        #f)))



(test-read-error "(#;a . b)")
(test-read-error "(a . #;b)")
(test-read-error "(a #;. b)")
(test-read-error "(#;x #;y . z)")
(test-read-error "(#; #;x #;y . z)")
(test-read-error "(#; #;x . z)")

(test #\a (read (open-input-string "#\\a")))
(test #\space (read (open-input-string "#\\space")))
(test 0 (char->integer (read (open-input-string "#\\null"))))
(test 7 (char->integer (read (open-input-string "#\\alarm"))))
(test 8 (char->integer (read (open-input-string "#\\backspace"))))
(test 9 (char->integer (read (open-input-string "#\\tab"))))
(test 10 (char->integer (read (open-input-string "#\\newline"))))
(test 13 (char->integer (read (open-input-string "#\\return"))))
(test #x7F (char->integer (read (open-input-string "#\\delete"))))
(test #x1B (char->integer (read (open-input-string "#\\escape"))))
(test #x03BB (char->integer (read (open-input-string "#\\λ"))))
(test #x03BB (char->integer (read (open-input-string "#\\x03BB"))))



(test "abc" (read (open-input-string "\"abc\"")))
(test "abc" (read (open-input-string "\"abc\" \"def\"")))
(test "ABC" (read (open-input-string "\"ABC\"")))
(test "Hello" (read (open-input-string "\"H\\x65;llo\"")))
(test 7 (char->integer (string-ref (read (open-input-string "\"\\a\"")) 0)))
(test 8 (char->integer (string-ref (read (open-input-string "\"\\b\"")) 0)))
(test 9 (char->integer (string-ref (read (open-input-string "\"\\t\"")) 0)))
(test 10 (char->integer (string-ref (read (open-input-string "\"\\n\"")) 0)))
(test 13 (char->integer (string-ref (read (open-input-string "\"\\r\"")) 0)))
(test #x22 (char->integer (string-ref (read (open-input-string "\"\\\"\"")) 0)))
(test #x7C (char->integer (string-ref (read (open-input-string "\"\\|\"")) 0)))
(test "line 1\nline 2\n" (read (open-input-string "\"line 1\nline 2\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\\ncontinued\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\ \ncontinued\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\\n continued\n\"")))

#|
(test "line 1continued\n" (read (open-input-string "\"line 1\\ \t \n \t continued\n\"")))
(test "line 1\n\nline 3\n" (read (open-input-string "\"line 1\\ \t \n \t \n\nline 3\n\"")))
(test #x03BB (char->integer (string-ref (read (open-input-string "\"\\x03BB;\"")) 0)))




(define-syntax test-write-syntax
  (syntax-rules ()
    ((test-write-syntax expect-str obj-expr)
     (let ((out (open-output-string)))
       (write obj-expr out)
       (test expect-str (get-output-string out))))))

(test-write-syntax "|.|" '|.|)
(test-write-syntax "|a b|" '|a b|)
(test-write-syntax "|,a|" '|,a|)
(test-write-syntax "|\"|" '|\"|)
(test-write-syntax "a" '|a|)
;; (test-write-syntax "a.b" '|a.b|)
(test-write-syntax "|2|" '|2|)
(test-write-syntax "|+3|" '|+3|)
(test-write-syntax "|-.4|" '|-.4|)
(test-write-syntax "|+i|" '|+i|)
(test-write-syntax "|-i|" '|-i|)
(test-write-syntax "|+inf.0|" '|+inf.0|)
(test-write-syntax "|-inf.0|" '|-inf.0|)

(test-write-syntax "|+nan.0|" '|+nan.0|)
(test-write-syntax "|+NaN.0|" '|+NaN.0|)
(test-write-syntax "|+NaN.0abc|" '|+NaN.0abc|)

|#

(test-end)



