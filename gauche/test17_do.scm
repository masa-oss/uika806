
(define   inp  (open-input-string "\"line 1\\ \t \n \t continued\n\""))


(do ((i #\x))
   ((eof-object? i) "end")
  ; (display i)
   (write i)
   (newline)
   (set! i (read-char inp)))


#\x
#\"
#\l
#\i
#\n
#\e
#\space
#\1
#\\
#\space
#\t
#\space
#\newline
#\space
#\t
#\space
#\c
#\o
#\n
#\t
#\i
#\n
#\u
#\e
#\d
#\newline
#\"
"end"


