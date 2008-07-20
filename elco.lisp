(defmacro with-emit-to-file (file &rest body)
  `(with-open-file (asm-file ,file :direction :output :if-exists :supersede)
      ,@body))

(defmacro emit (format &rest params)
  `(progn (format asm-file ,format ,@params)
          (format asm-file "~%")))

(defun compile-program (x)
  (unless (integerp x) (error "Supplied argument is not an integer"))
  (with-emit-to-file "elco.s"
    (emit ".text")
    (emit ".globl _scheme_entry")
    (emit "_scheme_entry:")
    (emit "movl $~a, %eax" x)
    (emit "ret")))
