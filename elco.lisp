(defmacro with-emit-to-file (file &rest body)
  `(with-open-file (asm-file ,file :direction :output :if-exists :supersede)
      ,@body))

(defmacro emit (format &rest params)
  `(progn (format asm-file ,format ,@params)
          (format asm-file "~%")))

(defconstant +fxshift+ 2)
(defconstant +fxmask+ #x03)
(defconstant +wordsize+ 4)

(defconstant +fixnum-bits+ (- (* +wordsize+ 8) +fxshift+))

(defconstant +fxlower+ (- (expt 2 (1- +fixnum-bits+))))

(defconstant +fxupper+ (1- (expt 2 (1- +fixnum-bits+))))

(defconstant +nil+ #x3F)

(defconstant +charshift+ 8)
(defconstant +chartag+ #x0F)

(defun fixnump (x)
  (and (integerp x) (typep x 'fixnum) (<= +fxlower+ x +fxupper+)))

(defun nilp (x)
  (not x))

(defun charp (x)
  (characterp x))

(defun immediatep (x)
  (or (fixnump x) (nilp x) (charp x)))

(defun immediate-rep (x)
  (cond
    ((fixnump x) (ash x +fxshift+))
    ((nilp x) +nil+)
    ((charp x) (logior (ash (char-code x) +charshift+) +chartag+))))

(defun compile-program (x)
  (unless (immediatep x) (error "Supplied argument is not an immediate value"))
  (with-emit-to-file "elco.s"
    (emit ".text")
    (emit ".globl _elco_entry")
    (emit "_elco_entry:")
    (emit "movl $~s, %eax" (immediate-rep x))
    (emit "ret")))
