;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Basic Emission stuff
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(in-package :elco)

(defvar *compile-directory* *default-pathname-defaults*)

(defvar *compile-port* nil)

(defun open-compile-port (file)
  (setf *compile-port* (open file :direction :output :if-exists :supersede)))

(defun close-compile-port ()
  (close *compile-port*)
  (setf *compile-port* nil))

(defun emit (form &rest params)
  (progn (apply #'format `(,*compile-port* ,form ,@params))
          (format *compile-port* "~%")))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Header and footer emission.
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-function-header (name)
  (emit "    .text")
  (emit ".globl _~a" name)
  (emit "_~a:" name))

(defun emit-function-footer ()
  (emit "    ret"))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Immediate handling and emission.
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defconstant +fxshift+ 2)
(defconstant +fxtag+ #x00)
(defconstant +fxmask+ #x03)
(defconstant +wordsize+ 4)

(defconstant +fixnum-bits+ (- (* +wordsize+ 8) +fxshift+))

(defconstant +fxlower+ (- (expt 2 (1- +fixnum-bits+))))

(defconstant +fxupper+ (1- (expt 2 (1- +fixnum-bits+))))

(defconstant +nil+ #x3F)

(defconstant +charshift+ 8)
(defconstant +charmask+ #xFF)
(defconstant +chartag+ #x0F)

(defconstant +bool-f+ #x2F)
(defconstant +bool-t+ #x6F)
(defconstant +bool-mask+ #x3F)
(defconstant +bool-bit+ 6)

(defun fixnump (x)
  (and (integerp x) (typep x 'fixnum) (<= +fxlower+ x +fxupper+)))

(defun nilp (x)
  (not x))

(defun charp (x)
  (characterp x))

(defun truep (x)
  (and (symbolp x) (equal (symbol-name x) "T")))

(defun falsep (x)
  (and (symbolp x) (equal (symbol-name x) "F")))

(defun immediatep (x)
  (or (fixnump x) (nilp x) (charp x) (truep x) (falsep x)))

(defun immediate-rep (x)
  (cond
    ((fixnump x) (ash x +fxshift+))
    ((nilp x) +nil+)
    ((charp x) (logior (ash (char-code x) +charshift+) +chartag+))
    ((truep x) +bool-t+)
    ((falsep x) +bool-f+)))

(defun emit-immediate (expr)
  (emit "    movl $~s, %eax" (immediate-rep expr)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Primcall definitions
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar *symbol-list* (make-hash-table))

(defmacro defprim ((prim &rest args) &body body)
  `(progn 
     (format t "~s~%" (string ',prim))
     (let ((prim-hash (make-hash-table)))
       (setf (gethash '*is-prim* prim-hash) t)
       (setf (gethash '*arg-count* prim-hash) 
             ,(length args))
       (setf (gethash '*emitter* prim-hash)
             (lambda ,args ,@body))
       ;(maphash #'(lambda (key value) (format t "prim-hash: ~s -> ~s~%" key value)) prim-hash)
       ;(maphash #'(lambda (key value) (format t "symbol-list: ~s -> ~s~%" key value)) *symbol-list*)
       ;(format t "symbol-list hash: ~s~%" (gethash (symbol-name ',prim) *symbol-list*))
       (setf (gethash ',prim *symbol-list*) prim-hash))))

(defprim (elco:fx1+ arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(defprim (elco:fx1- arg)
  (emit-expr arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(defprim (elco:char-fx arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" (- +charshift+ +fxshift+)))

(defprim (elco:fx-char arg)
  (emit-expr arg)
  (emit "    shll $~s, %eax" (- +charshift+ +fxshift+))
  (emit "    orl $~s, %eax" +chartag+))

(defprim (elco:fixnump arg)
  (emit-expr arg)
  (emit "    and $~s, %al" +fxmask+)
  (emit "    cmp $~s, %al" +fxtag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fxzerop arg)
  (emit-expr arg)
  (emit "    cmp $0, %al")
  (emit "    sete %al")  
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:nilp arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" +nil+)
  (emit "    sete %al")  
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:boolp arg)
  (emit-expr arg)
  (emit "    and $~s, %al" +bool-mask+)
  (emit "    cmp $~s, %al" +bool-f+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:charp arg)
  (emit-expr arg)
  (emit "    and $~s, %al" +charmask+)
  (emit "    cmp $~s, %al" +chartag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:not arg)
  (emit-expr arg)
  (emit "    cmp $~s, %al" +bool-f+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Primcall emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun primitivep (prim)
  (and (symbolp prim) 
       (gethash prim *symbol-list*) 
       (gethash '*is-prim* (gethash prim *symbol-list*))))

(defun primcallp (expr)
  (and (consp expr) (primitivep (car expr))))

(defun primitive-emitter (prim)
  (or (gethash '*emitter* (gethash  prim *symbol-list*))
      (error "The prim ~a has no emitter!" prim)))

(defun check-primcall-args (prim args)
  (or (= (gethash '*arg-count* (gethash prim *symbol-list*)) (length args)) 
      (error "Wrong amount of parameters to primcall ~a, we expected ~a" 
             prim (gethash '*arg-count* (gethash prim *symbol-list*)))))

(defun emit-primcall (expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Highlevel emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-expr (expr)
  (cond
    ((immediatep expr) (emit-immediate expr))
    ((primcallp expr) (emit-primcall expr))
    (t (error "Supplied argument not immediate nor primcall! ~a" expr))))

(defun emit-program (expr)
  (emit-function-header "elco_entry")
  (emit-expr expr)
  (emit-function-footer))

(defun compile-program (expr)
  (open-compile-port (merge-pathnames *compile-directory* #P"elco.s"))
  (emit-program expr)
  (close-compile-port))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Convinience code
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun build ()
  (with-output-to-string (gcc-output-stream)
    (sb-ext:run-program "/usr/bin/gcc" 
                 `("-Wall"
                   ,(namestring (merge-pathnames *compile-directory* #P"elco-driver.c")) 
                   ,(namestring (merge-pathnames *compile-directory* #P"elco.s")) 
                   "-o" ,(namestring (merge-pathnames *compile-directory* #P"elco"))) 
                 :output gcc-output-stream
                 :error :output)))

(defun execute ()
  (with-output-to-string (elco-output-stream)
    (sb-ext:run-program (namestring (merge-pathnames *compile-directory* #P"elco"))
                 nil
                 :output elco-output-stream)))

(defun execute-program (expr)
  (compile-program expr)
  (let ((gcc-output-string (build))
        (elco-output-string (execute)))
    (format t "gcc: ~a~%" gcc-output-string)
    (format t "elco: ~a" elco-output-string)))