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

;;; utilities
(defvar *symbol-list* (make-hash-table))

(defmacro defprim ((prim si env &rest args) &body body)
  `(progn 
     (format t "~s~%" (string ',prim))
     (let ((prim-hash (make-hash-table)))
       (setf (gethash '*is-prim* prim-hash) t)
       (setf (gethash '*arg-count* prim-hash) 
             ,(length args))
       (setf (gethash '*emitter* prim-hash)
             (lambda (,si ,env ,@args) ,@body))
       ;(maphash #'(lambda (key value) (format t "prim-hash: ~s -> ~s~%" key value)) prim-hash)
       ;(maphash #'(lambda (key value) (format t "symbol-list: ~s -> ~s~%" key value)) *symbol-list*)
       ;(format t "symbol-list hash: ~s~%" (gethash (symbol-name ',prim) *symbol-list*))
       (setf (gethash ',prim *symbol-list*) prim-hash))))

;;; Unary primitives
(defprim (elco:fx1+ si env arg)
  (emit-expr si env arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(defprim (elco:fx1- si env arg)
  (emit-expr si env arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(defprim (elco:char-fx si env arg)
  (emit-expr si env arg)
  (emit "    shrl $~s, %eax" (- +charshift+ +fxshift+)))

(defprim (elco:fx-char si env arg)
  (emit-expr si env arg)
  (emit "    shll $~s, %eax" (- +charshift+ +fxshift+))
  (emit "    orl $~s, %eax" +chartag+))

(defprim (elco:fixnump si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" +fxmask+)
  (emit "    cmp $~s, %al" +fxtag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fxzerop si env arg)
  (emit-expr si env arg)
  (emit "    cmp $0, %al")
  (emit "    sete %al")  
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fxlognot si env arg)
  (emit-expr si env arg)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    notl %eax")
  (emit "    shll $~s, %eax" +fxshift+))

(defprim (elco:nilp si env arg)
  (emit-expr si env arg)
  (emit "    cmp $~s, %al" +nil+)
  (emit "    sete %al")  
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:boolp si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" +bool-mask+)
  (emit "    cmp $~s, %al" +bool-f+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:charp si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" +charmask+)
  (emit "    cmp $~s, %al" +chartag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:not si env arg)
  (emit-expr si env arg)
  (emit "    cmp $~s, %al" +bool-f+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

;;; Binary primitives
(defprim (elco:fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    addl ~s(%esp), %eax" si))

(defprim (elco:fx- si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg1)
  (emit "    subl ~s(%esp), %eax" si))

(defprim (elco:fx* si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    imull ~s(%esp), %eax" si))

(defprim (elco:fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    andl ~s(%esp), %eax" si))

(defprim (elco:fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    orl ~s(%esp), %eax" si))

(defprim (elco:fx= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    cmp ~s(%esp), %eax" si)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx< si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    cmp ~s(%esp), %eax" si)
  (emit "    setg %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx<= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    cmp ~s(%esp), %eax" si)
  (emit "    setge %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx> si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    cmp ~s(%esp), %eax" si)
  (emit "    setl %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx>= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (- si +wordsize+) env arg2)
  (emit "    cmp ~s(%esp), %eax" si)
  (emit "    setle %al")
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

(defun emit-primcall (si env expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Conditional (if)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar *unique-label*
  (let ((count 0))
    (lambda ()
      (let ((L (format nil "L_~s" count)))
        (incf count)
        L))))

(defun ifp (expr)
  (and (equal (symbol-name (car expr)) "IF")
       (> (length expr) 2)
       (< (length expr) 5)))

(defun if-test (expr)
  (cadr expr))

(defun if-conseq (expr)
  (caddr expr))

(defun if-altern (expr)
  (cadddr expr))

(defun emit-if (si env expr)
  (let ((alt-label (funcall *unique-label*))
        (end-label (funcall *unique-label*)))
    (emit-expr si env (if-test expr))
    (emit "    cmp $~s, %al" +bool-f+)
    (emit "    je ~a" alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "    jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si env (if-altern expr))
    (emit "~a:" end-label)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Variables (let)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun varp (env expr)
  (and (symbolp expr) 
       (if (assoc expr env)
           t
           (error "Variable ~s is not bound!" expr))))

(defun letp (expr)
  (equal (symbol-name (car expr)) "LET"))

(defun emit-stack-save (si)
  (emit "    movl %eax, ~s(%esp)" si))

(defun next-stack-index (si)
  (- si +wordsize+))

(defun extend-env (var si env)
  (if (and (symbolp var)
           (not (immediatep var)))
      (cons (cons var si) env)
      (error "Variable Candidate ~a is not a symbol or it is an immediate!" var)))

(defun let-bindings (expr)
  (cadr expr))

(defun let-body (expr)
  (caddr expr))

(defun process-let (bindings si env new-env body)
  (if bindings
      (let ((b (car bindings)))
        (emit-expr si env (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     env
                     (extend-env (car b) si new-env) body))
      (emit-expr si new-env body)))

(defun emit-let (si env expr)
  (process-let (let-bindings expr) si env env (let-body expr)))

(defun emit-var (env expr)
  (emit "    movl ~s(%esp), %eax" (cdr (assoc expr env))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Highlevel emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-expr (si env expr)
  ;(format t "emitting ~a~%" expr)
  (cond
    ((immediatep expr) (emit-immediate expr))
    ((varp env expr) (emit-var env expr))
    ((ifp expr) (emit-if si env expr))
    ((letp expr) (emit-let si env expr))
    ((primcallp expr) (emit-primcall si env expr))
    (t (error "Supplied argument not immediate, var, conditional, let nor primcall! ~a" expr))))

(defun emit-program (expr)
  (emit-function-header "L_elco_entry")
  (emit-expr (- +wordsize+) nil expr)
  (emit-function-footer)
  (emit-function-header "elco_entry")
  (emit "    movl %esp, %ecx")
  (emit "    movl 4(%esp), %esp")
  (emit "    call _L_elco_entry")
  (emit "    movl %ecx, %esp")
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