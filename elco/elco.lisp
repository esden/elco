;;; Copyright (c) 2008, Piotr Esden-Tempski <piotr at esden.net>
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without 
;;; modification, are permitted provided that the following conditions are 
;;; met:
;;; 
;;; * Redistributions of source code must retain the above copyright notice, 
;;;   this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright 
;;;   notice, this list of conditions and the following disclaimer in the 
;;;   documentation and/or other materials provided with the distribution.
;;; * The names of its contributors may not be used to endorse or promote 
;;;   products derived from this software without specific prior written 
;;;   permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Basic Emission stuff
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(in-package :elco)

(defvar *compile-directory* *default-pathname-defaults*)

(defun emit (form &rest params)
  (with-output-to-string (emit-string)
    (apply #'format emit-string form params)
    (terpri emit-string)))

(defmacro emitn (&body body)
  `(concatenate 'string ,@body))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Header and footer emission.
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-function-header (name)
  (emitn
    (emit ".globl ~a" name)
    (emit "~a:" name)))

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

(defconstant +cons-mask+ 7)
(defconstant +cons-tag+ 1)

(defconstant +string-mask+ 7)
(defconstant +string-tag+ 2)

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
(defvar *prim-symbol-list* (make-hash-table :test 'equal))

(defstruct prim 
  (is-prim nil :type boolean)
  (arg-count 0 :type fixnum)
  (emitter nil :type function))

(defmacro defprim ((prim si env &rest args) &body body)
  `(setf (gethash (symbol-name ',prim) *prim-symbol-list*) 
         (make-prim :is-prim t 
                    :arg-count ,(length args) 
                    :emitter (lambda (,si ,env ,@args) (emitn ,@body)))))

(defun next-stack-index (si)
  (- si +wordsize+))

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
  (emit "    cmpl $0, %eax")
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

(defprim (elco:consp si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" +cons-mask+)
  (emit "    cmp $~s, %al" +cons-tag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:car si env arg)
  (emit-expr si env arg)
  (emit "    movl -1(%eax), %eax"))

(defprim (elco:cdr si env arg)
  (emit-expr si env arg)
  (emit "    movl 3(%eax), %eax"))

(defprim (elco:make-string si env arg)
  (emit-expr si env arg)
  (emit "    movl %eax, 0(%ebp)")
  (emit "    movl %eax, %ebx")
  (emit "    movl %ebp, %eax")
  (emit "    orl $~s, %eax" +string-tag+)
  (emit "    addl $11, %ebx")
  (emit "    andl $-8, %ebx")
  (emit "    addl %ebx, %ebp"))

(defprim (elco:stringp si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" +string-mask+)
  (emit "    cmp $~s, %al" +string-tag+)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:string-length si env arg)
  (emit-expr si env arg)
  (emit "    movl -2(%eax), %eax"))

;;; Binary primitives
(defprim (elco:fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    addl ~s(%esp), %eax" si))

(defprim (elco:fx- si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg1)
  (emit "    subl ~s(%esp), %eax" si))

(defprim (elco:fx* si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    imull ~s(%esp), %eax" si))

;;; I do not fully understand this one ...
(defprim (elco:fx/ si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg1)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, %edx")
  (emit "    sarl $31, %edx")
  (emit "    idivl ~s(%esp)" si)
  (emit "    shll $~s, %eax" +fxshift+))

;;; This one is also a bit magical ...
(defprim (elco:fxmod si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg1)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movl %eax, %edx")
  (emit "    sarl $31, %edx")
  (emit "    idivl ~s(%esp)" si)
  (emit "    movl %edx, %eax")
  (emit "    shll $~s, %eax" +fxshift+))

(defprim (elco:fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    andl ~s(%esp), %eax" si))

(defprim (elco:fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    orl ~s(%esp), %eax" si))

(defprim (elco:fx= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx< si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    setg %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx<= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    setge %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx> si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    setl %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:fx>= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    setle %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:char= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" +bool-bit+)
  (emit "    or $~s, %al" +bool-f+))

(defprim (elco:cons si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    movl %eax, ~s(%esp)" si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    movl %eax, 4(%ebp)")
  (emit "    movl ~s(%esp), %eax" si)
  (emit "    movl %eax, 0(%ebp)")
  (emit "    movl %ebp, %eax")
  (emit "    orl $~s, %eax" +cons-tag+)
  (emit "    addl $8, %ebp"))

(defprim (elco:string-ref si env string index)
  (emit-expr si env string)
  (emit "    movl %eax, %ebx")
  (emit-expr si env index)
  (emit "    shrl $~s, %eax" +fxshift+)
  (emit "    movzbl 2(%ebx, %eax), %eax")
  (emit "    shll $~s, %eax" +charshift+)
  (emit "    orl $~s, %eax" +chartag+))

; Trinary functions
(defprim (elco:string-set si env string index val)
  (emit-expr si env string)
  (emit "    movl %eax, %ebx     /* save string pointer */")
  (emit "    movl %ebx, ~s(%esp) /* save string pointer to stack */" si)
  (emit-expr (next-stack-index si) env index)
  (emit "    shrl $~s, %eax      /* convert index from fx */" +fxshift+)
  (emit "    addl %eax, %ebx     /* add index to string pointer */")
  (emit-expr (next-stack-index si) env val)
  (emit "    shrl $~s, %eax      /* convert from char */" +charshift+)
  (emit "    movb %al, 2(%ebx)   /* store char */")
  (emit "    movl ~s(%esp), %eax /* get string pointer */" si))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Primcall emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun primitivep (prim)
  (and (symbolp prim) 
       (gethash (symbol-name prim) *prim-symbol-list*) 
       (prim-is-prim (gethash (symbol-name prim) *prim-symbol-list*))))

(defun primcallp (expr)
  (and (consp expr) (primitivep (car expr))))

(defun primitive-emitter (prim)
  (prim-emitter (gethash (symbol-name prim) *prim-symbol-list*)))

(defun check-primcall-args (prim args)
  (or (= (prim-arg-count (gethash (symbol-name prim) *prim-symbol-list*)) (length args)) 
      (error "Wrong amount of parameters to primcall ~a, we expected ~a" 
             prim (prim-arg-count (gethash (symbol-name prim) *prim-symbol-list*)))))

(defun emit-primcall (si env expr)
  (let ((prim (car expr))
        (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Conditional (if)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(let ((count 0))
  (defun unique-label ()
    (let ((L (format nil "EL_~s" count)))
      (incf count)
      L)))

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
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emitn
      (emit-expr si env (if-test expr))
      (emit "    cmp $~s, %al" +bool-f+)
      (emit "    je ~a" alt-label)
      (emit-expr si env (if-conseq expr))
      (emit "    jmp ~a" end-label)
      (emit "~a:" alt-label)
      (emit-expr si env (if-altern expr))
      (emit "~a:" end-label))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Variables (let, let*)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun varp (env expr)
  (and (symbolp expr) 
       (if (assoc expr env)
           t
           (error "Variable ~s is not bound!" expr))))

(defun let*p (expr)
  (equal (symbol-name (car expr)) "LET*"))

(defun letp (expr)
  (or (equal (symbol-name (car expr)) "LET")
      (let*p expr)))

(defun emit-stack-save (si)
  (emit "    movl %eax, ~s(%esp)" si))

(defun extend-env (var si env)
  (if (and (symbolp var)
           (not (immediatep var)))
      (cons (cons var si) env)
      (error "Variable Candidate ~a is not a symbol or it is an immediate!" var)))

(defun let-bindings (expr)
  (cadr expr))

(defun let-body (expr)
  (cons 'elco:progn (cddr expr)))

(defun emit-let (si env expr)
  (labels ((process-let (bindings si new-env)
             (if bindings
                 (let ((b (car bindings)))
                   (emitn
                     (emit-expr si (if (let*p expr) new-env env) (cadr b))
                     (emit-stack-save si)
                     (process-let 
                      (cdr bindings)
                      (next-stack-index si)
                      (extend-env (car b) si new-env))))
                 (emit-expr si new-env (let-body expr)))))
    (process-let (let-bindings expr) si env)))

(defun emit-var (env expr)
  (emit "    movl ~s(%esp), %eax /* load ~a */" (cdr (assoc expr env)) expr))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Functions (letrec, lambda, app)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun letrecp (expr)
  (and (listp expr) 
       (symbolp (car expr))
       (equal (symbol-name (car expr)) "LETREC")))

(defun letrec-bindings (expr)
  (cadr expr))

(defun letrec-body (expr)
  (cons 'elco:progn (cddr expr)))

(defun unique-labels (lvars)
  (mapcar (lambda (l) (concatenate 'string "EF_" (symbol-name l))) lvars))

(defun make-initial-env (lvars asm-labels &optional env)
  (if (and lvars asm-labels)
      (make-initial-env (cdr lvars) (cdr asm-labels) (extend-env (car lvars) (car asm-labels) env))
      env))

(defun lambda-formals (expr)
  (cadr expr))

(defun lambda-body (expr)
  (cons 'elco:progn (cddr expr)))

(defun emit-lambda (env)
  (lambda (expr label)
    (emitn
      (emit-function-header label)
      (labels ((emit-lambda-closure (fmls si env)
                 (if (not fmls)
                     (emit-tail-expr si env (lambda-body expr))
                     (emit-lambda-closure (cdr fmls)
                                          (next-stack-index si)
                                          (extend-env (car fmls) si env)))))
        (emit-lambda-closure (lambda-formals expr) (- +wordsize+) env))
      (emit-function-footer))))

(defun emit-letrec (expr)
  (let* ((bindings (letrec-bindings expr))
         (lvars (mapcar #'car bindings))
         (lambdas (mapcar #'cadr bindings))
         (asm-labels (unique-labels lvars))
         (env (make-initial-env lvars asm-labels)))
    (emitn
      (apply #'concatenate (cons 'string (mapcar (emit-lambda env) lambdas asm-labels)))
      (emit-elco-entry env (letrec-body expr)))))

(defun call-target (expr)
  (if (equal (symbol-name (car expr)) "APP")
      (cadr expr)
      (car expr)))

(defun call-args (expr)
  (if (equal (symbol-name (car expr)) "APP")
      (cddr expr)
      (cdr expr)))

(defun appp (env expr)
  (and (listp expr)
       (or (equal (symbol-name (car expr)) "APP")
           (stringp (cdr (assoc (car expr) env))))))

(defun emit-adjust-base (adjust)
  (emit "    addl $~s, %esp" adjust))

(defun emit-call (env expr)
  (emit "    call ~a" (cdr (assoc expr env))))

(defun emit-app (si env expr)
  (labels ((emit-arguments (si args)
             (unless (not args)
               (emitn 
                 (emit-expr si env (car args))
                 (emit-stack-save si)
                 (emit-arguments (next-stack-index si) (cdr args))))))
    (emitn
      (emit-arguments (next-stack-index si) (call-args expr))
      (emit-adjust-base (+ si +wordsize+))
      (emit-call env (call-target expr))
      (emit-adjust-base (- (+ si +wordsize+))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Tail calls
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-tail-immediate (expr)
  (emit-immediate expr)
  ;(emit "    ret") ; why? o_O
  )

(defun emit-tail-var (env expr)
  (emit-var env expr)
  ;(emit "    ret") ; why? o_O
  )

(defun emit-tail-primcall (si env expr)
  (emit-primcall si env expr)
  ;(emit "    ret") ; why? o_O
  )

(defun emit-tail-if (si env expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emitn
      (emit-expr si env (if-test expr))
      (emit "    cmp $~s, %al" +bool-f+)
      (emit "    je ~a" alt-label)
      (emit-tail-expr si env (if-conseq expr))
      (emit "    jmp ~a" end-label)
      (emit "~a:" alt-label)
      (emit-tail-expr si env (if-altern expr))
      (emit "~a:" end-label))))

(defun emit-tail-let (si env expr)
  (labels ((process-let (bindings si new-env)
             (if bindings
                 (let ((b (car bindings)))
                   (emitn
                     (emit-expr si (if (let*p expr) new-env env) (cadr b))
                     (emit-stack-save si)
                     (process-let 
                      (cdr bindings)
                      (next-stack-index si)
                      (extend-env (car b) si new-env))))
                 (emit-tail-expr si new-env (let-body expr)))))
    (process-let (let-bindings expr) si env)))

(defun emit-tail-call (env expr)
  (emit "    jmp ~a /* tailcall */" (cdr (assoc expr env))))

(defun emit-tail-app (si env expr)
  (labels ((emit-arguments (si args)
             (unless (not args)
               (emitn 
                 (emit-expr si env (car args))
                 (emit-stack-save si)
                 (emit-arguments (next-stack-index si) (cdr args)))))
           (emit-move-args (pos si count)
             (unless (zerop count)
               (emitn
                 (emit "    movl ~s(%esp), %eax" si)
                 (emit "    movl %eax, ~s(%esp)" pos)
                 (emit-move-args (next-stack-index pos) (next-stack-index si) (1- count))))))
    (emitn
      (emit-arguments si (call-args expr))
      (emit-move-args (- +wordsize+) si (length (call-args expr)))
      (emit-tail-call env (call-target expr)))))

(defun emit-tail-progn (si env expr)
  (emit-progn si env expr))

(defun emit-tail-foreign-call (si env expr)
  (emit-foreign-call si env expr))

(defun emit-tail-expr (si env expr)
  (cond
    ((immediatep expr) (emit-tail-immediate expr))
    ((appp env expr) (emit-tail-app si env expr))
    ((varp env expr) (emit-tail-var env expr))
    ((ifp expr) (emit-tail-if si env expr))
    ((letp expr) (emit-tail-let si env expr))
    ((primcallp expr) (emit-tail-primcall si env expr))
    ((foreign-call-p expr) (emit-tail-foreign-call si env expr))
    ((prognp expr) (emit-tail-progn si env expr))
    (t (error "Suppied tail argument not an elco expression! ~a" expr))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; progn
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun prognp (expr)
  (and (listp expr)
       (equal (symbol-name (car expr)) "PROGN")))

(defun emit-progn (si env expr)
  (if (and (symbolp (car expr)) (equal (symbol-name (car expr)) "PROGN"))
      (emit-progn si env (cdr expr))

      (if (= (length expr) 1)
          (emit-tail-expr si env (car expr))
          (emitn 
            (emit-expr si env (car expr))
            (emit-progn si env (cdr expr))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Foreign calls
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun foreign-call-p (expr)
  (and (listp expr)
       (>= (length expr) 2)
       (equal (symbol-name (car expr)) "FOREIGN-CALL")
       (stringp (cadr expr))))

(defun foreign-call-label (expr)
  (cadr expr))

(defun emit-foreign-call (si env expr)
  (emitn
    (emit "    movl %esp, %edi")
    (emit "    addl $~s, %edi" (- si (* +wordsize+ (1- (length (cddr expr))))))
    (emit "    andl $15, %edi")
    (labels ((emit-params (si expr)
               (if (not expr)
                   si
                   (emitn
                     (emit-expr si env (car expr))
                     (emit "    subl %edi, %esp")
                     (emit-stack-save si)
                     (emit "    addl %edi, %esp")
                     (emit-params (next-stack-index si) (cdr expr))))))
      (let ((end-of-si (+ +wordsize+ (emit-params  si (reverse (cddr expr))))))
        (emitn
          (emit "    subl %edi, %esp")
          (emit-adjust-base end-of-si)
          (emit "    call e_~a" (foreign-call-label expr))
          (emit-adjust-base (- end-of-si))
          (emit "    addl %edi, %esp"))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Highlevel emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun emit-expr (si env expr)
  ;(format t "emitting ~a with env ~a~%" expr env)
  (cond
    ((immediatep expr) (emit-immediate expr))
    ((appp env expr) (emit-app si env expr))
    ((varp env expr) (emit-var env expr))
    ((ifp expr) (emit-if si env expr))
    ((letp expr) (emit-let si env expr))
    ((primcallp expr) (emit-primcall si env expr))
    ((foreign-call-p expr) (emit-foreign-call si env expr))
    ((prognp expr) (emit-progn si env expr))
    (t (error "Supplied argument not an elco expression! ~a" expr))))

(defun emit-elco-entry (env expr)
  (emitn
    (emit-function-header "E_elco_entry")
    (emit-expr (- +wordsize+) env expr)
    (emit-function-footer)))

(defun emit-program (expr)
  (emitn
    (if (letrecp expr)
        (emit-letrec expr)
        (emit-elco-entry nil expr))
    (emit-function-header "elco_entry")
    (emit "    movl 4(%esp), %ecx  /* get context save pointer */")
    (emit "    movl %ebx,  4(%ecx) /* save ebx */")
    (emit "    movl %esi, 16(%ecx) /* save esi */")
    (emit "    movl %edi, 20(%ecx) /* save edi */")
    (emit "    movl %ebp, 24(%ecx) /* save ebp */")
    (emit "    movl %esp, 28(%ecx) /* save esp */")
    (emit "    movl %ecx, %esi")
    (emit "    movl 12(%esp), %ebp /* get heap pointer */")
    (emit "    movl 8(%esp), %esp  /* get stack pointer */")
    (emit "    call E_elco_entry")
    (emit "    movl %esi, %ecx")
    (emit "    movl  4(%ecx), %ebx  /* restore ebx */")
    (emit "    movl 16(%ecx), %esi  /* restore esi */")
    (emit "    movl 20(%ecx), %edi  /* restore edi */")
    (emit "    movl 24(%ecx), %ebp  /* restore ebp */")
    (emit "    movl 28(%ecx), %esp  /* restore esp */")
    (emit-function-footer)))

(defun compile-program (expr)
  (emit-program expr))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Convinience code
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun build (code &key output driver-o)
  (let ((assembler-file-name (make-temp-file-name "s"))
        (binary-file-name (if output output (make-temp-file-name "bin")))
        (driver-object (if driver-o driver-o (create-driver-object))))
    (with-open-file (assembler-file assembler-file-name :direction :output :if-exists :supersede)
      (princ code assembler-file))
    (let ((gcc-output (with-output-to-string (gcc-output-stream)
                        (sb-ext:run-program "/usr/bin/gcc" 
                                            `("-O3"
                                              "-Wall"
                                              ,(namestring driver-object) 
                                              ,(namestring assembler-file-name) 
                                              "-o" ,(namestring binary-file-name)) 
                                            :output gcc-output-stream
                                            :error :output))))
      (unless (string= "" gcc-output) (format t "gcc-error: ~s~%" gcc-output)))
    (sb-ext:run-program "/bin/rm" `("-f" ,(namestring assembler-file-name)))
    (unless driver-o (sb-ext:run-program "/bin/rm" `("-f" ,(namestring driver-object))))
    binary-file-name))

(defun execute (binary)
  (with-output-to-string (elco-output-stream)
    (sb-ext:run-program (namestring binary)
                        nil
                        :output elco-output-stream)))

(defun execute-program (expr &key driver-o)
  (let* ((binary-file (build (compile-program expr) :driver-o driver-o))
         (elco-output (execute binary-file)))
    (sb-ext:run-program "/bin/rm" `("-f" ,(namestring binary-file)))
    elco-output))


