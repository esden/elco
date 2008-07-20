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
; Basic emission stuff
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(mac emitn body
  `(string
     ,@body))

(mac emit args
  `(tostring
     (prf ,@args)
     (prn)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Header and footer emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(def emit-fn-head (name)
  (emitn
    (emit ".globl ~a" name)
    (emit "~a:" name)))

(def emit-fn-foot ()
  (emit "    ret"))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Immediate handling and emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(set +fxshift+ 2)
(set +fxtag+ #x00)
(set +fxmask+ #x03)
(set +wordsize+ 4)

(set +fixnum-bits+ (- (* +wordsize+ 8) +fxshift+))

(set +fxlower+ (- (expt 2 (- +fixnum-bits+ 1))))

(set +fxupper+ (- (expt 2 (- +fixnum-bits+ 1)) 1))

(set +nil+ #x2F)

(set +charshift+ 8)
(set +charmask+ #xFF)
(set +chartag+ #x0F)

(set +bool-t+ #x6F)
(set +bool-mask+ #x3F)
(set +bool-bit+ 6)

(set +cons-mask+ 7)
(set +cons-tag+ 1)

(set +string-mask+ 7)
(set +string-tag+ 2)

(def fixnum? (x)
  (and (isa x 'int) (<= +fxlower+ x +fxupper+)))

(def nil? (x)
  (is x nil))

(def char? (x)
  (isa x 'char))

(def true? (x)
  (and (isa x 'sym) (is (coerce x 'string) "t")))


(def immediate? (x)
  (or (fixnum? x) 
      (nil? x) 
      (char? x) 
      (true? x)))

(def immediate-rep (x)
  (if (fixnum? x) (<< x +fxshift+)
      (nil? x) +nil+
      (char? x) (\| (<< (coerce x 'int) +charshift+) +chartag+)
      (true? x) +bool-t+))

(def emit-immediate (expr)
  (emit "    movl $~s, %eax" (immediate-rep expr)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Highlevel emission
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(def emit-expr (expr)
  (if 
    (immediate? expr) (emit-immediate expr)
    (err "Supplied argument not an elco expression ~a" expr)))

(def emit-elco-entry (expr)
  (emitn
    (emit-fn-head "E_elco_entry")
    (emit-expr expr)
    (emit-fn-foot)))

(def emit-program (expr)
  (emitn
    (emit-elco-entry expr)
    (emit-fn-head "elco_entry")
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
    (emit-fn-foot)))

(def compile-program (expr)
  (emit-program expr))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Convinience code
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(def mktmp-file-name (suffix)
  (tostring
    (pr "/tmp/")
    (if suffix
        (prf "elco-~s~s~s~s~s.~a"
             (rand 10)
             (rand 10)
             (rand 10)
             (rand 10)
             (rand 10)
             suffix)
        (prf "elco-~s~s~s~s~s"
             (rand 10)
             (rand 10)
             (rand 10)
             (rand 10)
             (rand 10)))))

(def writefileraw (val name) 
  " Write a list of bytes in val to a file.
    See also [[writefile1]] "
  (w/outfile s name (map [writeb _ s] val)))

(def build (code output driver-o)
  (with (assembler-file-name (mktmp-file-name "s")
         binary-file-name (if output output (mktmp-file-name "bin"))
         driver-object (if driver-o driver-o (create-driver-object)))
    (writefileraw (map [coerce _ 'int] (coerce code 'cons)) assembler-file-name)
    (let gcc-output (tostring:system:string 
                      "/usr/bin/gcc -O3 -Wall " 
                      driver-object 
                      " " 
                      assembler-file-name 
                      " -o "
                      binary-file-name)
      (unless (is "" gcc-output) 
        (prf "gcc-error: ~s\n" gcc-output)))
    (system:string "/bin/rm -f " assembler-file-name)
    (unless driver-o (system:string "/bin/rm -f " driver-object))
    binary-file-name))

(def execute (binary)
  (tostring:system binary))

(def execute-program (expr)
  (let binary-file (build (compile-program expr) nil nil)
    (let elco-output (execute binary-file)
        (system:string "/bin/rm -f " binary-file)
        elco-output)))
