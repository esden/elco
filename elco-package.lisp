(defpackage :elco
  (:use :common-lisp)
  (:export
   :execute-program
   :compile-program
   :build
   :execute
   ; immediates
   :T
   :F
   :nil
   ; unary primitives
   :fx1+
   :fx1-
   :char-fx
   :fx-char
   :fixnump
   :fxzerop
   :fxlognot
   :nilp
   :boolp
   :charp
   :not
   :consp
   :car
   :cdr
   :make-string
   :stringp
   :string-length
   :string-ref
   :string-set
   ; conditionals
   :if
   ; binary primitives
   :fx+
   :fx-
   :fx*
   :fxlogand
   :fxlogor
   :fx=
   :fx<
   :fx<=
   :fx>
   :fx>=
   :char=
   :cons
   ; let
   :let
   :let*
   ; functions
   :letrec
   :lambda
   :app
   ; progn
   :progn))
