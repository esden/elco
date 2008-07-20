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
   ; let
   :let
   :let*
   ; functions
   :letrec
   :lambda
   :app))
