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
   :nilp
   :boolp
   :charp
   :not
   ; conditionals
   :if))
