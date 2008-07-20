;;; -*- Mode: Lisp -*-

(defpackage #:elco-asd
  (:use #:cl #:asdf))

(in-package #:elco-asd)

(defsystem elco
  :description "Embedded Lisp COmpiler | Esdens Lisp COmpiler"
  :author "Piotr Esden-Tempski <piotr at esden.net>"
  :version "0.0.0"
  :license "BSD (see COPYING file)"
  :components 
  ((:module "elco"
            :components
            ((:file "package")
             (:file "driver-helpers" :depends-on ("package"))
             (:file "driver" :depends-on ("package" "driver-helpers"))
             (:file "elco" :depends-on ("package" "driver-helpers" "driver"))))))

;;; vim: ft=lisp et
