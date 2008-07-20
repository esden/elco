;;; -*- Mode: Lisp -*-

(defpackage #:elco-test-asd
  (:use #:cl #:asdf))

(in-package #:elco-test-asd)

(defsystem elco-test
  :description "Embedded Lisp COmpiler Testsystem"
  :author "Piotr Esden-Tempski <piotr at esden.net>"
  :version "0.0.0"
  :license "BSD"
  :depends-on (:elco)
  :components 
  ((:module "test"
            :components
            ((:file "package")
             (:file "tests" :depends-on ("package"))))))

;; vim: ft=lisp et