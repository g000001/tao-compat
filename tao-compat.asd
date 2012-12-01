;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)

(defpackage #:tao-compat-asd
  (:use #:cl #:asdf)
  (:export :*tao-compat-version-string*))

(in-package #:tao-compat-asd)

(defvar *tao-compat-version-string* "0.0.3"
  "Tao-compat's version number as a string.")

(in-package #:cl-user)

(asdf:defsystem #:tao-compat
  :name "tao-compat"
  :description "TAO Compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version tao-compat-asd:*tao-compat-version-string*
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "readtable" :depends-on ("package"))
	       (:file "tao-0")
	       (:file "tao-a")
	       (:file "tao-b")
	       (:file "tao-c")
	       (:file "tao-d")
	       (:file "tao-s")
	       (:file "tao-e" :depends-on ("tao-s"))
	       (:file "tao-f")
	       (:file "tao-g")
	       (:file "tao-h")
	       (:file "tao-i")
	       (:file "tao-l")
	       (:file "tao-m")
	       (:file "tao-n")
	       (:file "tao-t")
	       (:file "tao-p" :depends-on ("tao-t"))
	       (:file "tao-r")
	       (:file "tao-u")
	       (:file "tao-v")
	       (:file "tao-x")
	       (:file "tao-z"))
  :depends-on (#:cl-ppcre #:named-readtables))

