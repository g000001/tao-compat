;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)

(defpackage #:tao-compat-asd
  (:use #:cl #:asdf))

(in-package #:tao-compat-asd)

(defvar *tao-compat-version-string* "0.0.2"
  "Tao-compat's version number as a string.")
(export '*tao-compat-version-string*)


(in-package #:cl-user)

(asdf:defsystem #:tao-compat
  :name "tao-compat"
  :description "TAO Compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.0.1"
  :components ((:file "package")
	       (:file "tao-0" :depends-on ("package"))
	       (:file "tao-a" :depends-on ("package"))
	       (:file "tao-b" :depends-on ("package"))
	       (:file "tao-c" :depends-on ("package"))
	       (:file "tao-d" :depends-on ("package"))
	       (:file "tao-s" :depends-on ("package"))
	       (:file "tao-e" :depends-on ("package" "tao-s"))
	       (:file "tao-f" :depends-on ("package"))
	       (:file "tao-g" :depends-on ("package"))
	       (:file "tao-h" :depends-on ("package"))
	       (:file "tao-i" :depends-on ("package"))
	       (:file "tao-l" :depends-on ("package"))
	       (:file "tao-m" :depends-on ("package"))
	       (:file "tao-n" :depends-on ("package"))
	       (:file "tao-t" :depends-on ("package"))
	       (:file "tao-p" :depends-on ("package" "tao-t"))
	       (:file "tao-r" :depends-on ("package"))
	       (:file "tao-u" :depends-on ("package"))
	       (:file "tao-v" :depends-on ("package"))
	       (:file "tao-x" :depends-on ("package"))
	       (:file "tao-z" :depends-on ("package")))
  :depends-on (#:cl-ppcre))

