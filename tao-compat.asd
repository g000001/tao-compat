;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)


(asdf:defsystem #:tao-compat
  :name "tao-compat"
  :description "TAO Compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.22.10.15"
  :serial t
  :components ((:file "package")
               (:file "common-lisp-world")
               (:file "decl")
               (:file "tao-q")
               (:file "auxfns")
               (:file "patmatch")
               (:file "unify")
               (:file "prolog")
               (:file "prologc")
               (:file "prologcp")
               (:file "util")
               (:file "reader")
               (:file "readtable" :depends-on ("package"))
               (:file "tao-world")
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
               (:file "tao-w")
	       (:file "tao-x")
	       (:file "tao-z"))
  :depends-on (#:cl-ppcre #:babel #:closer-mop))


;;; *EOF*
