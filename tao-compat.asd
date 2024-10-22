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
               (:file "util")
               (:file "var")
               (:file "trail")
               (:file "auxfns")
               (:file "object")
               (:file "tao-q")
               (:file "tao-s")
               (:file "unify")
               (:file "prolog")
               (:file "prologc")
               (:file "prologcp")
               (:file "reader")
               (:file "readtable" :depends-on ("package"))
               (:file "tao-world")
               (:file "tao-z")
               (:file "tao-x")
               (:file "tao-j")
               (:file "tao-k")
               (:file "tao-y")
               ;; q
	       (:file "tao-h")
               (:file "tao-o")
               (:file "tao-v")
               (:file "tao-w")
	       (:file "tao-u")
               (:file "tao-i")
               ;;
               
               (:file "tao-e" :depends-on ("tao-s"))
               (:file "tao-g")
               (:file "tao-t")
               (:file "tao-b")
               (:file "tao-f")
               (:file "tao-n")
               ;; l
               (:file "tao-r")
               (:file "tao-p" :depends-on ("tao-t"))
               (:file "tao-m")
               (:file "tao-a")
               (:file "tao-d")
               (:file "tao-c")
	       (:file "tao-0")
               ;;
               (:file "tao-l")
	       ;; s
               (:file "tao-ext"))
  :depends-on (#:cl-ppcre #:babel #:closer-mop))


;;; *EOF*
