;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)


#||
-rw-r--r--  1 mc  staff  104499  8 13 21:28 s.txt
-rw-r--r--  1 mc  staff   66866  8 14 16:54 0.txt
-rw-r--r--  1 mc  staff   62461  8 13 21:28 c.txt
-rw-r--r--  1 mc  staff   62346  8 13 21:28 d.txt
-rw-r--r--  1 mc  staff   48725  8 13 23:28 a.txt
-rw-r--r--  1 mc  staff   48704  8 13 21:28 m.txt
-rw-r--r--  1 mc  staff   46750  8 13 21:28 p.txt
-rw-r--r--  1 mc  staff   43430  8 13 21:28 r.txt
-rw-r--r--  1 mc  staff   37531  8 13 21:28 l.txt
-rw-r--r--  1 mc  staff   36386  8 13 21:28 n.txt
-rw-r--r--  1 mc  staff   29181  8 13 21:28 f.txt
-rw-r--r--  1 mc  staff   27001  8 13 21:28 b.txt
-rw-r--r--  1 mc  staff   22010  8 13 21:31 t.txt
-rw-r--r--  1 mc  staff   20398  8 13 21:28 g.txt
-rw-r--r--  1 mc  staff   19610  8 13 21:28 e.txt
-rw-r--r--  1 mc  staff   17826  8 13 21:28 i.txt
-rw-r--r--  1 mc  staff   17418  8 13 21:28 u.txt
-rw-r--r--  1 mc  staff   12613  8 14 16:55 w.txt
-rw-r--r--  1 mc  staff    9627  8 13 21:28 v.txt
-rw-r--r--  1 mc  staff    9106  8 13 21:28 o.txt
-rw-r--r--  1 mc  staff    3213  8 13 21:28 h.txt
-rw-r--r--  1 mc  staff    2758  8 13 21:30 q.txt


||#

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
               (:file "auxfns")
               (:file "object")
               (:file "tao-q")
               (:file "tao-s")
               (:file "patmatch")
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
               (:file "tao-l")
               (:file "tao-r")
               (:file "tao-p" :depends-on ("tao-t"))
               (:file "tao-m")
               (:file "tao-a")
               (:file "tao-d")
               (:file "tao-c")
	       (:file "tao-0")
	       ;; s
               (:file "tao-ext"))
  :depends-on (#:cl-ppcre #:babel #:closer-mop))


;;; *EOF*
