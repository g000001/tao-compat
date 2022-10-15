(tao:common-lisp)

(defmacro tao:tao ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
   (cl:in-package tao-user)
   (setq *print-case* :downcase)
   (setq *readtable* tao:tao-standard-readtable)))
