(cl:in-package cl-user)


(defmacro tao:common-lisp ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:in-package cl-user)
     (setq *readtable* (copy-readtable nil))))
