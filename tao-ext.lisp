;;; -*- mode: Tao; coding: utf-8  -*-

(tao:tao)
(cl:in-package tao.ext)


(defmacro &arithmetic-function (name pred ari)
  (let ((args (image g (index 0 ari)
                (gensym "_"))))
    `(de ,name (,@(cdr args))
       (& (:aux ,(car args))
          (,pred ,@(cdr args) ,(car args))
          ,(car args)))))

