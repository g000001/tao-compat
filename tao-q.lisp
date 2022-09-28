;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package tao-internal)


(defmacro tao::query (log &rest args)
  `(funcall ,log ,@args #'tao.logic::logvar-setter))


;;; *EOF*
