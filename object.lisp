;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(defpackage tao.object 
  (:use c2cl))


(cl:in-package tao.object)


(defclass logical-class (standard-class) ())


(defmethod validate-superclass ((subclass logical-class) (superclass standard-class))
  T)


(defvar *instance-facts* (make-hash-table))


(defmethod make-instance :around ((class logical-class)
                                  &rest initargs
                                  &key (name (gentemp (string (class-name class)))))
  (let ((ins (call-next-method)))
    (setf (gethash ins *instance-facts*)
          name)
    ins))


(defun object-name (obj)
  (gethash obj *instance-facts*))


;;; *EOF*
