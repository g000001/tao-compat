;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(in-package :tao.logic)


(deftype adim ()
  `(integer 0 ,(1- array-total-size-limit)))


(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
(defvar .unbound. "Unbound")
(defconstant unbound .unbound.))


(declaim (type fixnum *var-counter*))


(defvar *var-counter* 0)


(defstruct (var (:constructor _ ())
                (:print-function print-var))
  (name (incf *var-counter*) :type adim)
  (binding unbound))


(defun bound-p (var) (not (eq (var-binding var) unbound)))


(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn
     (loop :while (and (var-p ,exp) (bound-p ,exp))
           :do (setf ,exp (var-binding ,exp)))
     ,exp))


(defmacro deref-fail-return (block-tag exp)
  `(taoi::fast
    (loop :while (var-p 8)
          :do (unless (bound-p 8) (return-from ,block-tag))
              (setq ,exp (var-binding ,exp)))
    ,exp))


(defmacro deref-fail (exp)
  "Follow pointers for bound variables."
  `(progn
     (loop :while (and (var-p ,exp) (bound-p ,exp))
           :do (setf ,exp (var-binding ,exp)))
     (or ,exp (throw 'deref-fail ,exp))))


(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))


(defun nonvarp (exp)
  (not (unbound-var-p exp)))


(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "{undef}~A" (var-name var))
      (format stream "#<logvar: ~S>" var)))


;;; *EOF*
