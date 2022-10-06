;;; -*- mode: Lisp; coding: utf-8  -*-


(cl:in-package tao-internal)


(defun logvar-setter-exist-p (env)
  (multiple-value-bind (ftype localp)
                       #+lispworks (hcl:function-information 'tao.logic::logvar-setter env)
    (and ftype localp)))


(defmacro tao::query (log &rest args &environment env)
  `(funcall ,log
            ,@(mapcar #'unquotify args)
            ,(if (logvar-setter-exist-p env)
                 '(function tao.logic::logvar-setter)
                 '(constantly T))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun qq-expand-list (x depth)
    (if (consp x)
        (case (car x)
          ((tao::quasiquote)
           (list (quote list)
                 (list 'cons (list (quote quote) (car x))
                       (qq-expand (cdr x) (+ depth 1)))))
          ((tao::unquote tao::unquote-splicing)
           (cond ((> depth 0)
                  (list (quote list)
                        (list (quote cons)
                              (list (quote quote)
                                    (car x))
                              (qq-expand (cdr x) (- depth 1)))))
                 ((eq (quote tao::unquote) (car x))
                  (list* (quote list)
                         (cdr x)))
                 (:else
                  (list* (quote append)
                         (cdr x)))))
          (otherwise
           (list (quote list)
                 (list (quote append)
                       (qq-expand-list (car x) depth)
                       (qq-expand (cdr x) depth)))))
        (list (quote quote)
              (list x) )))

  (defun qq-expand (x depth)
    (if (consp x)
        (case (car x)
          ((tao::quasiquote)
           (list (quote cons)
                 (list (quote quote)
                       (car x) )
                 (qq-expand (cdr x) (+ depth 1)) ))
          ((tao::unquote tao::unquote-splicing)
           (cond ((> depth 0)
                  (list (quote cons)
                        (list (quote quote)
                              (car x) )
                        (qq-expand (cdr x) (- depth 1)) ))
                 ((and (eq (quote tao::unquote) (car x))
                       (not (null (cdr x)))
                       (null (cddr x)) )
                  (cadr x) )
                 (:else
                  (error "Illegal") )))
          (otherwise
           (list (quote append)
                 (qq-expand-list (car x) depth)
                 (qq-expand (cdr x) depth) )))
        (list (quote quote) x) )))


(defmacro tao::quasiquote (&whole form expr)
  (if (eq (quote tao::quasiquote) (car form))
      (qq-expand expr 0)
      form ))


;;; *EOF*
