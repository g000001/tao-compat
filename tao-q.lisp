;;; -*- mode: Lisp; coding: utf-8  -*-


(cl:in-package tao-internal)


(defun var-name-p (expr)
  (and (symbolp expr)
       (eql 0 (position #\_ (string expr)))))


(defun unquotify (expr)
  (typecase expr
    (null '())
    ((and vector (not string))
     (cons 'vector
           (map 'list
                (lambda (elt)
                  (unquotify elt))
                expr)))
    (atom (if (var-name-p expr) ;TODO
              expr
              `',expr))
    ((cons (eql tao:unquote)) (cadr expr))
    (cons (list 'cons
                (unquotify (car expr))
                (unquotify (cdr expr))))))


(defun logvar-setter-exist-p (env)
  (multiple-value-bind (ftype localp)
                       (#+lispworks hcl:function-information
                        #+sbcl sb-cltl2:function-information
                        'tao.logic::logvar-setter env)
    (and ftype localp)))


#+old
(defmacro tao::query (log &rest args &environment env)
  `(funcall ,log
            ,@(mapcar #'unquotify args)
            ,(if (logvar-setter-exist-p env)
                 '(function tao.logic::logvar-setter)
                 '(constantly T))))

(defmacro tao::query (log &rest args)
  (let ((cont (gensym "cont")))
    `(with-return-from-reval ,cont (,log ,args)
       (funcall ,log
                ,@(mapcar #'unquotify args)
                #',cont))))

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
  (typecase x
    (cons (case (car x)
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
                   (qq-expand (cdr x) depth) ))))
    (simple-vector 
     (coerce (eval (qq-expand (coerce x 'list) depth)) ;FIXME
             'simple-vector))
    (T (list (quote quote) x)))))


(defmacro tao::quasiquote (&whole form expr)
  (if (eq (quote tao::quasiquote) (car form))
      (qq-expand expr 0)
      form ))


;;; *EOF*
