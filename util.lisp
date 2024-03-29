(tao:common-lisp)


(in-package #:tao-internal)


(defmacro tao-internal::defsynonym (new-name old-name &optional docstring)
  "New-name is a subst for old-name.  Uses rest arg so be careful."
  `(progn
     ,(if (and (every #'symbolp (list new-name old-name))
               (macro-function old-name) )

          `(setf (macro-function ',new-name)
                 (macro-function ',old-name) )

          `(setf (fdefinition ',new-name)
                 (fdefinition ',old-name) ) )

     ,(when docstring
        `(setf (documentation ',new-name 'function)
               ,docstring ))
     ',new-name ))


(defmacro tao-internal::defclsynonym (new-name &optional docstring)
  (let ((clsym (intern (string new-name) :cl)))
    `(tao-internal::defsynonym ,new-name ,clsym ,docstring) ))


(defmacro def-q-ql-qu-fun (prefix 
                           args
                           (&rest docs)
                              form
                           &optional 
                             (test-place-folder '%test%))
  `(progn
     ,@(mapcar (lambda (doc eqv suf)
                 (let ((name (intern (concatenate 'string
                                                  (string prefix)
                                                  (string suf))
                                     :tao)))
                   `(progn
                      (declaim (inline ,name))
                      (defun ,name ,(if (string= suf "")
                                        (cons 'pred args)
                                        args)
                        ,doc
                        ,(substitute eqv test-place-folder form)))))
               docs
               '((lambda (x y) 
                   (declare (ignore x))
                   (funcall pred item y))
                 #'cl:eq
                 #'cl:eql
                 #'cl:equal)
               '("" "Q" "QL" "QU"))))


(defmacro with-null-venv (&body body &environment env)
  (declare (ignorable env))
  #+lispworks (compiler::|set COMPILER-ENVIRONMENT-VENV| env '())
  `(progn ,@body))


(defmacro with-&aux (&body body)
  (etypecase body
    (null body)
    ((cons &aux-form *)
     `(tao:let (,@(cdr (car body))) ,@(cdr body)))
    (cons `(progn ,@body))))


(defmacro with-return-from-reval (cont-name (&optional bvl &rest vars) &body body)
  (let ((reval (gensym "Reval")))
    `(block ,reval
       (tao:let (,@(tao.logic::variables-in bvl))
         (flet ((,cont-name ()
                  ,@(mapcar (lambda (v) `(when (and (tao.logic::var-p ,v)
                                                    (tao.logic::bound-p ,v))
                                           (setq ,v (tao.logic::deref-exp ,v))))
                            (tao.logic::variables-in (cons bvl vars)))
                  (return-from ,reval T)))
           ,@body)))))


(defmacro with-return-from-pred (pred-name cont-name (&optional bvl &rest vars) &body body)
  `(block ,pred-name
     (tao:let (,@(tao.logic::variables-in bvl))
       (flet ((,cont-name ()
                ,@(mapcar (lambda (v) `(when (and (tao.logic::var-p ,v)
                                                  (tao.logic::bound-p ,v))
                                         (setq ,v (tao.logic::deref-exp ,v))))
                          (tao.logic::variables-in (cons bvl vars)))
                (return-from ,pred-name T)))
         ,@body))))


;;; *EOF*
