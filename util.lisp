(in-package #:tao-internal)


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



;;; 
