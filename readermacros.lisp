;; setf
(set-dispatch-macro-character #\# #\!
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                'setf))

;; selfass
(set-dispatch-macro-character #\# #\!
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                (if (char= #\! (peek-char nil stream))
                                    (progn
                                      (read-char stream nil nil)
                                      'selfass)
                                    'setf)))
#+sbcl
(defun tao-read-list (stream ignore)
  (case (peek-char t stream)
    ((#\!) (read-char stream)
     (case (peek-char nil stream)
       ((#\space #\newline #\return #\tab)
        (read-char stream)
        `(or ,@(sb-impl::read-list stream ignore)))
       ((#\!) 
        (read-char stream)
        `(tao-compat:selfass 
          ,@(sb-impl::read-list stream ignore)))
       (otherwise 
        `(setf ,@(sb-impl::read-list stream ignore)))))
    (otherwise
     (sb-impl::read-list stream ignore))))