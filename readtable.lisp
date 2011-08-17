(in-package :tao-compat)

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

(defreadtable :tao-compat
  (:merge :standard)
  (:macro-char #\( #'TAO-READ-LIST 'T)
  (:case :upcase))