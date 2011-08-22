(in-package :tao-internal)

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
            `(tao:selfass
              ,@(sb-impl::read-list stream ignore)))
         (otherwise
            `(setf ,@(sb-impl::read-list stream ignore)))))
    (otherwise
       (sb-impl::read-list stream ignore))))

(defreadtable :tao
  (:merge :standard)
  (:macro-char #\( #'tao-internal::TAO-READ-LIST 'T)
  (:case :upcase))