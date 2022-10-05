(cl:in-package :tao-internal)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar tao:tao-standard-readtable (copy-readtable nil))
(let ((*readtable* tao:tao-standard-readtable))
  (set-macro-character #\( #'tao-internal::tao-read-list)
  (set-macro-character #\^ #'tao-internal::tao-read-toga)
  (set-macro-character #\^ #'tao-internal::tao-read-toga)
  ;; (set-macro-character #\. #'read-|.| 'T)
  (set-macro-character #\'
                       (get-macro-character #\' (copy-readtable nil))
                       T)
  (set-syntax-from-char #\] #\))
  (set-macro-character #\[ #'tao-internal::read-|[|)
  (set-dispatch-macro-character #\# #\! #'tao-internal::codnum-reader)
  (set-macro-character #\,
                       (lambda (stream char)
                         (declare (cl:ignore char))
                         (let ((next (peek-char t stream t nil t)))
                           (if (char= #\@ next)
                               (progn
                                 (read-char stream t nil t)
                                 (list (quote tao::unquote-splicing)
                                       (cl:read stream t nil t) ))
                               (list (quote tao::unquote)
                                     (cl:read stream t nil t) )))))
  (set-macro-character #\`
                       (lambda (stream char)
                         (declare (cl:ignore char))
                         (list (quote tao::quasiquote)
                               (cl:read stream t nil t) )))))


(defmacro tao:tao ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:in-package tao-user)
     (setq *readtable* tao:tao-standard-readtable)))


;;; *EOF*
