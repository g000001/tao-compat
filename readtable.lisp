(cl:in-package :tao-internal)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar tao:tao-standard-readtable (copy-readtable nil))
(let ((*readtable* tao:tao-standard-readtable))
  (set-macro-character #\( #'tao-internal::tao-read-list)
  (set-macro-character #\^ #'tao-internal::tao-read-toga)
  (set-macro-character #\^ #'tao-internal::tao-read-toga)
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar tao-internal::tao-no-dots-readtable (copy-readtable tao:tao-standard-readtable)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*readtable* tao-internal::tao-no-dots-readtable))
    (set-macro-character #\.
                         (lambda (srm dot)
                           (let ((dots (cons dot (loop :for c := (peek-char nil srm T nil T)
                                                    :while (char= #\. c)
                                                    :do (read-char srm T nil T)
                                                    :collect c))))
                             (case (peek-char nil srm T nil T)
                               ((#\Space #\Newline #\Return #\Tab #\Page)
                                (intern (coerce dots 'string) :tao))
                               (otherwise
                                (let ((*readtable* tao:tao-standard-readtable))
                                  (dolist (d dots)
                                    (unread-char d srm))
                                  (read srm T nil T))))))
                         :non-terminating)))


;;; *EOF*
