(in-package :tao)


(tao-internal::defreadtable :tao
  (:merge :standard)
  (:macro-char #\( #'tao-internal::tao-read-list)
  (:macro-char #\^ #'tao-internal::tao-read-toga)
  (:macro-char #\^ #'tao-internal::tao-read-toga)
  ;; (:macro-char #\. #'read-|.| 'T)
  (:macro-char #\' (get-macro-character 
                    #\' (tao-internal::find-readtable :standard)) 'T)
  (:syntax-from :common-lisp #\) #\])
  (:macro-char #\[ #'tao-internal::read-|[|)
  (:dispatch-macro-char #\# #\! #'tao-internal::codnum-reader)
  (:macro-char  #\,
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
  (:macro-char #\`
   (lambda (stream char)
     (declare (cl:ignore char))
     (list (quote tao::quasiquote)
           (cl:read stream t nil t) )))
  (:case :upcase))
