(in-package :tao)


(tao-internal::defreadtable :tao
  (:merge :standard)
  (:macro-char #\( #'tao-internal::tao-read-list)
  (:macro-char #\^ #'tao-internal::tao-read-toga)
  ;; (:macro-char #\^ #'tao-read-toga)
  ;; (:macro-char #\. #'read-|.| 'T)
  (:macro-char #\' (get-macro-character 
                    #\' (tao-internal::find-readtable :standard)) 'T)
  (:syntax-from :common-lisp #\) #\])
  (:macro-char #\[ #'tao-internal::read-|[|)
  (:dispatch-macro-char #\# #\! #'tao-internal::codnum-reader)
  (:case :upcase))
