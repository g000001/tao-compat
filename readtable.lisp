(in-package :tao-internal)


(defmacro toga (obj) obj)

(defmacro tao:selfass (fn &rest args)
  "<説明>
 形式 : (!!func arg1 arg2 ... !argI ... argN)
上式は (setq argI (func arg1 arg2 ... argI ... argN)) と同じ。
自己代入式を作る。関数 func を arg1 ... argN を引数として実行し、
結果を argI に代入する。

<例>
         (!x '(1 2 3))
         (!y '(a b c))
         (!!cons !x y) -> (1 2 3 a b c)
         x -> (1 2 3 a b c)
         y -> (a b c)"
  (let (var)
    (labels ((frob (args)
               (if (null args)
                   ()
                   (let ((car (car args))
                         (cdr (cdr args)))
                     (cond ((symbolp car)
                            (cond ((string= "!" car)
                                   ;; (!!foo !(bar x) 33)
                                   (setq var (cadr args))
                                   (cons (cadr args)
                                         (frob (cddr args))))
                                  ;;
                                  ((string= "!" (subseq (string car) 0 1))
                                   ;; !x
                                   (let ((sym (intern (subseq (string car) 1))))
                                     ;; get var
                                     (setq var sym)
                                     (cons sym (frob cdr))))
                                  ;;
                                  ('T (cons car (frob cdr)))))
                           ;;
                           ((consp car)
                            ;; 入れ子のselfass対応
                            (if (eq 'selfass (car car))
                                (cons car (frob cdr))
                                (cons (frob car)
                                      (frob cdr))))
                           ;;
                           ('T (cons car (frob cdr))))))))
      (let ((expr (frob args)))
        `(setf ,var (,fn ,@expr))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tao-read-toga (stream ignore)
    (declare (ignore ignore))
    (case (peek-char nil stream)
      ((#\( #\' #\")
       (list 'toga (read stream T nil T)) )
      ((#\Space #\Tab #\Newline #\Return)
       (intern "^" *package*) )
      (otherwise
       (list 'toga (read stream T nil T)) ))))

#|(let ((*readtable* (copy-readtable nil)))
  ;(in-package :cl-user)
  (set-macro-character #\^ #'tao-read-toga *readtable*)
  (list (read-from-string "^'(foo bar baz)")
        (read-from-string "^x")
        (read-from-string "^'\"||\"")
        (read-from-string "(^ foo)")))|#

(defun read-list (stream ignore)
  #+sbcl (sb-impl::read-list stream ignore)
  #+lispworks (system::read-list stream ignore)
  #+allegro (excl::read-list stream ignore )
  #+ccl (ccl::read-list stream))

(defun tao-read-list (stream ignore)
  (case (peek-char t stream)
    ((#\!) (read-char stream)
     (case (peek-char nil stream)
       ((#\space #\newline #\return #\tab)
        (read-char stream)
        `(or ,@(read-list stream ignore)) )
       ((#\!)
        (read-char stream)
        `(tao:selfass
          ,@(read-list stream ignore) ))
       (otherwise
        `(setf ,@(read-list stream ignore)) )))
    (otherwise
     (read-list stream ignore) )))

(defun read-|.| (stream char)
  (declare (ignore char))
  (case (peek-char nil stream 'T nil 'T)
    ((#\. )
       (read-char stream) 'T nil 'T
       '|..|)
    ((#\Space #\Tab #\Return #\Newline)
       '|.|)
    (otherwise (values))))

(defun method.chain-to-prefix (symbol &rest args)
  (let* ((form (ppcre:split "\\." (string symbol)))
         (form (mapcar #'intern form)))
    (if (cdr form)
        `(apply (function ,(car (last form)))
                ,(reduce (lambda (ans e)
                           (cons e (list ans)))
                         (butlast form))
                (list ,@args))
        `(,symbol ,@args))))


#|(defun infix-to-prefix (obj mesg &rest args)
  (cond ((null args)
         (method.chain-to-prefix mesg obj))
        ((null (cdr args))
         `(,@(method.chain-to-prefix mesg obj) ,@args))
        ('T `(,@(method.chain-to-prefix mesg obj)
                ,(apply #'infix-to-prefix args)))))|#

(defun infix-to-prefix (obj mesg &rest args)
  (cond ((null args)
         (if (typep mesg 'fixnum)
             (list 'cl:elt obj mesg)
             (list mesg obj)))
        ((null (cdr args))
         `(,mesg ,obj ,@args))
        ('T `(,mesg ,obj ,(apply #'infix-to-prefix args)))))

(defun read-|[| (stream char)
  (declare (ignore char))
  (let ((expr (read-delimited-list #\] stream 'T)))
    (apply #'infix-to-prefix expr)))

(defreadtable :tao
  (:merge :standard)
  (:macro-char #\( #'tao-read-list)
  ;; (:macro-char #\^ #'tao-read-toga)
  ;; (:macro-char #\. #'read-|.| 'T)
  (:syntax-from :common-lisp #\) #\])
  (:macro-char #\[ #'read-|[|)
  (:case :upcase))
