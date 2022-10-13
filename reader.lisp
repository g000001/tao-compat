(in-package :tao-internal)                   ;cl-user!

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


(defun logic-method-p (mesg)
  (and (symbolp mesg)
       (eql 0 (position #\& (string mesg)))))


(defun infix-to-prefix (obj mesg &rest args)
  (cond ((logic-method-p mesg)
         `(,mesg ,obj ,@args))
        ((null args)
         (if (typep mesg 'fixnum)
             (list 'cl:elt obj mesg)
             (list mesg obj)))
        ((null (cdr args))
         `(,mesg ,obj ,@args))
        ('T `(,mesg ,obj ,(apply #'infix-to-prefix args)))))

(defun read-|[| (stream char)
  (declare (ignore char))
  (let ((*readtable* tao-no-dots-readtable))
    (let ((expr (read-delimited-list #\] stream 'T)))
      (apply #'infix-to-prefix expr))))


(declaim (inline codnums))
(defun codnums ()
  '((  0 . tao:expr)
    (  1 . tao:exprdyn)
    (  2 . tao:macro)
    (  3 . tao:subst)
    (  4 . tao:closure)
    (  5 . tao:array)
    (  6 . tao:&+)
    (  7 . tao:hclauses)
    ( 10 . tao:&+dyn)
    ( 11 . tao:subr)
    ( 12 . tao:expr-simple)
    ( 13 . tao:exprdyn-simple)
    ( 14 . tao:subr-simple)
    ( 15 . tao:unit-clauses)
    ( 40 . :ob)
    ( 41 . :qob)
    ( 42 . :opt)
    ( 43 . :qopt)
    ( 44 . :rest)
    ( 45 . :qrest)
    ( 46 . :aux)
    ( 47 . :logic)
    ( 50 . :closed)
    ( 51 . :optn)
    ( 52 . :qoptn)
    (100 . tao:1b-memblk)
    (101 . tao:2b-memblk)
    (102 . tao:4b-memblk)
    (103 . tao:8b-memblk)
    (104 . tao:16b-memblk)
    (105 . tao:32b-memblk)
    (106 . tao:64b-memblk)
    (107 . tao.sys:prestk-memblk)
    (110 . tao.sys:id-hash-memblk)
    (111 . tao.sys:64bloc-memblk)
    (113 . tao.sys:strhead-memblk)
    (114 . tao.sys:locbit-memblk)
    (115 . tao.sys:cell-memblk)
    (116 . tao.sys:vector-memblk)
    (117 . tao.sys:id-memblk)
    (120 . tao.sys:str-memblk)
    (121 . tao.sys:bad-memblk)
    (122 . tao.sys:free-memblk)))


;;; #!codnum
(let ((nums (codnums)))
  (defun codnum-reader (stream char arg)
    (declare (ignore char arg))
    (let* ((codnum (read stream t nil t))
           (num (car (rassoc codnum nums))))
      (or num
          (error "Unknown codnum: #!~A." codnum)))))


;;; eof
