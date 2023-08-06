(tao:common-lisp)


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


(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar tao-internal::*invisible-funcall* '#:funcall)
(defvar tao-internal::*invisible-query* '#:query)
(macrolet ((define-invisible ()
             `(progn
                (defmacro ,tao-internal::*invisible-funcall* (&rest args)
                  `(cl:funcall ,@args))
                (defmacro ,tao-internal::*invisible-query* (&rest args)
                  `(tao:query ,@args)))))
  (define-invisible)))


(defmethod print-object ((obj (eql tao-internal::*invisible-funcall*)) stream)
  (values))


(defmethod print-object ((obj (eql tao-internal::*invisible-query*)) stream)
  (values))


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
    ((#\()
     (let ((xpr (read-list stream ignore)))
       (typecase (car xpr)
         (cons
          (case (caar xpr)
            ((tao:expr)
             (destructuring-bind ((lam (&rest bvl) &body body) &rest args)
                                 xpr
               `(,tao-internal::*invisible-funcall* (,lam (,@bvl) ,@body) ,@args)))
            ((tao:&+ tao:&+dyn)
             (destructuring-bind ((lam bvl &body body) &rest args)
                                 xpr
               `(,tao-internal::*invisible-query* (,lam ,bvl ,@body) ,@args)))
            (otherwise xpr)))
         (atom xpr))))
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


(defun &-p (mesg)
  (eq mesg 'tao:&))


(defun logic-method-p (mesg)
  (and (symbolp mesg)
       (eql 0 (position #\& (string mesg)))))

(defvar *left-associative-operators*
  '(tao:+ tao:- tao:* tao:/ tao:= tao:< tao:> tao:<= tao:>= tao:/=))

(defun infix-to-prefix (obj mesg &rest args)
  (cond ((member mesg *left-associative-operators*)
         (typecase (length args)
           ((integer 1 1)
            `(,mesg ,obj ,@args))
           ((integer 3 *)
            (apply #'infix-to-prefix
                   `(,mesg ,obj ,(car args))
                   (cadr args)
                   (cddr args)))
           (integer
            (warn "Invalid arguments ~S for left-associative-operator ~S."
                  args
                  mesg))))
        ((eq 'tao:++ obj)
         `(tao-internal::++n ,mesg))
        ((eq 'tao:+ mesg)
         (if (null (cdr args))
             `(locative+ ,obj ,@args)
             `(locative+ ,obj ,(apply #'infix-to-prefix args))))
        ((consp mesg)
         `(tao-internal::list-message ,obj (sxhash ',mesg) ,@args))
        ((&-p mesg)
         `(tao.logic::&instance-fact ,obj ,args))
        ((logic-method-p mesg)
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


(defun read-@ (stream char)
  (declare (ignore char))
  (list 'tao:deref (read stream T nil T)))


(declaim (inline codnums))


(defun codnums ()
  '((  #o0 . "expr")
    (  #o1 . "exprdyn")
    (  #o2 . "macro")
    (  #o3 . "subst")
    (  #o4 . "closure")
    (  #o5 . "array")
    (  #o6 . "&+")
    (  #o7 . "hclauses")
    ( #o10 . "&+dyn")
    ( #o11 . "subr")
    ( #o12 . "expr-simple")
    ( #o13 . "exprdyn-simple")
    ( #o14 . "subr-simple")
    ( #o15 . "unit-clauses")
    ( #o40 . ":ob")
    ( #o41 . ":qob")
    ( #o42 . ":opt")
    ( #o43 . ":qopt")
    ( #o44 . ":rest")
    ( #o45 . ":qrest")
    ( #o46 . ":aux")
    ( #o47 . ":logic")
    ( #o50 . ":closed")
    ( #o51 . ":optn")
    ( #o52 . ":qoptn")
    (#o100 . "1b-memblk")
    (#o101 . "2b-memblk")
    (#o102 . "4b-memblk")
    (#o103 . "8b-memblk")
    (#o104 . "16b-memblk")
    (#o105 . "32b-memblk")
    (#o106 . "64b-memblk")
    (#o107 . "sys:prestk-memblk")
    (#o110 . "sys:id-hash-memblk")
    (#o111 . "sys:64bloc-memblk")
    (#o113 . "sys:strhead-memblk")
    (#o114 . "sys:locbit-memblk")
    (#o115 . "sys:cell-memblk")
    (#o116 . "sys:vector-memblk")
    (#o117 . "sys:id-memblk")
    (#o120 . "sys:str-memblk")
    (#o121 . "sys:bad-memblk")
    (#o122 . "sys:free-memblk")))


;;; #!codnum
(let ((nums (codnums)))
  (defun codnum-reader (stream char arg)
    (declare (ignore char arg))
    (let* ((codnum (read stream t nil t))
           (num (car (rassoc codnum nums :test #'string-equal))))
      (or num
          (error "Unknown codnum: #!~A." codnum)))))


;;; *EOF*
