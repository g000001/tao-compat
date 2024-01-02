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
  '(tao:+ tao:- tao:* tao:/ tao:// tao:= tao:< tao:> tao:<= tao:>= tao:/=))

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


(defun fix-cons (expr)
  (cond ((atom expr) expr)
        (T (if (and (symbolp (nth 1 expr))
                    (string= "." (string (nth 1 expr))))
               (cons (fix-cons (car expr))
                     (fix-cons (caddr expr)))
               (cons (fix-cons (car expr))
                     (fix-cons (cdr expr)))))))


(defun read-|[| (stream char)
  (declare (ignore char))
  (let ((*readtable* tao-no-dots-readtable))
    (let ((expr (read-delimited-list #\] stream 'T)))
      (fix-cons (apply #'infix-to-prefix expr)) ;FIXME
      )))


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


#|(defvar *standard-readtable* (copy-readtable nil))|#


(defvar *nest-level* 0)


(defun make-marker/next-form-alist (markers next-forms)
  (let ((marker/next-form-alist '()))
    (dolist (next next-forms)
      (push (cons (find (named-paren-name next) markers
                        :key #'next-form-marker-name)
                  next)
            marker/next-form-alist))
    marker/next-form-alist))


(defun replace-mark (marker/next-form-alist form)
  (dolist (m/nf marker/next-form-alist)
    (destructuring-bind (m . nf)
                        m/nf
      (setq form
            (if (next-form-marker-named-paren? m)
                (if (next-form-marker-splicing? m)
                    (subst/splicing (named-paren-form nf) m form)
                    (subst (named-paren-form nf) m form))
                (subst (named-paren-form nf) m form)))))
  form)


(defun tao-read-list/heredoc (srm chr)
  (let* ((form (let ((*nest-level* (1+ *nest-level*)))
                 (tao-read-list srm chr)))
         (flat-form (alexandria:flatten form)))
    (if (and (zerop *nest-level*)
             (find-if (lambda (form)
                        (typep form 'next-form-marker))
                      flat-form))
        (flet ((make-canonicalized-next-form (mark)
                 (let ((next (read srm T nil T)))
                   (typecase next
                     (named-paren next)
                     (next-form-marker
                      (make-named-paren :name (next-form-marker-name next)
                                        :form (read srm T nil T)))
                     (T (make-named-paren :name (next-form-marker-name mark)
                                          :form next))))))
          (let* ((markers (remove-if-not #'next-form-marker-p flat-form))
                 (next-forms (mapcar #'make-canonicalized-next-form markers)))
            (replace-mark (make-marker/next-form-alist markers next-forms)
                          form)))
        form)))


(defun terminating-char-p (char)
  (multiple-value-bind (macro? terminating?)
                       (get-macro-character char)
    (and macro? (not terminating?))))


(defstruct next-form-marker name splicing? named-paren?)


(defun subst/splicing (new old list)
  (cond ((null list) '())
        ((atom list) list)
        ((eql old (car list))
         (append new
                 (subst/splicing new old (cdr list))))
        (T (cons (subst/splicing new old (car list))
                 (subst/splicing new old (cdr list))))))


(defun read-\#_ (srm chr arg)
  (declare (ignore chr arg))
  (let ((next-char (peek-char nil srm T nil T)))
    (make-next-form-marker :name (if (or (terminating-char-p next-char)
                                         (find next-char '(#\Space #\Tab #\Newline)))
                                     '||
                                     (read srm T nil T)))))


(defstruct named-paren name form)


(defun named-paren-reader (srm chr arg)
  (declare (ignore arg chr))
  (let ((mark (read srm T nil T)))
    (if (zerop *nest-level*)
        (let* ((end-mark (intern (concatenate 'string "END-OF-" (string mark)))))
          (make-named-paren :name mark
                            :form (loop :for form := (read srm T nil T)
                                        :until (eq end-mark form)
                                        :collect form)))
        (make-next-form-marker :name mark
                               :splicing? T
                               :named-paren? T))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; *EOF*
