(in-package :tao-internal)

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