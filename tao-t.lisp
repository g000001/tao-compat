(in-package #:tao-compat)

#|
togap                                  関数[#!subr]

<説明>
  形式 : togap arg
arg がトガ ^ のついた式なら t を返し、そうでなければ nil を返す。

<例>
        (togap '^(x y `z)) -> t
|#

#|
 (defmacro togap (&body expr)
  (if (eq '^ (cadar expr))
      t
      nil))
|#


(defun togap (expr)
  (and (eq 'toga (and (consp expr) (car expr)))))


;;  3:19pm Monday, 6 August 2007
;(togap '^(x y `z))
