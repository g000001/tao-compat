(tao:tao)
(in-package #:tao-internal)

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


(defun tao:togap (expr)
  (and (eq 'toga (and (consp expr) (car expr)))))


;;  3:19pm Monday, 6 August 2007
;(togap '^(x y `z))


(defmacro tao:trim (var list &rest forms)
  "trim                                   関数[#!subr]

<説明>
  形式 : trim &rest var list form1 form2 ... formN
まず、list の第 1 要素を var として form1 form2 ... formN を順に評価
する。次に、list の第 2 要素を var として form1 form2 ... formN を順に
評価する。以下、list の最後の要素までこれを繰り返す。そして、最後の
フォーム formN の評価結果の値をすべて調べ、それが non-nil であった時の、
最新の var の値を並べてリストとして返す。

<例>
        (trim i '(1 2 3 4 5 6 7) (oddp i)) -> (1 3 5 7)
        (trim i '(1 2 3 4 5) (!!1+ !i) (evenp i)) -> (2 4 6)
        (trim i (index 1 10) (oddp i) (evenp i)) -> (2 4 6 8 10)"
  `(cl:loop :for ,var :in ,list
            :when (cl:progn ,@forms) :collect ,var))

#|(progn
  (trim i '(1 2 3 4 5 6 7) (oddp i))
;=>  (1 3 5 7)
  (trim i '(1 2 3 4 5) (!!1+ !i) (evenp i))
;=>  (2 4 6)
  (trim i (tao:index 1 10) (oddp i) (evenp i))
;=>  (2 4 6 8 10)
  )|#

