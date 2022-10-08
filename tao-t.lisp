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


(defun tao:tconc (list1 list2)
  "tconc                                  関数[#!subr]

<説明>
  形式 : tconc list1 list2
list1 と list2 をこの順でコピーしないで連結する。list1 の最後のセルは、
list2 に結合される。ただし list1 が成長リストである場合、nconc より実行
が速くなる。

<例>
        (tconc '(a) '(b))  ->  (a b)
        (tconc '(a) ())    ->  (a)"
  (etypecase list1
    (list (nconc list1 list2))
    (gl (typecase list2
          (LIST
           (setf (cdr (cdr (gl.cons list1)))
                 list2)
           (setf (cdr (gl.cons list1))
                 (last list2))
           list1)
          (GL
           (setf (cdr (cdr (gl.cons list1)))
                 (car (gl.cons list2)))
           (setf (cdr (gl.cons list1))
                 (last (car (gl.cons list2))))
           (setf (cdr (gl.cons list1))
                 (last (cdr (gl.cons list2))))
           list1)))))


(defun tao:tcons (list new-value)
  "tcons                                  関数[#!subr]

<説明>
  形式 : tcons list new-value
new-value を、成長リスト list の最後の要素として挿入し、その結果を返す。
list が成長リストでない場合は、list を成長リストとする。list がどのよう
な長さでも実行時間は一定。

<例>
  通常のリストを成長リストにしたり、また成長リストを通常のリストにし
たりする方法には以下のような方法がある。

        (!g (tcons nil 1)) -> (1)   ここで  g=(1)  成長リスト
        (tcons g 2) -> (1 2)   ここで  g = (1 2)  成長リスト
        (tcons g 3) -> (1 2 3)  ここで  g = (1 2 3)  成長リスト
        (!g (peelinv g)) -> (1 2 3)  ここで  g = (1 2 3)  通常のリスト
        (!g (tcons (list 1 2 3 4 5) 100)) -> 
        	(1 2 3 4 5 100)  成長リスト"
  (typecase list
    (LIST
     (gl-snoc new-value (list->gl list)))
    (GL
     (gl-snoc new-value list ))))


;;; *EOF*
