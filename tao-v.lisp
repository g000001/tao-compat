;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-

(tao:common-lisp)


(in-package #:tao-internal)


(define
 "|sys:v-sem|"
 (subr nil)
 :documentation
 "形式 : sys:v-sem semaphore
プロセスに semaphore を開放させる。ただしこれは、プロセスが実際に
semaphore を占有している場合で、もし、semaphore が占有されていなければ
何もしない。"
 :example
 "")


(define
 "v-sem"
 (subr nil)
 :documentation
 "形式 : v-sem semaphore
semaphore を1 つずつ増やす v-operation を行う。つまり、この関数を呼んだ
プロセスは、semaphore を開放する。プロセスがリソースを使い終え、それを
開放した時に実行されなければならない。"
 :example
 "")


(declaim (inline tao:value (setf tao:value)))


(defun (setf tao:value) (val var)
  (setf (symbol-value var) val))


(define
 "value"
 (subr (sym)
   (symbol-value sym))
 :documentation
 "形式 : value var
var のグローバル値を返す。"
 :example
 "(!y \"asd\")
        (value 'y) -> \"asd\"
        (value 'z) -> (unbound-variable z)
        (prog (p) (special-variables p) (!p 25) (value 'p))
        	-> (unbound-variable p)
        (!p 12) -> 12
        (prog (p) (!p 25) (value 'p)) -> 12
        (prog (p) (special-variables p) (!p 25) (value 'p)) -> 12")


(define
 "values"
 #'values
 :documentation
 "形式 : values &rest arg1 arg2 ... argN
arg1 arg2 ... argN を返す。トップレベルでは、先頭に ! のついた括孤の中
に arg1 arg2 ... argN を表示する。"
 :example
 "(values 1 2 3) -> !(1 2 3)")


(define
 "values-list"
 #'values-list
 :documentation
 "形式 : values-list list
list の要素で構成される多値変数を返す。multiple-value-list の逆関数。"
 :example
 "(values-list '(1 2 3)) = !( 1 2 3)
        (values-list (cons 'a '(p q r))) -> (a p q r)")


;; "vanilla-class" -> object.lisp


(define
 "vcons"
 (subr (vtitle vsize)
   (declare (ignore vtitle))
   (make-array vsize :initial-element nil))
 :documentation
 "形式 : vcons vtitle vsize
名称が vtitleで、大きさが vsize のベクタを作成し、全ての要素を nil に
初期化する。"
 :example
 "(!v (vcons \"vec\" 8)) -> {vector}69984(\"vec\" . 8)")


(define
 "vdir"
 (expr nil)
 :documentation
 "形式 : vdir &opt pathname output-stream
pathname での指定した全てのファイルについての情報が、アルファベット順に
プリントされる。pathname の既定値は、現在接続されているディレクトリに
おける全ファイル。pathname にはワイルドカードが指定可能。"
 :example
 "(vdir) = (vdir \"*.*\")
        (vdir \"*:<*>*.*.*\")
        	 全てのディレクトリの全てのファイルの情報をプリント
        (vdir \"cs:<dir1>\")
        	 ディレクトリ cs:<dir1> の全ファイルの情報をプリント
        (vdir \"elis::as:<dir2>kk\")
        	 ファイル kk についての情報をプリント
        (vdir \"<nue>*.tao.*\")
        	 ディレクトリ <nue> 中のファイルタイプが tao である全て
        	 のファイルの情報をプリント")


(define
 "vector"
 #'vector
 :documentation
 "形式 : vector &rest object1 object2 ... objectN
初期値 object1 object2 ... objectN をもつ一般単純ベクタを生成し、その
結果を返す。"
 :example
 "(!a (vector '(a b) '(c d) '(e f) '(g h)))
        	-> {vector}48381(common:simple-general-ector . 4)
        (show-vector a) ->
        vtitle: common:simple-general-vector  vsize: 4
            0 kdr: (a b)
            1 kar: (c d)
            2 kdr: (e f)
            3 kar: (g h)
        {vector}48381(common:simple-general-vector . 4)")


(define
 "vector"
 #'vector
 :documentation
 "インスタンスは vcons または vector によって生成されたベクタ。"
 :example
 "")


(define
 "vector-equal"
 (subr nil)
 :documentation
 "形式 : vector-equal vector1 vector2
vector1 と vector2 の大きさ、名称及びすべての要素の値が equal なら
t 、そうでない場合は nil を返す。"
 :example
 "(!x (vcons 'asd 5)) -> {vector}70034(asd . 5)
        (!y (vcons 'asdf 5)) -> {vector}70042(asdf . 5)
        (!z (vcons 'asd 5)) -> {vector}70050(asd . 5),
        (vector-equal x y) -> nil
        (vector-equal x z) -> t
        (!(nthv 2 x) 14) -> 14
        (vector-equal x z) -> nil")


(define
 "vector-filler"
 (expr nil)
 :documentation
 "形式 : vector-filler vector filler
vector のすべての要素に、filler を代入する。"
 :example
 "(!v (vcons 'vec 9)) -> {vector}70024(vec . 9),
        (vector-filler v 1) -> {vector}70024(vec . 9).")


(define
 "vector-pop"
 #'vector-pop
 :documentation
 "形式 : vector-pop vector
vector を指すフィルポインタの値を 1 減少し、新しいフィルポインタの値
によって指されるベクタの要素の値を返す。vector は、フィルポインタを持
つ 1 次元の配列でなければならない。また、フィルポインタが 0 の場合は、
警告を発する。"
 :example
 "(!aa (make-array 5 :fill-pointer t)) 
        		-> {vector}66922(common:array . 4)
        (vector-pop aa) -> nil
        (fill-pointer aa) -> 4
        (vector-pop aa) -> nil
        (fill-pointer aa) -> 3")


(define
 "vector-push"
 #'vector-push
 :documentation
 "形式 : vector-push object vector
vector のフィルポインタが指す要素に、object を代入し、フィルポインタの
値を 1 増加させ、新しいフィルポインタの値を返す (フィルポインタがベクタ
の要素を指していなければ nil を返す)。vector は、フィルポインタを持つ
1 次元の配列でなければならない。"
 :example
 "(!aa (make-array 5 :fill-pointer t)) 
        		-> {vector}66922(common:array . 4)
        (vector-pop aa) -> nil
        (fill-pointer aa) -> 4
        (vector-push '(1 2 3) aa) -> 4
        (fill-pointer aa) -> 5
        (show-vector (nthc 0 aa)) ->
        vtitle: common:general-vector  vsize: 5
        0 kdr: nil
        1 kar: nil
        2 kdr: nil
 	3 kar: nil
        4 kdr: (1 2 3)
        {vector}1239136(common:general-vector . 5)")


(define
 "vector-push-extend"
 #'vector-push-extend
 :documentation
 "形式 : vector-push-extend object vector &opt extend
vector のフィルポインタが指す要素に、object を代入し、フィルポインタの
値を 1 増加させ、新しいフィルポインタの値を返す (フィルポインタがベクタ
の要素を指していなければ nil を返す)。フィルポインタが増加し、ベクタの
要素が不足した時は、要素を追加する。vector は、フィルポインタを持つ
1 次元の配列でなければならない。"
 :example
 "(!aa (make-array 5 :fill-pointer t)) 
        		-> {vector}64808(common:array . 4)
        (fill-pointer aa) -> 5
        (vector-pop aa) -> nil
        (fill-pointer aa) -> 4
        (vector-push-extend '(a b c) aa) -> 5
        (fill-pointer aa) -> 5
        (show-vector (nthc 0 aa)) ->
        vtitle: common:general-vector  vsize: 5
        0 kdr: nil
        1 kar: nil
        2 kdr: nil
 	3 kar: nil
        4 kdr: (a b c)
        {vector}1239484(common:general-vector . 5)")


(define
 "vectorp"
 #'cl:vectorp
 :documentation
 "形式 : vectorp vector
vector がベクタなら vector を返し、それ以外なら nil を返す。"
 :example
 "(!x (vcons \"asd\" 5)) -> {vector}57931(\"asd\" . 5),
        (vectorp x) -> {vector}57931(\"asd\" . 5).")


(define
 "common:vectorp"
 #'cl:vectorp
 :documentation
 "形式 : common:vectorp vector
vector がベクタなら t 、それ以外なら nil を返す。"
 :example
 "(!x (array 7)) -> {applobj}1793347(#!array . 8)
        (common:vectorp x) -> {applobj}1793347(#!array . 8)
        (!y (vcons \"asd\" 5)) -> {vector}1793752(\"asd\" . 5)
        (common:vectorp y) -> {vector}1793752(\"asd\" . 5)")


(define
 "vprint"
 (subr (object &optional stream)
  (let ((len (cl:length (princ-to-string object))))
    (values (princ object stream) len)))
 :documentation
 "形式 : vprint object &opt stream
stream に object の値をプリントし、object の値を返す。stream の既定値は、
*standard-output* 。エスケープ文字は、出力されない。"
 :example
 "(vprint 'abc) -> abc!(abc 3)
        (vprint 70) -> 70!(70 2)
        (vprint \"abc def\") -> \"abc def\"!(\"abc def\" 9)")


(define
 "vsize"
 #'cl:length
 :documentation
 "形式 : vsize vector
vector の大きさを返す。"
 :example
 "(!x (vcons 'asd 5)) -> {vector}58002(asd . 5)
        (!y (vcons 'asd 20)) -> {vector}58010(asd . 5),
        (vsize x) -> 5
        (vsize y) -> 20.")


(define
 "vt100-terminal"
 (class T)
 :documentation
 "インスタンスが vt100 ターミナルのクラス。"
 :example
 "")


(define
 "vt200-terminal"
 (class T)
 :documentation
 "インスタンスが vt200 ターミナルのクラス。"
 :example
 "")


(define
 "vtitle"
 (subr nil)
 :documentation
 "形式 : vtitle vector
vector の名称を返す。"
 :example
 "(!x (vcons 'asd 5)) -> {vector}57981(asd . 5)
        (!y (vcons \"asd\" 5)) -> {vector}57989(\"asd\" . 5)
        (vtitle x) -> asd
        (vtitle y) -> \"asd\"")


;;; *EOF*
