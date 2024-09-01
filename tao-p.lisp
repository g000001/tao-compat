(tao:common-lisp)


(in-package #:tao-internal)


(define
 "p-sem"
 (subr nil)
 :documentation
 "形式 : p-sem semaphore
semaphore をチェックする p-オペレーションを実行する。
semaphore により示された使用可能なリソースの数を返す。
その値が 0 なら、この関数を呼び出したプロセスは、値が増えるまで待たな
ければならない。semaphore の値が 1 ならば、値を 1 減じる。"
 :example
 "(!abc (make-instance 'semaphore :name 'abc))
        (de test1 (semaphore name)
        	(loop (&aux count)
        		(:init (!count 0))
        		(:until (= count 5))
        		(p-sem semaphore)
        		(print \"test-1\")
        		(print-time)
        		(sleep 5)
        		(v-sem semaphore)
        		(inc count)))
        (de test2 (semaphore name)
        	(loop (&aux count)
        		(:init (!count 0))
        		(:until (= count 5))
        		(p-sem semaphore)
        		(print \"test-2\")
        		(print-time)
        		(!!cons name !queue)
        		(sleep 2)
        		(v-sem semaphore)
        		(inc count)))
        (!process1
         (process-fork 'def-1 'test1 (list abc 'def-1)
        	:standard-output *standard-output*))
        (!process2
         (process-fork 'def-2 'test2 (list abc 'def-2)
        	:standrd-output *standard-output*))
        ->
           \"test-1\" 8 Apr-87 13:02:39
           \"test-1\" 8-Apr-87 13:02:44
           \"test-2\" 8-Apr-87 13:02:45
           \"test-2\" 8-Apr-87 13:02:47
           \"test-2\" 8-Apr-87 13:02:49
           \"test-1\" 8-Apr-87 13:02:50
           \"test-2\" 8-Apr-87 13:02:51
           \"test-2\" 8-Apr-87 13:02:53
           Fork process completed! def-2 with: 5
           \"test-1\" 8-Apr-87 13:02:55
           \"test-1\" 8-Apr-87 13:03:00
           Fork process completed! def-1 with: 5")


(define
 "p-sem-with-timeout"
 (subr nil)
 :documentation
 "形式 : p-sem-with-timeout tick semaphore
時間切れを設定できることを除いては、関数 p-sem と同じ。
semaphore をチェックする p-オペレーションを実行する。
semaphore の値が 0 なら、この関数を呼び出したプロセスは、値が増えるか、
時間切れとなるまで待たなければならない。時間切れは、tick で指定。
セマフォをひとつも得ることなく時間切れになれば nil を返す。
うまくセマフォを占有できた時は t を返す。"
 :example
 "")


(define
 "package-name"
 #'package-name
 :documentation
 "形式 : package-name package
package のパッケージ名を文字列として返す。
パッケージとは {vector}34567(package .  10) などであり、
パッケージ名とは make-package の中で指定された文字列名である。"
 :example
 "(package-name (list-all-global-packages)) ->
                 (\"apropos\" \"net\" \"step\" \"bas\" \"sys\" \"key\")
        いまここでTAOのシステムパッケージ名が見える。
        (package-name (list-all-packages))
        	 -> (\"dbg\" \"zetalisp\" \"maclisp\")")


(define
 "package-nicknames"
 #'package-nicknames
 :documentation
 "形式 : package-nicknames package
package のニックネームを文字列のリストで返す。"
 :example
 "(package-nicknames (list-all-global-packages)) -> nil
        (!a (make-package \"hiro\" :nickname '(\"s\")) ->
        	{vector}1312994(package . 12)
        (package-nickname a) -> (\"s\")
        (make-package) で nicknameを付ける時、nicknameは 1文字。")


(define
 "package-shadowing-symbols"
 #'package-shadowing-symbols
 :documentation
 "形式 : package-shadowing-symbols &opt package
package (既定値、カレントパッケージ) 内のシンボルのうち関数 shadow 
または shadow-import によって他のシンボルをシャドウしているものを
リストにして返す。"
 :example
 "(package-shadowing-symbols) -> nil
        (shadow 'car) -> t
        (package-shadowing-symbols) -> (car)
        (car '(1 2 3)) -> エラー")


(define
 "package-use-list"
 #'package-use-list
 :documentation
 "形式 : package-use-list &opt package
package (既定値、カレントパッケージ) がユースしている全てのパッケージ
名をリストで返す。"
 :example
 "(package-name (package-use-list)) -> nil
        (use \"net\")
        (package-name (package-use-list)) -> (\"net\")")


(define
 "package-used-by-list"
 #'package-used-by-list
 :documentation
 "形式 : package-used-by-list package
package をユースしている全てのパッケージ名をリストで返す。"
 :example
 "(package-name (package-used-by-list (find-package \"dbg\"))) ->
        	(\"abc\" \"window\")
        (package-name (package-used-by-list (find-package \"bas\"))) ->
        	(\"dbg\" \"zetalisp\" \"maclisp\")")


(define
 "packagep"
 #'cl:packagep
 :documentation
 "形式 : packagep pkg
pkg が パッケージなら t を返し、それ以外なら nil を返す。"
 :example
 "(!gg (make-package 'gonta)) -> {vector}61567(package . 10)
        (packagep gg) -> t
        (packagep 'gonta) -> nil
        (packagep (global-package \"bas\")) -> t")


(define
 "pairlis"
 #'pairlis
 :documentation
 "形式 : pairlis car-list cdr-list
リスト car-list の各要素と cdr-list の各要素を対にした連想リストを作
り、それを返す。"
 :example
 "(pairlis '(okuno ohsato hibino) '(3323 3668 3589))
        	 -> ((okuno . 3323) (ohsato . 3668) (hibino . 3589))")


(define
 "parent-package"
 (expr nil)
 :documentation
 "形式 : parent-package &opt package
package (既定値はカレントパッケージ) の親パッケージを返す。"
 :example
 "(package-name (parent-package)) -> \"bas\"")


(define
 "parent-package-chain"
 (expr nil)
 :documentation
 "形式 : parent-package-chain &opt package
package (既定値はカレントパッケージ) から根パッケージ 
(sys:univ-package) へとつながる親リンクチェーンにあるすべての
パッケージをリストにして返す。"
 :example
 "(package-name (parent-package-chain)) -> (\"bas\" \"univ\")")


(define
 "parse-integer"
 #'parse-integer
 :documentation
 "形式 : parse-integer string &opt start end radix junk-allowed
string から整数を 1 つ読み込んで、それを返す。junk-allowed が nil 
(既定値) であれば string のstart と end の範囲(省略時、全体)を、基数 
radix により読み込む。string が整数を全く含まなければエラーを警告する。
junk-allowed が nil 以外なら整数の前後に空白文字があっても無視し、う
まく整数が読み込めなくてもエラーを出さずに nil を返す。"
 :example
 "")


(define
 "parse-namestring"
 #'parse-namestring
 :documentation
 "形式 : parse-namestring thing &opt host defaults
thing を対応する パス名 に変換し、それを返す。"
 :example
 "(!u (parse-string \"Ho::bs:<tan>abc.tao\"))
        	-> !({udo}1798330pathname 20)")


(define
 "parse-universal-time"
 (expr nil)
 :documentation
 "形式 : parse-universal-time universal-string
時刻を表す文字列 universal-string をユニバーサルタイム形式の時刻に
変換し、それを返す。関数 get-universal-time を参照。"
 :example
 "(parse-universal-time (pname-of-time (get-universal-time)))
        	-> 2698274161
        (parse-universal-time \" 1-Jan-85 00:00:00\") -> 2682406800")


(define
 "pathname"
 #'pathname
 :documentation
 "形式 : pathname path
ファイル指定 path をパス名の udo に変換して返す。
path は、文字列、シンボル、パス名の udo、またはストリームの udo 。"
 :example
 "(pathname \"abc.tao\") -> !({udo}1827722pathname 7)
        (namestring (pathname \"abc.tao\")) -> \"abc.tao\"")


(define
 "pathname-device"
 #'pathname-device
 :documentation
 "形式 : pathname-device pathname
ファイル pathname のデバイス名を返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれでもよい。"
 :example
 "(pathname-device *default-pathname-defaults*) -> \"bs\"
        (pathname-device \"Al::cs:<user>test.tao.5\") -> \"cs\"")


(define
 "pathname-directory"
 #'pathname-directory
 :documentation
 "形式 : pathname-directory pathname
ファイル pathname のディレクトリ名を返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれかでよい。"
 :example
 "(pathname-directory *default-pathname-defaults*) -> \"dire\"
        (pathname-device \"Al::cs:<user>test.tao.5\") -> \"user\"")


(define
 "pathname-host"
 #'pathname-host
 :documentation
 "形式 : pathname-host pathname
ファイル pathname のホスト名を返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれかでよい。"
 :example
 "(pathname-host *default-pathname-defaults*) -> \"Ho\"
        (pathname-device \"Al::cs:<user>test.tao.5\") -> \"Al\"")


(define
 "pathname-name"
 #'pathname-name
 :documentation
 "形式 : pathname-name pathname
ファイル pathname のファイル名を返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれかでよい。"
 :example
 "(pathname-name *default-pathname-defaults*) -> \"foo\"
        (pathname-name \"Al::cs:<user>test.tao.5\") -> \"test\"")


(define
 "pathname-type"
 #'pathname-type
 :documentation
 "形式 : pathname-type pathname
ファイル pathname のタイプを返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれかでよい。"
 :example
 "(pathname-type *default-pathname-defaults*) -> \"tao\"
        (pathname-type \"Al::cs:<user>test.tao.5\") -> \"tao\"")


(define
 "pathname-version"
 #'pathname-version
 :documentation
 "形式 : pathname-version pathname
ファイル pathname のバージョン番号を返す。
pathname は、文字列、シンボル、パス名の udo、ストリームの udo の
いずれかでよい。"
 :example
 "(pathname-version *default-pathname-defaults*) -> :newest
        (pathname-version \"Al::cs:<user>test.tao.5\") -> 5")


(define
 "pathnamep"
 #'cl:pathnamep
 :documentation
 "形式 : pathnamep object
object がパス名の udo なら t 、そうでなければ nil を返す。"
 :example
 "(pathnamep \"cs\") -> nil
        (pathnamep (make-pathname :host \"asd\")) -> t
        (pathnamep *default-pathname-defaults*) -> t")


(define
 "pc98k-terminal"
 (class T)
 :documentation
 "ターミナルのクラス。インスタンスは pc98k。"
 :example
 "")


(define
 "peek-char"
 #'peek-char
 :documentation
 "形式 : peek-char &opt peek-type input-stream
peek-type が、t の時、空白文字はスキップして、その次に読まれるべき 
1 文字を返す。peek-type が nil の時、ストリーム input-stream から次
に読まれるべき 1 文字を返す。peek-type の既定値は、nil 。
input-stream の既定値は *standard-input* の値。"
 :example
 "\"asd.tao\" の内容が  D aN......とする。
        (!aa (open \"asd.tao\")) -> {udo}1172343file-stream
        (peek-char t aa) -> \"D\"
        (read-char aa) -> \"D\"
        (peek-char nil aa) -> #\\space
        (peek-char t aa) -> \"a\"    スペ-スはスキップする。
        (read-char aa) -> \"a\"
        (peek-char t aa) -> \"N\"
        (read-char aa) -> \"N\"")


(define
 "peelinv"
 (subr (object)
   (typecase object
     (GL (car (gl.cons object)))
     (T object)))
 :documentation
 "形式 : peelinv object
object の潜在ポインタを順に取り去っていき、最初の顕在ポインタ、
つまり通常のポインタに行き当たったらそこで止まる。"
 :example
 "(peelinv '(a b c)) ->(a b c)
        (peelinv 'd) -> d")


(define
 "phase"
 #'phase
 :documentation
 "形式 : phase complex
複素数 complex を極座標表現での偏角を表現するためのラジアン値に変更
して返す。"
 :example
 "(phase #c(2 3)) -> 0.98279972324799f0
        (phase #c(4 -1)) -> -0.244978663126964f0")


(define
 "pi"
 (constant pi)
 :documentation
 "円周率(π)を表す値が格納されているシステム定数 (大域定数)。
本システムでは 3.1415926535898f0。"
 :example
 "")


(define
 "pipe-stream"
 (class two-way-stream)
 :documentation
 "インスタンスが パイプストリーム であるクラス。
入出力両方向に動作する。入出力時に、2 つ以上のプロセスがこの
ストリームを分割し、データはこのストリームを通してあるプロセスからも
う一方のプロセスにパスされる。"
 :example
 "")


#-(or allegro lispworks ccl)
(defsynonym (setf tao:plist) (setf cl:symbol-plist))

#+lispworks
(defun (setf tao:plist) (var sym)
  (system::set-symbol-plist sym var))


#+ccl
(defun (setf tao:plist) (var sym)
  (ccl::set-symbol-plist sym var))

#+allegro
(defun (setf tao:plist) (var sym)
  (setf (excl::sy_plist sym) var))

(define
 "plist"
 #'cl:symbol-plist
 :documentation
 "形式 : plist symbol
symbol の属性リストを返す。
symbol の属性リストを作るためにも用いられる。"
 :example
 "(plist 'aa) aa の属性リストを返す。
        (!(plist 'aa) new-plst)
        	 aa の属性リスト全部を new-plst で置き換える。
        (!(plist 'xxx) '(a 1 b 2 c 3 d 4)) -> (a 1 b 2 c 3 d 4)
        	 xxx は (a 1 b 2 c 3 d 4) になる。")


(define
 "plus"
 #'+
 :documentation
 "形式 : plus &rest number1 number2 ... numberN
number1, number2, ... numberN の値の和を返す。
(+ number1 number2 ... numberN) と同じ。"
 :example
 "(plus 1 2 3) -> 6
        (plus) -> 0")


(define
 "plusp"
 #'plusp
 :documentation
 "形式 : plusp number
number が正の数なら、その値を返し、それ以外なら nil を返す。"
 :example
 "(plusp 543654) -> 543654
        (plusp -123) -> nil
        (plusp 0) -> nil")


(define
 "pname"
 (subr (object)
   (typecase object
     (string object)
     (symbol (string object))
     (otherwise (write-to-string object))))
 :documentation
 "形式 : pname object
object の値を文字列として返す。symbol-name 参照。
(pname x) は、x が string である点を除けば (write-to-string x) と同じ。"
 :example
 "(pname 'uvwxyz) -> \"uvwxyz\"
        (pname \"abcdefg\") -> \"abcdefg\" ( \"\"abcdefg\"\"ではない)
        (pname '(a . b)) -> \"(a . b)\"")


(define
 "pname-of-time"
 (expr (universal-time)
   (multiple-value-bind (ss mm hh d m y) (decode-universal-time universal-time)
     (format nil "~2,,D-~
                ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]-~
                ~2,'0D ~
                ~2,'0D:~
                ~2,'0D:~
                ~2,'0D" d (1- m) (if (> y 1999) (- y 2000) (- y 1900)) hh mm ss)))
 :documentation
 "形式 : pname-of-time universal-time
ユニバーサルタイム形式の時刻を、文字列に変換し、返す。
関数 get-universal-time を参照。"
 :example
 "(pname-of-time (get-universal-time)) -> \" 3-Jul-85 13:50:35\"")


(define
 "pop"
 (cl-macro pop)
 :documentation
 "形式 : pop 'var
リスト変数 var の内容の car 部 (リストの第 1 要素) を返す。
副作用として、var の内容の cdr 部が、var の内容の新しい値となる。
  (pop x) = (progi ^(car x) (cdr! x))。
ただし、(pop x) の x は一度しか評価されない。"
 :example
 "x = (1 2 3 4) の場合
        (pop x) -> 1 であり、同時に x = (2 3 4) となる。さらに続けると、
        (pop x) -> 2 であり、同時に x = (3 4) となる。
        y = ((1 2) 3 (4 5) 6) の場合
        (pop y) -> (1 2)であり、同時に y = (3 (4 5) 6) となる。さらに
        (pop y) -> 3 であり、同時に y = ((4 5) 6) となる。さらに
        (pop y) -> (4 5) であり、同時に y = (6) である。")


(define
 "position"
 #'position
 :documentation
 "形式 : position item seq &key :from-end :test :test-not
                                :start :end :key
:from-end が nil (既定値) ならシーケンス seq の :start から :end まで
の範囲で、item が、条件 :test または :test-not を満たす最初の要素 
(:from-end が nil 以外なら最後の要素) の添字の値を返す。
(該当する要素が見つからなければ、nil を返す)。:from-ed と :test-not 
を同時に指定してはいけない。"
 :example
 "(position 'a '(1 2 a b c )) -> 2
        (position 'a '(1 2 a b c d) :start 3 :end 5) -> nil
        (position '3 '(1 2 3 4 5) :test #'<) -> 3
        (position '3 '(1 2 3 4 5) :test-not #'< :from-end t) -> 2")


(define
 "position-if"
 #'position-if
 :documentation
 "形式 : position-if test seq &key :from-end :start :end :key
シーケンス seq の :start から :end までの範囲で、条件 test を満足する
要素を検索し、最初の要素 (:from-end が nil 以外なら最後の要素) の添字
の値を返す。(満足する要素が見つからなければ、nil を返す)。"
 :example
 "(position-if #'evenp '(1 2 3 4 5)) -> 1
        (position-if #'evenp '(1 2 3 4 5) :from-end t) -> 3
        (position-if #'oddp '(1 2 3 4 5) :start 3 :end 5) -> 4")


(define
 "position-if-not"
 #'cl:position-if-not
 :documentation
 "形式 : position-if-not test seq &key :from-end :start :end :key
シーケンス seq の :start から :end までの範囲で、条件 test を満足し
ない要素を検索し、最初の要素 (:from-end が nil以外なら最後の要素) の
添字の値を返す。(満足しない要素が見つからなければ、nil を返す)。"
 :example
 "(position-if-not #'evenp '(1 2 3 4 5)) -> 0
        (position-if-not #'evenp '(1 2 3 4 5) :from-end t) -> 4
        (position-if-not #'oddp '(1 2 3 4 5) :start 3 :end 5) -> 3")


(define
 "pprint"
 #'pprint
 :documentation
 "形式 : pprint object &opt stream
object の印字表現を、変数 *pretty-print* の値に従って、その前に改行を
付けて、stream へ出力する。この関数は、値を返さない。
stream の既定値は、変数 *standard-output* の値。"
 :example
 "(!print-pretty* t) -> t
        (pprint '((a b) c d)) ->
        			 ((a b)
        			  c d)
                                 t
        (!*print-pretty* nil) -> nil
        (pprint '((a b) c d)) ->
        			 ((a b)
        			  c d)")


(define
 "prin1"
 #'prin1
 :documentation
 "形式 : prin1 object &opt stream
object の印字表現を、stream に出力し、object の値を返す。
object の中に、escape 文字があってもかまわない。
stream の既定値は、変数 *standard-output* の値。
(prin1 x y) = (common:write x :stream y :escape t)
関数 read に対応している基本的な出力関数。"
 :example
 "(prin1 'a) -> aa")


(define
 "prin1-to-string"
 #'prin1-to-string
 :documentation
 "形式 : prin1-to-string object
object を文字列にして返す。object の中に、escape 文字があってもよい。
関数 write-to-string,princ-to-string,prin1 参照。"
 :example
 "(prin1to-string 'asdfg) -> \"asdfg\"")


(define
 "princ"
 #'princ
 :documentation
 "形式 : princ object &opt stream
object の印字表現を、stream に出力し、object の値と出力文字数を返す。
出力は escape 文字を持たない。
stream の既定値は、変数 *standard-output* の値。
(princ object output-stream) = (write object :stream ouput-stream 
        			             :escape nil)"
 :example
 "(princ 'a) -> a!(a 1)
        (princ 'abcdef) -> abcdef!(abcdef 6)")


(define
 "princ-to-string"
 #'princ-to-string
 :documentation
 "形式 : princ-to-string object
object を文字列にして返す。出力は escape 文字を持たない。
関数 write-to-string,prin1-to-string,princ 参照。"
 :example
 "(princ-to-string 'qwert) -> \"qwert\"")


(define
 "prins"
 (subr (object &optional stream)
  (princ object stream))
 :documentation
 "形式 : prins object &opt stream
stream に object をプリントし、object の値を返す。
stream の既定値は、変数 *standard-output* の値。
object が数なら、prins は、関数 tyo と同じ。
object がシンボルなら、その印字名をプリントする。
object が文字列なら、ダブルクォートなしに、プリントする。"
 :example
 "(prins 70) -> F70
        (prins 'abc) -> abcabc
        (prins \"abc def\") -> abc def\"abc def\"")


(define
 "print"
 #'print
 :documentation
 "形式 : print object &opt stream
改行文字、object、1 文字の空白を、stream に出力し、object の値を返
す。stream の既定値は、変数 *standard-output* の値。"
 :example
 "(print 'abc) -> 
        		abc abc
        (print \"abc def\") -> 
        		     \"abc def\" \"abc def\"")


(define
 "print-methods-of-class"
 (exprdyn nil)
 :documentation
 "形式 : print-methods-of-class class-vect
クラス class-vect のメソッドを表示する。"
 :example
 "(defmethod (abc tashizan) (x y &aux z) (!z (+ x y)))
        	-> tashizan
        (defmethod (abc hikizan) (x y &aux z) (!z (- x y)))
        	-> hikizan
        (print-methods-of-class 'abc) -> Id methods are: t")


(define
 "print-time"
 (expr nil)
 :documentation
 "形式 : print-time &opt universal-time
ユニバーサルタイム形式の時刻 universal-time を改行なしに文字列で
プリントし、nil を返す。universal-time の既定値は、
(get-universal-time) の返す値。"
 :example
 "(print-time) とすると、3-Jul-85 13:50:35 がプリントされる。")


(define
 "probe-file"
 #'probe-file
 :documentation
 "形式 : probe-file file
file が存在しなければ nil を返し、あれば、そのパス名を返す。
file があるファイルに結合され、オープンされたストリームを指定されれば、
結合されたファイル名を生成する。関数 truename,open 参照。"
 :example
 "(probe-file \"abc.tao\") -> \"Ho::bs:<dire>abc.tao\"
        (probe-file \"cba.tao\") -> nil")


(define
 "process"
 (expr nil)
 :documentation
 "形式 : process job-number
job-number のプロセスを返す。"
 :example
 "(process 4) -> {udo}1137949")


(define
 "process-allow-schedule"
 (subr nil)
 :documentation
 "形式 : process-allow-schedule
通常に処理を行っているプロセスを直ちに処理待ち行列に並ばせ、
その待ち行列の先頭のプロセスをポップして、それに処理を行わせる。"
 :example
 "")


(define
 "process-fork"
 (exprdyn nil)
 :documentation
 "形式 : process-fork name func args &key :standard-input
                      :standard-output :completion-message
                      :bottom-bias :shared-variables :survive
名前が name のプロセスを生成し、それをフォークし、処理を行わせる。
つまり、プロセスにおいて、関数 func を引数リスト args に適用して
実行させる。キーワード引数 :standard-input と :standard-output は、
各々、フォークプロセスの *standard-input* と *standard-output* を
指定する。リターン値は、フォークプロセス。
フォークプロセスの属性リストは、親プロセスでフォークされたことを示す
属性 forked を持つ。キーワード引数の既定値は次のようになる。
        :standard-input は null-stream
        :standard-output は null-stream
        :completion-message は t
        :bottom-bias は nil
        :shared-variables は nil
        :survive は nil
フォークプロセスが完了したら、ターミナルベルを鳴らし、
\"fork process completed! name with: retur-value of form\" を表示する。"
 :example
 "(de receive-fun ()
        	(loop (&aux x)
        		(!x (receive-mail box-0))
        		(:until (string= x \"owari1\"))
        		(write x)))
        (de send-fun ()
        	(send-mail box-0 \"aho\")
        	(sleep 2)
        	(send-mail box-0 \"owari\"))
        (!box-0 (make-instance 'mailbox))
        (!process1
         (process-fork 'test1 'receive-fun '() 
        		:standard-output *standard-oupt*))
        (!process2
         (process-fork 'test2 'send-fun '()))
        ->
           \"aho\"
           Fork process completed! test2 with: \"owari\"
           Fork process completed! test1 with: \"owari\"")


(define
 "process-interrupt"
 (expr nil)
 :documentation
 "形式 : process-interrupt process func args flag
process を中断させる。中断が受け入れられれば、中断前のプロセスの環境で、
関数 func をリスト args に適用する。
ここで、フォーム (apply func args) を form とする。
flag の値が t なら、たとえ process が処理可能な状態でなく待ち状態でも、
ただちに割り込みフォーム form を実行する。
待ち状態のプロセスは、bas:del-proc-from-all-queue  により abort され、
そこで割り込みフォームが評価される。しかしながら flag の値が nil なら、
form で指された割り込みフォームは、プロセスが次に処理を行う時に実行され
る。後者では、割り込みフォームの評価が計算状態を壊さなければ、現在進行
中の計算は、その割り込みフォームの評価が終わった後も処理を続ける。"
 :example
 "(de initial-fun (&aux term-no out-stream)
          (!term-no [[sys:current-process :login] :terno])
          (!out-stream (cdr (nthv (* term-no 2) sys:terno-table)))
          (loop (sleep 1) (write 'test out-stream)))
        (de int-form (&aux term-no out-stream)
          (!term-no [[sys:current-process :login] :terno])
            (!out-stream (cdr (nthv (* term-no 2) sys:terno-table)))
        	(write \"interrupt succeeded!!\" out-stream)))
        (!process1
         (make-process
         'abc
         :priority (1- [sys:current-process :priority])
         :readtable *readtable*
         :interprocess-closure nil
         :initial-function 'initial-fun
         :bottom (max2 4 (1- 
        	[sys:current-process sys:bottom-stack-block#])))))
        (process-preset process1 'initial-fun '())
        (sleep 5)
        (process-interrupt process1 'int-form '() t)
        (sleep 5)
        (process-kill process1)
        -> 
           test
           test
           test
           test
           test
           \"interrupt succeeded!!\"
           test
           test
           test
           test
           test
           (initial-fun int-form {udo}53040process
        	({udo}53040process)
        	t {udo}53040process t
        	({udo}53040process))")


(define
 "process-kill"
 (expr nil)
 :documentation
 "形式 : process-kill process
process を取り除く。process は、リセットされ、直ちに停止され、
プロセスキューとプロセスプールから除かれる。
process が占有している全てのセマフォを開放し、process がオープンした
全てのファイルを棄却する。"
 :example
 "(process-kill process1) -> {udo}53040process")


(define
 "process-peep"
 (expr nil)
 :documentation
 "形式 : process-peep process &opt func args
process が、その環境で、関数 func をリスト args に適用する。
リターン値は、func が返す値。func が省略されると関数 backtraceが使わ
れる。ゆえに、別のプロセスに影響を与えることなく、その環境スタックを
見ることができる。"
 :example
 "")


(define
 "process-preset"
 (expr nil)
 :documentation
 "形式 : process-preset process func &rest args
process の初期関数を関数 func に、その関数の初期引数を引数 args に
セットする。process は、その時リセットされるので、そこに現れる計算は、
全て消去される。そして、func を args に適用することによりあらためて始
める。内部的に関数 process-reset を使っている。"
 :example
 "(de initial-fun (&aux term-no out-stream)
          (!term-no [[sys:current-process :login] :terno])
          (!out-stream (cdr (nthv (* term-no 2) sys:terno-table)))
        	(loop (sleep 1) (write 'test out-stream)))
        (!process1
         (make-process
         'abc
         :priority (1- [sys:current-process :priority])
         :readtable *readtable*
         :interprocess-closure nil
         :bottom (max2 4 (1- 
        	[sys:current-process sys:bottom-stack-block#])))))
        (process-preset process1 'initial-fun '())
        (sleep 20)
        (process-kill process1)
        ->
           test
           ....
           test
           (process-kill process1)
           (initial-fun {udo}1168763process
        	({udo}1165112process {udo}1168763)
        	t {udo}1168763process)")


(define
 "process-reset"
 (expr nil)
 :documentation
 "形式 : process-reset process
process に現行の計算を止めさせ、その初期関数を初期引数に適用し、
再スタートさせる。process が占めている全てのセマフォは開放され、
process がオープンしている全てのファイルは棄却される。
つまり、それらのファイルに加えられた修正は、すべて棄てられて無効になる。
関数 unwind-protect は、この関数に対しては働かない。"
 :example
 "(de initial-fun (&aux term-no out-stream)
          (!term-no [[sys:current-process :login] :terno])
          (!out-stream (cdr (nthv (* term-no 2) sys:terno-table)))
        	(loop (sleep 1) (write 'test out-stream)))
        (!process1
         (make-process
         'abc
         :priority (1- [sys:current-process :priority])
         :readtable *readtable*
         :interprocess-closure nil
         :initial-function 'initial-fun
         :bottom (max2 4 (1- 
        	[sys:current-process sys:bottom-stack-block#])))))
        (process-reset prcess1)
        (sleep 20)
        (process-kill process1)
        -> 
           test
           ....
           test
           (initial-fun {udo}1160649process
        	({udo}1160649process)
        	t ({udo}116064process))")


(define
 "tao.sys:process-stop"
 (subr nil)
 :documentation
 "カレントプロセスの処理を停止させる。"
 :example
 "")


(define
 "proclaim"
 #'proclaim
 :documentation
 "形式 : proclaim decl
宣言子 decl にグローバルな宣言を与える。"
 :example
 "(proclaim '(special x))  変数 x をスペシャル変数として定義
        (proclaim '(type float tolerance))
          tolerance の動的な値は常に浮動小数点数になることを定義
        (proclaim '(inline floor))
          floor はコンパイラによってインラインに開かれてコード化される
          ことを定義")


(define
 "prog"
 (macro (binds &body tags-and-forms)
     (let ((value (gensym "value")))
       `(let (,value
              ,@binds)
          (declare (dynamic-extent ,value))
          (tagbody ,@(butlast tags-and-forms)
                   (setq ,value (multiple-value-list ,@(last tags-and-forms))))
          (values-list ,value))))
 :documentation
 "形式 : prog &rest [exit-id] var-list form1 form2 ... formN
nil に束縛された var-list 内の変数で form1 form2 ... formN を
逐次評価する。var-list 内の変数は prog 内で部分的に有効な一時的な変数。
prog を formN が (return) や (return var) の実行なしで評価するなら、
formN の値を返す。prog は (return var) が評価されると、var の値を返す。
prog は (return) が評価されると、{undef}0 を返す。formi がアトムなら、
評価されず、go タグとして解釈される。"
 :example
 "(prog (x) (return x)) -> nil
        (prog (x) (!x 1) (+ x 3) x) -> 4 (last x is not evaluated)
        (prog (x y z) (!x 1) (!y (x + 1))
               (!z (y + 1)) (list x y z)) -> (1 2 3)
        (prog (x)
              (!x 1)
        loop1 (do-some-work x)
              (dec x)
              (cond ((x = 100) (return)) (t (go loop1))) )
                -> {undef}0  (value is not relevant)
        exit-id はオプション。
        これは非局所脱出の場所を指示するのに使用。
        (prog outer-loop (x y)
              ...
              (prog (u v) ... (return var outer-loop) ...)
              ... )
        (return var outer-loop) が実行されると、評価の制御は
        (prog outer-loop ...) の外に出る。")


(define
 "common:prog"
 (cl-macro prog)
 :documentation
 "形式 : common:prog ((var1 val-form1) (var2 val-form2) ...)
                       form1 form2 ...
フォーム val-form1 val-form2 ... を順に評価する。
次に、変数 var1 var2 ... を各々、val-form1 val-form2 ... の評価結果
に同時に束縛する。そして、フォーム form1 form2 ... を順に評価し、nil
を返す。val-formi は省略可能で、その場合は vari は nil となる。"
 :example
 "(common:prog (x) (return x)) -> nil
        (common:prog (n) (!x 1) (+ x 3) x) -> nil
        (common:prog (x y z) (!x 1) (!y (x + 1))
                      (!z (y + 1)) (list x y z)) -> nil")


(define
 "common:prog*"
 (cl-macro prog*)
 :documentation
 "形式 : common:prog* ((var1 val-form1) (var2 val-form2) ...)
                        form1 form2 ...
フォーム val-form1 を評価し、変数 var1 をその評価結果に束縛する。
次に、val-form2 を評価し、var2 をその評価結果に束縛する。
以下、逐次的に val-formi を評価し vari をその評価結果に束縛していく。
そして、フォーム form1 form2 ... を順に評価し、nil を返す。
val-formi は省略可能で、その場合は vari は nil となる。 
ローカル変数の初期値の評価とその束縛は逐次実行されるので、先に束縛
されたローカル変数 (var1) の束縛結果を次のローカル変数 (var2) の
初期値の評価に使うことができる。"
 :example
 "(common:prog* ((y '( 2 3)) (x (car y)))
        	       (return x)) -> 1")


(define
 "prog1"
 (cl-macro prog1)
 :documentation
 "形式 : prog1 form1 &rest form2 ...
form1 form2 ... を順に評価し、form1 の評価値を返す。
(prog1 form1 form2 ... ) = (progi ^form1 form2 ... )"
 :example
 "(prog1 1 2 3 4 5) -> 1
        (!x '(1 2 3)) -> (1 2 3)
        (prog1 (car x) (!x (cdr x))) -> 1
        x -> (2 3)")


(define
 "common:prog1"
 (cl-macro prog1)
 :documentation
 "形式 : common:prog1 form1 &rest form2 ...
form1 form2 ... を逐次評価し、form1 の値を返す。"
 :example
 "(!x '(a b c)) -> (a b c)
        (!y '(1 2 3)) -> (1 2 3)
        (common:prog1 (car x) (rplaca x y)) -> a
        x -> ((1 2 3) b c)
        y -> (1 2 3)")


(define
 "prog2"
 (cl-macro prog2)
 :documentation
 "形式 : prog2 form1 form2 &rest forms 
form1 form2 ... を順に評価し、form2 の評価値を返す。
(prog1 form1 form2 ...) = (progi form1 ^form2 ...)"
 :example
 "(prog1 1 2 3 4 5) -> 2
        (!x '(1 2 3)) -> (1 2 3)
        (prog1 (car x) (!x (cdr x))) -> (2 3)
        x -> (2 3)")


(define
 "common:prog2"
 (cl-macro prog2)
 :documentation
 "形式 : common:prog2 form1 form2 &rest forms
form1 form2 ... を逐次評価し、form2 の値を返す。"
 :example
 "x1 = (1 2 3), x2 = (4 5 6), x3 = (7 8 9)
        ならば
        (common:prog2 (cons x1 x2)
        	      (cons x2 x3)
         	      (cons x3 x1)) -> ((4 5 6) 7 8 9)")


(defmacro trans-progi-if-toga (toga-form cache)
  (if (tao:togap toga-form)
      `(setq ,cache ,toga-form)
      toga-form))


(define
 "progi"
 (macro (&body body)
     (let ((progi-id (and (symbolp (car body))
                          (not (boundp (car body)))
                          (pop body)))
           (cache (gensym "TOGA-CACHE-")))
       `(block ,progi-id
          (let (,cache)
            ,@(mapcar (lambda (x) (if (tao:togap x) `(setq ,cache ,x) x))
                      (butlast body))
            (if (or ,@(and (tao:togap (last body)) (list t))
                    (null ,cache))
                (setq ,cache ,@(last body))
                ,@(last body))
            ,cache))))
 :documentation
 "形式 : progi &rest progi-id form1 form2 ...
form1 form2 ... を順に評価し、最後の トーガ (^) を持つ式の値を返す。
トーガを持つ式がないときは最後の式の値を返す。
form1 form2 ... はリストかアトム。
progi-id は、関数 exit-progi による脱出のためのマーク。"
 :example
 "(progi 1 2 3 ^4 5 6 ^7 8 9) -> 7
        (progi 1 2 3 4 5 6 7 8 9) -> 9
        (progi ^(car x) (!x (cdr x))) = (pop x)")


(define
 "progn"
 (macro (&body body)
     `(cl:progn ,@body))
 :documentation
 "形式 : progn &rest form1 form2 ...
form1 form2 ... を逐次評価し、その最後の値を返す。
(progn form1 form2 ...) = (seq form1 form2 ...)"
 :example
 "(defvar *count* 0)
        (de count-cons (x y)
            (progn (!!+ !*count*)
        	   (cons x y))) -> count-cons
        (count-cons 'a 'b) -> (a . b)
        *count* -> 1")


(define
 "progv"
 (cl-macro progv)
 :documentation
 "形式 : progv '(var1 var2 ...) '(value1 value2 ...)
                                  &rest form1 form2 ...
var1 を value1 に、var2 を value2 に ... と束縛し、form1 form2 ... を
評価し、最後のフォームの評価結果を返す。
var1, var2, ... はグローバル変数。ローカル変数スコープに影響を与えない。
この関数が、あるローカル変数のスコープ内であるなら、そのローカル変数を
この関数内で参照できる。関数 progv は普通 関数 progv の環境内の変数と
は異なったグローバル変数を使いたい時に使用。"
 :example
 "(!x 12) -> 12
        (progv '(x) '(10) (+ x 20)) -> 30
        x -> 12
        (prog (p) (!p 100)
        	  (progv '(p) '(2)
        		 (write p) (symbol-value p)))
        	  (write p)) -> 100
        100 (ローカル変数)、2 (グローバル変数)、100 の順に逐次プリント。")


(define
 "protect-file"
 (expr nil)
 :documentation
 "形式 : protect-file file code
code によって決められた形式でファイル file をプロテクトする。

   3 つのプロテクションレベル
第 1 のレベル
  プロテクトされたとき、オペレータのように特権を持つユーザ以外はすべて
のユーザにプロテクトされる。しかし、プロテクトされなかったときでも
ファイルのオーナー以外のすべてのユーザに対してもプロテクトされる。
第 2 のレベル
  プロテクトされたとき、オーナー以外のすべてのユーザにプロテクトされる。
しかし、プロテクトされなかったときでもファイルのオーナーのグループ
以外のすべてのユーザに対してもプロテクトされる。
第 3 のレベル
  プロテクトされたとき、グループ以外のすべてのユーザにプロテクトされる。
しかし、プロテクトされなかったときは、ユーザに対してプロテクトされない。
注意することは、ファイルがプロテクトされてもされなくても、特権を持つ
ユーザは、すべてのファイルにアクセスすることができる、ということ。

   3 つのプロテクションスイッチ
第 1 のスイッチ
  ユーザがプロテクトされたファイルを実行できるかどうかを決定する。
このモードを \"x\" とする。
第 2 のスイッチ
  ユーザがプロテクトされたファイルを書いたり変更することができるか
どうかを決定する。このモードを \"w\" とする。
第 3 のスイッチ
  ユーザがプロテクトされたファイルを読むことができるかどうかを決定する。
このモードを \"r\" とする。

\"rw\" モードでは、ファイルを読んだり、変更することはできない。
\"rwx\" モードでは、ファイルを読んだり、書いたり、実行したりすることは
できない。

code において、3 つのスイッチは、3 つのレベルに対して適用される。
例えば、code が、\"---rw-rwx\" とする。これは、オーナーが、読んだり、
書いたり、実行することができることを意味する。
グループのメンバーは、ファイルを実行できるが、読んだり、書いたりする
ことはできないし、それ以外のユーザは、このファイルになにもすることは
できない。\"-\" はファイルを扱う宣言を意味する。
code のレベル順序は、\"read\" (r) , \"write\" (w) , \"excute\" (x) 。
code での初めの \"-\" は、省略可能。\"rw-rwx\" は、\"---rw-rwx\" と同じこと。"
 :example
 "(protect-file \"michadame.yo\" \"rwxrwxrwx\") -> ok
        特権を持つユーザ以外のすべてのユーザに対してプロテクトされる。
        (protect-file \"mitemoiiwa.yo\" \"rwx\") -> ok
        グループ以外のすべてのユーザに対してプロテクトされる。
        (protect-file \"runbanned.des\" \"--x--x--x\") -> ok
        特権を持つユーザ以外のユーザによって実行されない。")


(define
 "provide"
 #'provide
 :documentation
 "形式 : provide name
モジュールの開始を指定する。name を *module* 変数 (リスト形式) に
追加登録し、そのモジュールがロードされていることを表し、その
モジュール名を返す。"
 :example
 "(provide 'maclisp) -> (\"maclisp\")")


(define
 "psetf"
 (cl-macro psetf)
 :documentation
 "形式 : psetf &rest var1 value1 var2 value2 ...
並列代入を行う。全ての valuei を評価して、それらの値の vari への代入
の全てを平行して行う。"
 :example
 "(psetf a 1 b 2 c 3) -> nil
        a = 1  b = 2  c = 3
        (psetf a (+ b c) b (+ c a) c (+ a b)) - nil
        a = 5  b = 4  c = 3
        (setf x 2) -> 2
        (psetf x 1 y (1+ x)) -> nil
        x -> 1
        y -> 2")


(define
 "psetq"
 (cl-macro psetq)
 :documentation
 "形式 : psetq &rest var1 value1 var2 value2 ...
並列代入を行い、nil を返す。value1 value2 ... を先に評価して、
それらの値を var1 var2 ... に代入する。"
 :example
 "x = 1, y = 2 , z =3
        (psetq x (+ y z) y (+ z x) z (+ x y)) -> t
        x = 5, y = 4, z = 3")


(define
 "pure-jstring-p"
 (expr nil)
 :documentation
 "形式 : pure-jstring-p string
string 内の全ての文字が jcharacter 、すなわち 16 ビット文字なら、
それを返し、それ以外なら nil を返す。"
 :example
 "(pure-jstring-p \"ＮＴＴ電気通信研究所\") ->
                        \"ＮＴＴ電気通信研究所\"
        (pure-jstring-p \"ＮＴＴNTT電気通信研究所\") -> nil")


(define
 "purge"
 (expr nil)
 :documentation
 "形式 : purge stream
stream が入力ファイルに結合されているなら、関数 close と同じ。
stream が出力ファイルに結合されているなら、そのファイルがオープンされ
なかったかのように、ディレクトリ内容をアップデイトすることなくクローズ
する。"
 :example
 "(!aa (open \"tss.tao\" :direction :output :if-does-not-exist 
        	:create))  -> {udo}1819363file-stream
        (write \"aaaaa\" aa) -> \"aaaaa\"
        (purge aa) -> ok
          ファイル \"tss.tao\" には \"aaaaa\" は更新されない。")


(define
 "purge-spies"
 (expr nil)
 :documentation
 "形式 : purge-spies &opt terno
ターミナル terno のスパイをとりやめる。
terno の既定値は login しているターミナル。"
 :example
 "")


(define
 "purge-spy"
 (expr nil)
 :documentation
 "形式 : purge-spy remove-terno &opt terno
ターミナル remove-terno からターミナル terno のスパイをとりやめる。
terno の既定値は login しているターミナル。"
 :example
 "")


(define
 "push"
 (cl-macro push)
 :documentation
 "形式 : push list1 list2
list1 と list2 をコンスし、そのコンスしたリストを list2 の新しい値とし
返す。list1 は、任意の Lisp オブジェクトでよい。
list2 は、リストを含む汎変数の名前。
(push x y) = (!!cons x !y) = (!y (cons x y))"
 :example
 "x = (1 2 3)  で、 y = (4 5 6 7) の場合
        (push x y) -> ((1 2 3) 4 5 6 7) となり
 	x = (1 2 3)  で、 y = ((1 2 3) 4 5 6 7) となる。")


(define
 "pushnew"
 (cl-macro pushnew)
 :documentation
 "形式 : pushnew item place &key :test :test-not :key
item は、任意のオブジェクトを参照できる。place は、リストを含む汎変数
名。item がその place のリストメンバーでなければ、item を place の先
頭にコンスする。そして、そのリストを、place の新しい値とし、返す。
そうでなければ、変更されていないリストを返す。"
 :example
 "x = '(a (b c) d)
        (pushnew 5 (cadr x)) -> (5 b c)
        (pushnew 'b (cadr x)) -> (5 b c)")


(define
 "put-alist"
 (subr (item value a-list &key (test #'cl:eql))
   (let* ((a-list (copy-alist a-list))
          (pair (rassoc value a-list :test test)))
     (if pair
         (setf (car pair) item)
         (push (cons item value) a-list))
     a-list))
 :documentation
 "形式 : put-alist item value a-list
連想リスト a-list 内で、第 2 要素が item と eq な要素対があれば、
item をその要素対の第 1 要素に代入する。ない場合は、item と value の
要素対 (item . value) を a-list に追加する。代入形式で使用。"
 :example
 "alist = (2 . 1)
        (!alst (put-alist 3 4 alst)) -> ((3 . 4) (2 . 1))  ここで
        (!alst (put-alist 'qwe 4 alst)) -> ((qwe . 4) (2 . 1))")


(define
 "put-comma"
 (expr nil)
 :documentation
 "形式 : put-comma object &optn y
object の先頭にカンマ (,) を付けて返す。"
 :example
 "(put-comma 'a) -> ,a
        (put-comma (list 'a 'b 'c)) -> ,(a b c)")


(define
 "put-toga"
 (expr (object)
   (list 'taoi::toga object))
 :documentation
 "形式 : put-toga object
object に ^ (toga) を付けた値を返す。"
 :example
 "(put-toga (list 1 2 3)) -> ^(1 2 3)")


(define
 "putplist"
 (subr (p-list val ind)
   (setf (getf p-list ind) val)
   p-list)
 :documentation
 "形式 : putplist p-list val ind
属性リスト p-list で、ind の値と eq な最初のデータの値をval の値に置
き換える。見つからなかった場合は、p-list に ind と val の値のペアを
コンスする。代入の形式で用いる。連想リストのかわりに属性リストが扱わ
れる点を除いて、putalist 関数と同じ。"
 :example
 "(!(plist 'xxx) '(a 1 b 2 c 3 d 4)) -> (a 1 b 2 c 3 d 4)
        (!yyy (plist 'xxx)) -> (a 1 b 2 c 3 d 4)
        (putplist yyy 5 'e) -> (e 5 a 1 b 2 c 3 d 4)
        yyy -> (a 1 b 2 c 3 d 4)
        (!yyy (putplist yyy 5 'e)) -> (e 5 a 1 b 2 c 3 d 4)
        yyy -> (e 5 a 1 b 2 c 3 d 4)
        (putplist yyy 6 'c)  -> (e 5 a 1 b 2 c 6 d 4)
        yyy -> (e 5 a 1 b 2 c 6 d 4)")


(define
 "putprop"
 (subr (object val ind)
   (setf (get object ind) val)
   (get object ind))
 :documentation
 "形式 : putprop object val ind
識別子又はベクタ object が指定する属性リストの ind の値と eq な最初の
インディケータに対応する属性値を、val の値で置き換え、属性リストに代
入した後、その値を返す。一方、見つからなかった場合は、属性リストに 
ind とval の値のペアを consし、その値を返す。いつも属性リストを変更す
る(破壊的)。連想リストのかわりに属性リストが扱われる点を除いて、
putalist 関数と同じ。"
 :example
 "(!(plist 'xxx) '(q 2 r 3 s 4)) -> (q 2 r 3 s 4)
        xxx -> (q 2 r 3 s 4)
        (!yyy (plist 'xxx)) -> (q 2 r 3 s 4)
        (putprop 'xxx 1 'p) -> 1
        (plist 'xxx) -> (p 1 q 2 r 3 s 4)
        yyy -> (q 2 r 3 s 4)
        (putprop 'xxx 5 's) -> 5
        (plist 'xxx) -> (p 1 q 2 r 3 s 5)
        yyy -> (q 2 r 3 s 5)")


;;; *EOF*
