(tao:common-lisp)


(in-package #:tao-internal)


(define
 "udo"
 (class T)
 :documentation
 "インスタンスは、ユーザ定義のオブジェクト。udo は \"user defined 
object\" の意味。make-instance で作られたオブジェクトはクラス udo の
インスタンス。"
 :example
 "")


(define
 "udo-equal"
 (subr nil)
 :documentation
 "形式 : udo-equal object1 object2
object1 と object2 がベクタのとき、その両方が udo でかつ equal ならば 
t を返し、そうでなければ nil を返す。"
 :example
 "(defclass abc () ((a 1) (b 2)) () :gettable :settable) -> abc
        (!def1 (make-instance 'abc)) -> {udo}66591abc
        (!def2 (make-instance 'abc)) -> {udo}74027abc
        (udo-equal def1 def2) -> t
        [def2 set-b 3] -> 3
        (udo-equal def1 def2) -> nil")


(define
 "udop"
 (subr nil)
 :documentation
 "形式 : udop object &opt class
object が class の udo ならば class を返し、それ以外なら nil を返す。
class の指定がない場合、object が udo ならばクラス名を表すシンボルを
返し、それ以外なら nil を返す。"
 :example
 "(udop 12) -> nil
        (defclass abc () ((a 1) (b 2)) () :gettable :settable) -> abc
        (defclass ghi () ()) -> ghi
        (!def (make-instance 'abc)) -> {udo}66591abc
        (udop def) -> abc
        (udop def 'abc) -> abc
        (udop def 'ghi) -> nil")


(define
 "ueq"
 (subr nil)
 :documentation
 "形式 : ueq object1 object2
object1 と object2 に対して、潜在ポインタを削除しないで eq を行う。"
 :example
 "")


(define
 "undeclare-semi-globals"
 (expr nil)
 :documentation
 "形式 : undeclare-semi-globals &rest var1 var2 ... varN
var1 var2 ... varN がセミグローバル変数ではないことを宣言する。
つまり、セミグローバル変数テーブル中から、それらの名前を削除する。"
 :example
 "(semi-globals a b c) -> 
        	(+ - * / ** ++ // *** *prompt-function*
        	 *history-command-over it that +++ ///
        	 *screen-out-file* *ansi$caution-type* c b a)
        (symbol-value 'a) -> nil
        (undeclare-semi-globals a b c) ->
        	(+ - * / ** ++ // *** *prompt-function*
        	*history-command-over it that +++ ///
        	*screen-out-file* *ansi$caution-type*)
        (symbol-value 'a) -> (unbound-variable a)")


(define
 "undef"
 (subr ()
  (tao.logic::_))
 :documentation
 "新しい未定義のデータを返す。"
 :example
 "(undef) -> {undef}654
        (undef) -> {undef}655")


(define
 "undefclass"
 (expr nil)
 :documentation
 "形式 : undefclass class
class を削除する。"
 :example
 "(defclass itazra () () ()) -> itazra
        (undefclass 'itazra) -> nil")


(define
 "undeflogic-method"
 (macro ((class message) (&rest args))
     (let ((gf (gensym "gf"))
           (md (gensym "md")))
       `(let* ((,gf #',(tao.logic::make-predicate message (1+ (length args))))
               (,md (find-method ,gf
                                 nil
                                 (list (find-class ',class)
                                       ,@(mapcar (constantly `(find-class T))
                                                 (cons 'cont args)))
                                 nil)))
          (when ,md (remove-method ,gf ,md))
          ',message)))
 :documentation
 "形式 : undeflogic-method method-spec &opt arg-pattern-list
既に定義された論理メソッドを消去する。
method-spec = (class-name method-type selector)
arg-pattern-list はヘッド部分に対応する。これが省略されると、selector
という名前を持つすべての論理メソッドが消去される。"
 :example
 "(undeflogic-method (aclass amethod) (_x _y)) -> t
        (deflogic-method (aclass amethod) (_x _y) (cons _x _y))
        が消去される。
        (undeflogic-method (aclass amethod)) -> t
        非メソッドの名前を持つ全ての論理メソッドが消去される。
        (undeflogic-method (aclass amethod) (_x _y)) -> nil")


(define
 "undefmethod"
 (macro nil)
 :documentation
 "形式 : undefmethod method-spec
メソッド仕様 method-spec のメソッドの定義を削除する。
メソッド仕様については defmethod 参照。"
 :example
 "(defclass a1 () (b1) () :gettable :settable) -> a1
        (defmethod (a1 mult) () (!!* !b1 20)) -> mult
        (!cc (make-instance 'a1 b1 10)) -> {udo}44594a1
        [cc b1] -> 10
        [cc mult] -> 200
        (undefmethod (a1 mult)) -> mult
        [cc mult]   エラーを警告する")


(define
 "undefp"
 (subr (obj)
   (tao.logic::unbound-var-p obj))
 :documentation
 "形式 : undefp data
data が未定義なら data を返し、それ以外なら nil を返す。"
 :example
 "(undefp _x) -> {undef}1235 if _x is uninstantiated.")


(define
 "undelete-file"
 (expr nil)
 :documentation
 "形式 : undelete-file file
削除済ファイル (またはファイル群) file を復元する。但し、
expunge-files 後に undelete-file を行っても復元しない。
delete-file 参照。"
 :example
 "(undelete-file \"qwe.tao\") -> (\"cs:<dire>qwe.tao.1\")
        (undelete-file \"foo.tao.*\")
        カレントディレクトリ中のファイル foo.tao の全バージョンを復元
        (undelete-file \"*.tao\")
        拡張子 が \"tao\" のファイルの全バージョンを復元")


(define
 "unexport"
 #'unexport
 :documentation
 "形式 : unexport symbol &opt package
symbol を package 中の内部シンボルとする。t を返す。"
 :example
 "(do-external-symbols (i (find-package \"window\")) (print i)) ->
        	print-more
        	make-scloll-menu-with-label
                ...
        (unexport 'window:print-more (find-packge \"window\")) -> t
        (do-external-symbols (i (find-package \"window\")) -> 
        	make-scloll-menu-with-label
                ...")


(define
 "union"
 #'union
 :documentation
 "形式 : union list1 list2 &key :test :test-not :key
list1 とlist2 の論理和をとり、その結果を返す。2 つのリストに重複して
含まれる要素があれば、1 つだけが結果に含まれる。nunion 参照。"
 :example
 "(union  '(a b) '(a c))  ->  (a b c)
        (union  '(a b) '())  ->  nil")


(define
 "unionq"
 (subr (list1 &rest listn)
   (remove-duplicates
    (reduce (lambda (res x) (union x res :test #'eq))
            listn :initial-value list1) :test #'eq))
 :documentation
 "形式 : unionq list1 &rest list2 ... listN
list1 list2 ... listN を集合と見なして、その和集合をリストで返す。
和集合の中で重複した要素は、それが eq であれば一方は削除される。
和集合での要素の並び順序と list1 list2 ... listN での要素の並び順序は
必ずしも一致しない。"
 :example
 "(unionq '(a b c d a d s w a f)) -> (b c d s w f a)
        (unionq (index 1 4) (list -3 2 3 5)) -> (1 2 3 4 5 -3)")


(define
 "unit-clauses"
 (not-implemented "unit-clauses" nil)
 :documentation
 "形式 : unit-clauses &rest 'x
ユニット節だけを定義するということを除いては、hclauses と同じ。
ユニット節は、ボディのない節である。assert により内部的に使われている。
ユーザが、同じ主ファンクタで複数のユニット節を言明すると、assert は、
自動的に unit-clauses の代わりに hclauses を用いる。シンボル A1' と
シンボル A1 の違いについては、 &+ を参照。"
 :example
 "(assertz (city tokyo))
        (assertz (city nagoya))
        (assertz (city osaka)) は、
        (definition city (unit-clauses (&+ (tokyo)) (&+ (nagoya))
                                       (&+ (osaka))) と同じ。
        assert が assertz の代わりに使われると、引数つまりユニット節は、
        tokyo, nagoya, osaka をアルファベット順にソートしたものとなる。")


(define
 "unitern"
 (expr nil)
 :documentation
 "形式 : unintern symbol &opt package
package から symbol を取り除けたら t を返し、そうでなければ nil を返す。"
 :example
 "(!y 1) -> 1
        (unintern 'y) -> t
        (find-symbol \"y\") -> !(() nil)
        y -> エラー")


(define
 "|sys:univ-package|"
 (constant (find-package 'tao))
 :documentation
 "パッケージ \"univ\" へのポインタ。 \"univ\" は、根パッケージであり、
すべてのグローバルパッケージ、\"bas\" \"key\" \"tao\" \"common\" などはこの
パッケージのサブパッケージ。"
 :example
 "sys:univ-package -> {vector}32221(package . 12)")


(define
 "unless"
 (cl-macro unless)
 :documentation
 "形式 : unless pred &rest body
pred を評価し、値が nil の場合は nil を返し、nil 以外の場合は、body 
のフォームを左から右に逐次評価し、最後のフォームの評価結果を返す。
(unless p a b c) = (cond ((not p) s b c))
        	 = (if p nil (progn a b c))
        	 = (when (not p) a b c)"
 :example
 "")


(define
 "unprotect-file"
 (expr nil)
 :documentation
 "形式 : unprotect-file file code
code によって決定された方法で、file のプロテクションを解除する。code 
が、\"rwxrwxrwx\" なら、すべてのレベルのプロテクションが解除される。
protect-file 参照。"
 :example
 "")


(define
 "unquote"
 (subr (list) ;(macro nil)
   (and (eq 'quote (car list))
        (cadr list)))
 :documentation
 "形式 : unquote list
list の car部を返す。"
 :example
 "(!x ''a) -> 'a で x = 'a ならば
        (unquote x) は a を返す。
        Common Lisp の場合は
        (cadr x) -> a
        (unquote 'a) はエラー")


(define
 "unread-char"
 #'unread-char
 :documentation
 "形式 : unread-char char &opt stream
stream のバッファのヘッドに char を書き込み、nil を返す。char は、
stream から最後に読まれた文字と同じ文字でなければならない。それ故、
unread-char は、通常、read-char の実行後に使用する。stream が省略され
ると、*standard-input* の値 (通常コンソールターミナル) が指定されたも
のと見なす。先読みのために用いられる。"
 :example
 "ファイル \"asd.tao\" が  qwe..... とする。
        (!aa (open \"asd.tao\")) -> {udo}1275908file-stream
        (read-char aa) -> \"q\"
        (read-char aa) -> \"w\"
        (unread-char \"w\" aa) -> 119
        (read-char aa) -> \"w\"
        (read-char aa) -> \"e\"")


(define
 "unsigned-integer-locative"
 #-lispworks
 (exprdyn nil)
 #+lispworks
 (exprdyn (&rest vars)
   `(progn
      ,@(mapcar (lambda (v)
                  `(setf ,v (fli:allocate-foreign-object :type :uint64)))
                vars)
      ',vars))
 :documentation
 "形式 : unsigned-integer-locatives &rest var1 var2 ... varN
N 個の 64 ビット符号なし整数ロカティブを生成し、それらを対応する各々の
変数に代入する。リスト (var1 var2 ... varN) を返す。初期設定は行わない。"
 :example
 "(unsigned-integer-locatives d e f g h) -> (d e f g h)
        d -> #162310
        e -> #162311")


(define
 "unsigned-integer-locative-arrays"
 (macro nil)
 :documentation
 "形式 : unsigned-integer-locative-arrays &rest array-spec
要素のデータ型が符号なし整数ロカティブである配列を生成し、array-spec 
で指定された値をリストにして返す。array-spec は、var dimension という
形式。ここで、var は生成される配列の名前であり、dimension は配列の
次元を指定する整数のリスト。"
 :example
 "(signed-integer-locative-arrays (b1 10)(b2 (-5 4) 47))
        -> ((b1 10) (b2 (-5 4) 47))
        2 つの配列 b1 b2 が生成
        b1 は、ランクが 1 で、次元が 0 から 9 である配列
        b2 は、ランクが 2 で、第 1 次元が -5 から 4 、第 2 次元が
        0 から 46 の配列
        ((b1 5) <- #123) -> #123
        (b1 5) -> #123
        ((b2 -3 39) <- #456) -> #456
        (b2 -3 39) -> #456")


(define
 "untrace"
 (cl-macro untrace)
 :documentation
 "形式 : untrace &rest func1 func2 ... funcN
引数なしに呼び出されると、全ての関数について現在のトレースが中止される。
関数 func1 func2 ... funcN が指定されると、指定された関数についてだけ
現在のトレースが中止される。"
 :example
 "(defun fact (x)
               (cond ((= x 0) 1)
        	     (t (* x (fact (1- x)))))) -> fact
        (defun abc () (car '(a b c))) -> abc
        (trace fact abc) -> (fact abc)
        (fact 3) -> 1 <IN > (fact [x = 3])
        	    .2 <IN > (fact [x = 2])
        	    . 3 <IN > (fact [x = 1])
        	    . .4 <IN > (fact [x = 0])
        	    . .4 <OUT> (fact 1)
        	    . 3 <OUT> (fact 1)
           	    .2 <OUT> (fact 2)
        	    1 <OUT> (fact 6)
        	    6
        (abc) -> 1 <IN > (abc)
        	 1 <OUT> (abc a)
        	 a
        (untrace fact) -> (fact)  (fact のみトレースの中止)
        (fact 3) -> 6")


(define
 "untrail"
 (subr (&rest vars)
   (dolist (v vars)
     (setf (tao.logic::var-binding v)
           tao.logic::unbound)))
 :documentation
 "形式 : untrail _x1 _x2 ...  _xn
24 ビットの世代番号で _x1 _x2 ... _xn を \"undefind\" 状態に戻す。異なる
名前を持つ変数は、\"undefinded\" 状態に関する異なる世代番号と関連付けられ
る。ただし (!_X undef) は tagundef をセットするが、世代番号のフィールド
はゼロにセットされる 。"
 :example
 "")


(define
 "untyi"
 (subr nil)
 :documentation
 "形式 : untyi code &opt stream
stream のバッファのヘッドに、code で指定された ASCII コードに対応する
文字を書き込み、code を返す。stream が省略されると、*standard-input* 
の値 (通常コンソールターミナル) が指定されたものと見なす。code は
stream から最新に読まれたコードと同じでなければならない。それ故、untyi
は、通常、tyi の実行後に使用される。先読みのために用いられる。"
 :example
 "ファイル \"asd.tao\" が  qwe..... とする。
        (!aa (open \"asd.tao\")) -> {udo}1276197file-stream
        (tyi aa) -> 113  (\"q\")
        (tyi aa) -> 119  (\"w\")
        (untyi 119 aa) -> 119  (\"w\")
        (tyi aa) -> 119  (\"w\")
        (tyi aa) -> 101  (\"e\")")


(define
 "unuse"
 (expr nil)
 :documentation
 "形式 : unuse package
package の中の全てのシンボルをカレントパッケージで使えないようにする。
package はパッケージ、パッケージ名、パッケージのリスト、パッケージ名の
リストのどれでもよい。
(unuse pkg) = (unuse-package pkg)"
 :example
 "(package-name (package-use-list)) -> (\"step\" \"net\" \"apropos\")
        (unuse \"net\") -> t
        (unuse \"apropos\") -> t
        (package-name (package-use-list)) -> nil")


(define
 "unuse-package"
 #'unuse-package
 :documentation
 "形式 : unuse-package package
package にある全てのシンボルをカレントパッケージで使えないようにする。
package はパッケージ、はパッケージ名、パッケージのリスト、パッケージ名
のリストのいずれでもよい。"
 :example
 "(package-name (package-use-list)) -> (\"step\" \"net\" \"apropos\")
        (unuse-package \"net\") -> t
        (unuse-package \"apropos\") -> t
        (package-name (package-use-list)) -> nil")


(define
 "unwind-protect"
 #'unwind-protect
 :documentation
 "形式 : unwind-protect form1 &rest form2 ... formN
form1 を評価し、その評価結果を返す。評価が正常に終わろうと、非局所的な
脱出（throw, exit, exit-for 等による）により評価が中断されようと、実行
を終える時には必ず form2 ... formN を全て評価する。"
 :example
 "(unwind-protect
              (seq (change-a-very-important-state-temporarily)
                   (do-some-complicated-work ...) )
              (reset-the-state) )")


(define
 "upper-case-p"
 #'upper-case-p
 :documentation
 "形式 : upper-case-p char
char の値が大文字であれば t を返し、それ以外の場合 nilを返す。
standard-char-p で定義された標準文字の場合、A から Z までが大文字。"
 :example
 "(upper-case-p #\\a) -> nil
        (upper-case-p #\\A) -> \"A\"")


(define
 "use"
 (expr nil)
 :documentation
 "形式 : use packge
package の中の全てのシンボルをカレントパッケージで使用可能にする。
package はパッケージ、パッケージ名、パッケージのリスト、パッケージ名の
リストのどれでもよい。
(use pkg) = (use-package pkg)"
 :example
 "(packge-name (package-use-list)) -> (\"step\")
        (use \"net\") -> t
        (use \"apropos\") -> t
        (package-name (package-use-list)) -> (\"step\" \"net\" \"apropos\")")


(define
 "use-package"
 #'use-package
 :documentation
 "形式 : use-package package
package の中の全てのシンボルをカレントパッケージで使用可能にする。
package はパッケージ、パッケージ名、パッケージのリスト、パッケージ名の
リストのどれでもよい。"
 :example
 "(package-name (package-use-list)) -> (\"step\")
        (use-package \"net\") -> t
        (use-package \"apropos\") -> t
        (package-name (package-use-list)) -> (\"step\" \"net\" \"apropos\").")


(define
 "user"
 (class T)
 :documentation "TAO では全てのユーザはクラス user のインスタンスとして登録される。"
 :example
 "")


(define
 "user-homedir-pathname"
 #'user-homedir-pathname
 :documentation
 "形式 : user-homedir-pathname &opt host
host 上での、ユーザのホームディレクトリのパス名を返す。host は、
ホスト名の文字列をとる。"
 :example
 "(!a (user-homedir-pathname \"Cobalt\")) -> nil
        (!b (user-homedir-pathname \"Co\")) -> {udo}1775492pathname
        (namestring b) -> \"Co::bs:<dire>.\"")


;;; *EOF*
