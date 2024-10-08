(tao:common-lisp)


(in-package #:tao-internal)


(deftype shortnum ()
  `(integer ,(expt -2 23) ,(1- (expt 2 23))))


(define
 "i/="
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (/= shortnum1 shortnum2))
 :documentation
 "形式 : i/= shortnum1 shortnum2
shortnum1 と shortnum2 を比較し、一致しなければ shortnum2 を返し、
そうでなければ nil を返す。
2 つの引数がともに shortnum という点を除けば /= と同じ。
i/= の方が /= より動作が多少速い。"
 :example
 "(i/= 1 2) -> 2
        (i/= 2 2) -> nil
        (3 i/= 4) -> エラー
        (1 i/= 2 i/= 3 i/= 4) -> エラー
        (i/= -2 1) -> 1")


(define
 "i<"
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (< shortnum1 shortnum2))
 :documentation
 "形式 : i< shortnum1 shortnum2
shortnum1 が shortnum2 より小さいなら、shortnum2 の評価値を返し、
そうでなければ nil を返す。 引数が 1 つのときは、その評価値を返す。
infix notation では、任意個の引数が指定可能。
2 つの引数がともに shortnum という点を除けば < と同じ。
i< の方が < より動作が多少速い。"
 :example
 "(i< 4 5) -> 5
        (i< 5 4) -> nil
        (i< 0) -> エラー
        (i< 3 3) -> nil")


(define
 "i<="
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (<= shortnum1 shortnum2))
 :documentation
 "形式 : i<= shortnum1 shortnum2
shortnum1 が shortnum2 より小さいか等しいなら、shortnum2 の評価値を、
そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
infix notation では、任意個の引数が指定可能。
2 つの引数がともに shortnum という点以外は <= と同じ。
i<= の方が <= より動作が多少速い。"
 :example
 "(i<= 4 5) -> 5
        (i<= 5 4) -> nil
        (i<= 0) -> エラー
        (i<= -3 1) -> 1
        (i<= 3 3) -> 3")


(define
 "i="
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (= shortnum1 shortnum2))
 :documentation
 "形式 : i= shortnum1 shortnum2
shortnum1 と shortnum2 を比較し、等しいなら、shortnum2 の評価値を返し、
そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
infix notation では任意個の引数が指定可能。
2 つの引数がともに shortnum という点を除けば = と同じ。
i= の方が = より動作が多少速い。"
 :example
 "(i= 4 4) -> 4 (not t!)
        (i= 3 4) -> nil
        (i= 4) -> エラー
        (i= \"string\" \"string\") -> エラー
        (i= nil nil) -> エラー
        (i= 1 -1) -> nil")


(define
 "i>"
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (> shortnum1 shortnum2))
 :documentation
 "形式 : i> shortnum1 shortnum2
shortnum1 が shortnum2 より大きいなら、shortnum2 の評価値を返し、
そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
infix notation では任意個の引数が指定可能。
2 つの引数がともに shortnum という点を除けば > と同じ。
i> の方が > より動作が多少速い。"
 :example
 "(i> 5 4) -> 4
        (i> 4 5) -> nil
        (i> 5) -> エラー
        (1> 3 3) -> nil")


(define
 "i>="
 (subr (shortnum1 shortnum2)
   (declare (type shortnum shortnum1 shortnum2)
            (optimize speed (safety 0)))
   (>= shortnum1 shortnum2))
 :documentation
 "形式 : i>= shortnum1 shortnum2
shortnum1 が shortnum2 より大きいか等しいなら、shortnum2 の評価値を返し、
そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
2 つの引数がともに shortnum という点以外は >= と同じ。
i>= の方が >= より動作が多少速い。"
 :example
 "(i>= 5 4) -> 4
        (i>= 4 5) -> nil
        (i>= 5) -> エラー
        (i>= 3 3) -> 3")


(define
 "id"
 (class symbol)
 :documentation
 "インスタンスは abc, aho, koke のような識別子。"
 :example "")


(define
 "identity"
 #'identity
 :documentation
 "形式 : identity object
object をそのまま返す。
引数として関数を要求する関数の引数を指定するときに便利。"
 :example
 "(de sum-up (f n &aux (sum 0))
            (dotimes (i n sum)
                 (isum (+ (funcall f i) sum)))) -> sum-up
        (sum-up 'square 10) -> 285
        (sum-up '1+ 10) -> 55
        (sum-up 'identity 10000) -> 49995000")


(define
 "idp"
 (subr nil)
 :documentation
 "形式 : idp arg
arg が識別子なら、評価値を返し、それ以外なら nil を返す。"
 :example
 "(idp 'asdf) -> asdf
        (idp 3) -> nil
        (idp '_x) -> nil
        (idp #!expr) -> nil")


(define
 "if"
 (macro (pred then &optional else)
     `(cl:if ,pred ,then ,else))
 :documentation
 "形式 : if pred then &opt else
pred を評価し、その結果が nil でなければ then を評価し、nil ならば 
else を評価する。 else の指定がない時は nil となる。
 (if pred then else) = (cond (pred then) (t else))"
 :example
 "(!a 5)→ 5
        (if (oddp a) 'kisuu 'guusuu)→ kisuu
        (if (listp \"a b c\") 'list)→ nil")


(define
 "ifundef"
 (subr (object1 object2)
   "ifundef                                関数[#!subr]

<説明>
  形式 : ifundef object1 object2
object1 が未定義なら object2 の評価値を返し、その他の場合は object1 の
評価値を返す。

<例>
        (de f (&opt x)
            (!x (ifundef x 1)) ; x の既定値は 1
             ... )"
   (if (tao:undefp object1)
       object2
       object1))
 :documentation
 "形式 : ifundef object1 object2
object1 が未定義なら object2 の評価値を返し、その他の場合は object1 の
評価値を返す。"
 :example
 "(de f (&opt x)
            (!x (ifundef x 1)) ; x の既定値は 1
             ... )")


(define
 "ignore"
 (subr (&rest x)
   (declare (cl:ignore x))
   nil)
 :documentation
 "形式 : ignore &rest 'x
引数を評価し、常に nil を返す。ダミーの関数として有用。"
 :example
 "(defun f00 (x y) 
            (ignore y) (sin x))")

(define
 "image"
 (macro (var list &body forms)
     (cl:let ((tag (gensym "image-")))
       `(block ,tag
          (macrolet ((tao::exit-image (val)
                       `(return-from ,',tag ,val)))
            (let (result)
              (dolist (,var ,list (nreverse result))
                (push (progn ,@forms) result)))))))
 :documentation
 "形式 : image var list form1 form2 ... formN
list の第 1 要素を変数 var に代入して form1 form2 ... を順に評価する。
次に list の第 2 要素を var に代入して form1 form2 ... を評価する。
これを継続し、 list の最後の要素を var に代入して form1 form2 ... を
評価したところで終了する。そして最後の formN の評価結果を並べて新しい
リストを作り、それを返す。返されるリストの長さは list と同じ長さ。"
 :example
 "(image i (index 1 10) (i * i)) -> 
        	(1 4 9 16 25 36 49 64 81 100)
        (image i (index 1 5) (!j (1+ i)) (i * j)) -> (2 6 12 20 30)
        (image i '(0 2 4 6 8) (!j (i * i)) (j - i)) -> (0 2 12 30 56)")


(define
 "image-can"
 (macro (var list &body forms)
     "image-can                              関数[#!macro]

<説明>
  形式 : image-can var list form1 form2 ... formN
list の第 1 要素を変数 var に代入して form1 form2 ... を順に評価する。
次に list の第 2 要素を var に代入して form1 form2 ... を評価する。
これを継続し、list の最後の要素を var に代入して form1 form2 ... を
評価したところで終了する。そして最後の formN の各評価結果をそれぞれ
リストにして、それらを連結 (nconc) したリストを作り、それを返す。
list の長さが n なら n 個のリストを作り、それらを連結したリストを返す。

<例>
        (image-can i '(1 2 3 4) (index i 4)) -> (1 2 3 4 2 3 4 3 4 4)
        (image-can i (list 1 2 3 4) (list i)) -> (1 2 3 4)         ^,-?"
   `(mapcan (lambda (,var) ,@forms) ,list))
 :documentation
 "形式 : image-can var list form1 form2 ... formN
list の第 1 要素を変数 var に代入して form1 form2 ... を順に評価する。
次に list の第 2 要素を var に代入して form1 form2 ... を評価する。
これを継続し、list の最後の要素を var に代入して form1 form2 ... を
評価したところで終了する。そして最後の formN の各評価結果をそれぞれ
リストにして、それらを連結 (nconc) したリストを作り、それを返す。
list の長さが n なら n 個のリストを作り、それらを連結したリストを返す。"
 :example
 "(image-can i '(1 2 3 4) (index i 4)) -> (1 2 3 4 2 3 4 3 4 4)
        (image-can i (list 1 2 3 4) (list i)) -> (1 2 3 4)")


(define
 "imagen"
 (macro (&body binds-and-form)
     "imagen                                 関数[#!macro]

<説明>
  形式 : imagen (var1 list1) (var2 list2) ...  (varN  listN) form
まず、list1 list2 ... の第 1 要素をそれぞれ変数 var1 var2 ... に
代入して form を評価する。次に list1 list2 ... の第 2 要素をそれぞれ
var1 var2 ... に代入して form を評価する。これを継続し、list1 list2
... の最後の要素をそれぞれ var1, var2, ... に代入して form を評価して
終了する。そして以上の各評価結果を順に並べてリストを作り、それを返す。
list1 list2 ... の長さが違う場合には最も短いリストの最後の要素に対する
form の評価により終了する。他のリストの余分な要素は無視される。
返されるリストの長さは一番短い引数リストと同じ長さ。

<例>
        (imagen (a '(1 2 3))(b '(1 2 3))(!c (+ a b))) ->
        (2 4 6)  c -> 6
        (imagen (a '(1 2))(b '(1 2 3))(!c (+ a b))) -> (2 4)  c -> 4"
   (cl:let* ((binds (butlast binds-and-form))
             (form (car (last binds-and-form)))
             (vars  (mapcar #'first  binds))
             (lists (mapcar #'second binds)))
     `(mapcar (lambda (,@vars) ,form) ,@lists)))
 :documentation
 "形式 : imagen (var1 list1) (var2 list2) ...  (varN  listN) form
まず、list1 list2 ... の第 1 要素をそれぞれ変数 var1 var2 ... に
代入して form を評価する。次に list1 list2 ... の第 2 要素をそれぞれ
var1 var2 ... に代入して form を評価する。これを継続し、list1 list2
... の最後の要素をそれぞれ var1, var2, ... に代入して form を評価して
終了する。そして以上の各評価結果を順に並べてリストを作り、それを返す。
list1 list2 ... の長さが違う場合には最も短いリストの最後の要素に対する
form の評価により終了する。他のリストの余分な要素は無視される。
返されるリストの長さは一番短い引数リストと同じ長さ。"
 :example
 "(imagen (a '(1 2 3))(b '(1 2 3))(!c (+ a b))) -> 
        (2 4 6)  c -> 6
        (imagen (a '(1 2))(b '(1 2 3))(!c (+ a b))) -> (2 4)  c -> 4")


(define
 "imagpart"
 #'imagpart
 :documentation
 "形式 : imagpart number
number の型が複素数の場合、その数の虚部を、浮動小数点数の場合、
同一形式の浮動小数点数の 0 を、その他の場合は (* 0 x) を返す。
realpart 参照。"
 :example
 "(imagpart #c(2.2 -0.234)) -> -0.234
        (imagpart #c(2.2 0.234) -> 0.234
        (imagpart 3.09f0) -> 0.0f0")


(define
 "import"
 #'import
 :documentation
 "形式 : import &rest 'symbol
symbol がカレントパッケージにあれば、それを削除して t を返し、
なければ nil を返す。"
 :example
 "(import edit ledit) -> nil")


(define
 "in-package"
 (cl-macro in-package)
 :documentation
 "形式 : in-package package &rest key
カレントパッケージを package に変更する。それが存在しないパッケージなら、
新しいパッケージを作りそれをカレントパッケージとする。"
 :example
 "(package-name (current-package)) -> \"abc\"
        (in-package sys:bas-package)) -> {vector}32228(package . 12))
        (package-name (current-package)) -> \"bas\"")


(define
 "inc"
 (macro (var &optional (val 1))
     "inc                                    関数[#!macro]

<説明>
  形式 : inc var &opt val
var の値に val の値 (既定値は 1) を加えた結果を var に代入し、その値
を返す。(!!+ !x n) と同じ。

<例>
        (!x 10) -> 10
        (inc x) -> 11
        (!x 11) -> 11
        (inc x -2) -> 9
        x -> 9
        (inc 3 2) -> エラー"
   `(!!+ !,var ,val))
 :documentation
 "形式 : inc var &opt val
var の値に val の値 (既定値は 1) を加えた結果を var に代入し、その値
を返す。(!!+ !x n) と同じ。"
 :example
 "(!x 10) -> 10
        (inc x) -> 11
        (!x 11) -> 11
        (inc x -2) -> 9
        x -> 9
        (inc 3 2) -> エラー")


(define
 "incf"
 (cl-macro incf)
 :documentation
 "形式 : incf number1 &opt number2
浮動小数点数 number1 に number2 (既定値 1) を加えた結果を number1 に
代入し、その値を返す。
引数が浮動小数である点を除いて inc と同じ。"
 :example
 "(!x 10.0f) -> 10.0f
        (incf x) -> 11.0f
        (!x -10.0f) -> -10.0f
        (incf x) -> -9.0f
        (incf 10) -> エラー")


(define
 "index"
 (expr (start end &optional (increment 1))
   "<説明>
  形式 : index start end &opt increment
start (数)で始まり end (数)で終わる数値リストを作成し、その結果を返す。
その数値リストの要素の値は increment (数 : 既定値は 1)ずつ増していく。
start が end より小さい場合、increment は正の数でなければならない。
start が end より大きい場合、increment は負の数でなければならない。
上記条件を満足しない場合、nil を返す。なお、for 関数において使用する
場合、数値リストは実際には作られない。

<例>
        (index 1 5) -> (1 2 3 4 5)
        (index 5 1) -> nil
        (index 1 9 3) -> (1 4 7)
        (index 10 0 -3) -> (10 7 4 1)
        (index 1 5 -1) -> nil"
   (and (or (and (< start end) (plusp increment))
            (and (> start end) (minusp increment)))
        (prog (l cnt pred)
              (setq pred (if (plusp increment) #'> #'<))
              (setq l () )
              (setq cnt start)
              l  (cond ((funcall pred cnt end) (return (nreverse l))))
              (push cnt l)
              (setq cnt (+ cnt increment))
              (go l))))
 :documentation
 "形式 : index start end &opt increment
start (数)で始まり end (数)で終わる数値リストを作成し、その結果を返す。
その数値リストの要素の値は increment (数 : 既定値は 1)ずつ増していく。
start が end より小さい場合、increment は正の数でなければならない。
start が end より大きい場合、increment は負の数でなければならない。
上記条件を満足しない場合、nil を返す。なお、for 関数において使用する
場合、数値リストは実際には作られない。"
 :example
 "(index 1 5) -> (1 2 3 4 5)
        (index 5 1) -> nil
        (index 1 9 3) -> (1 4 7)
        (index 10 0 -3) -> (10 7 4 1)
        (index 1 5 -1) -> nil")


(define
 "input-stream-p"
 #'input-stream-p
 :documentation
 "形式 : input-stream-p stream
stream が入力可能なストリームなら、そのストリームを、そうでなければ
nil を返す。"
 :example
 "(!aa (open \"asd.tao\")) -> {udo}71499file-stream
        (input-stream-p aa) -> {udo}71499file-stream")


(define
 "inspect"
 #'inspect
 :documentation
 "形式 : inspect object
object についての情報を会話的に得る。"
 :example
 "")


(define
 "int-char"
 #'code-char
 :documentation
 "形式 : int-char integer
非負の整数 integer に対応する文字を返す。"
 :example
 "(int-char 65) -> \"A\"
        (int-char 97) -> \"a\"
        (int-char 128) -> nil")


(define
 "integer"
 (class integer)
 :documentation
 "このクラスのインスタンスは fixnum でも bignum でもよい。
fixnum は 符号ビットを含め、24 ビットで表現。
bignum は 24 ビット以上で表現。リスト構造を持つ。"
 :example
 "")


(define
 "integer-decode-float"
 #'integer-decode-float
 :documentation
 "形式 : integer-decode-float number
浮動小数点数 number について次の 3 つの値を返す。
1) 小数部を表す整数   2) 指数部を表す整数   3) 1 または -1 (符号)"
 :example
 "(integer-decode-float 0.0) -> !(0 0 1)
        (integer-decode-float 3.0) -> !(#600000 -16 1)
        (integer-decode-float -0.24) -> !(#753412 -20 -1)")


(define
 "integer-length"
 #'integer-length
 :documentation
 "形式 : integer-length integer
仮想的な 2 の補数表現における integer のビット表現を考え、その有効
ビット長を整数で返す。"
 :example
 "(integer-length 0) -> 0
        (integer-length 1) -> 1
        (integer-length 3) -> 2
        (integer-length 4) -> 3
        (integer-length 7) -> 3
        (integer-length -2) -> 0
        (integer-length -4) -> 2
        (integer-length -7) -> 3
        (integer-length -8) -> 3")


(define
 "integerp"
 #'cl:integerp
 :documentation
 "形式 : integerp number
number が整数 (shortnum または bignum) ならその評価値を返し、
それ以外なら nil を返す。"
 :example
 "(integerp 1) -> 1
        (integerp 1000000000000000000)
               -> 1000000000000000000
        (integerp #7776000002) -> #7776000002")


(define
 "intern"
 #'intern
 :documentation
 "形式 : intern name &opt package
package (既定値はカレントパッケージ) と、それがユースしているパッケージ
の中に印字名が name のシンボルがあれば第 1 の値として name を、第 2 の
値として次のどれかを返す。
:internal    シンボルは内部シンボルとしてそのパッケージに直接存在
:external    シンボルは外部シンボルとして直接存在
:inherited   シンボルはユースしているパッケージの外部シンボル
なければ、印字名が name のシンボルを新たに作り package に登録し、
その値を第 1 の値、第 2 の値として nil を返す。"
 :example
 "(intern \"thief\") -> !(thief :internal)
        (intern \"nil\") -> !(nil :internal)
        (intern \"123\") -> !(\\123 :internal)")


(define
 "intern-local"
 (subr nil)
 :documentation
 "形式 : intern-local name &opt package
package (既定値はカレントパッケージ) の中で、印字名が name のシンボルが
あれば name を返し、なければ、印字名が name のシンボルを新たに作り
package に登録し、その値を返す。
ユースするパッケージを検索しない点を除いて intern と同じ。"
 :example
 "(intern-local 'asd) -> asd")


(define
 "intern-local*"
 (subr nil)
 :documentation
 "形式 : intern-local* &rest symbol1 symbol2 ... symbolN
symbol1 symbol2 ... symbolN がカレントパッケージにある場合は、それを
シャドウイングシンボルとして登録し、なかった場合は、新たにカレント
パッケージの中で生成し、シャドウイングシンボルとして登録する。"
 :example
 "(intern-local* car) -> (car)
        (de car (x) (caar x)) -> car
        (car '((a) c)) -> a
        普通のプリミティブな car に戻りたいときは import を使い、
        (import car) を実行する。")


(define
 "internal-time-units-per-second"
 (constant internal-time-units-per-second)
 :documentation
 "1 秒当たりのインターナル・タイム・ユニットを意味する整数。
ELIS では 50 ( 1 ユニットは 20 ミリ秒であることを意味)。"
 :example
 "")


(define
 "interprocess-closure"
 (macro nil)
 :documentation
 "形式 : interprocess-closure &rest var1 var2 ... varN
いくつかのプロセス間で共有されるクロージャを生成し返す。変数 var1 var2... varN はクローズされる。
この関数が返す値は、関数 make-process においてキーワード引数 
:interprocess-closure に代入して使う。"
 :example
 "(!aa (let ((x 123) (y 456)) (declare 
                                      (special x y *standard-output*))
                  (interprocess-closure 'x 'y *standard-output*)))
        -> {applobj}1652187(#!closure . 8)
        (!pro (make-process 'pro-name :inter-process-closure aa))
        -> {udo}1636913process
        (de func1 () (write x) (write y)) -> ({udo}1636913process)
                                              123
                                              456
        123 と 456 はクローズされた変数 x と y の値。
        123 と 456 は、*standard-output* がクローズされなければ、端末
        にはプリントされない。")


(define
 "intersection"
 #'intersection
 :documentation
 "形式 : intersection list1 list2 &key :test :test-not :key
list1 と list2 の共通要素を抽出し、その結果をリスト形式で返す。
共通要素がない場合は、nil を返す。"
 :example
 "(intersection'(a b c) '(f a d))  -> (a)")


(define
 "intersectionq"
 (subr (list1 &rest lists)
   "<説明>
  形式 : intersectionq list1 &rest list2 ... listN
list1 list2 ... listN のどれにも含まれている要素で構成されるリストを
返す。各要素が等しいか否かのチェックは関数 eq で行う。返される値に
おける要素の並び順序は、必ずしも元のリストの並び順序と一致しない。

<例>
        (intersectionq '(a a a a)) -> (a)
        (intersectionq '(2 4 6 8 10 12 14 16) '(3 6 9 12 15)
                '(1 3 4 6 8 10 12))
                  -> (6 12)"
   (prog (l ls)
         (setq l list1)
         (setq ls lists)
         l  (cond ((endp ls) (return (delete-duplicates l))))
         (setq l (intersection l (car ls)))
         (setq ls (cdr ls))
         (go l)))
 :documentation
 "形式 : intersectionq list1 &rest list2 ... listN
list1 list2 ... listN のどれにも含まれている要素で構成されるリストを
返す。各要素が等しいか否かのチェックは関数 eq で行う。返される値に
おける要素の並び順序は、必ずしも元のリストの並び順序と一致しない。"
 :example
 "(intersectionq '(a a a a)) -> (a)
        (intersectionq '(2 4 6 8 10 12 14 16) '(3 6 9 12 15) 
        	'(1 3 4 6 8 10 12))
                  -> (6 12)")


(define
 "isqrt"
 #'isqrt
 :documentation
 "形式 : isqrt integer
integer (負以外の整数)の平方根を超えない最大の整数を返す。"
 :example
 "(isqrt 9) -> 3
        (isqrt 12) -> 3
        (isqrt 300) -> 17
        (isqrt 325) -> 18")


;;; *EOF*
