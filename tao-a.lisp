;; -*- Mode: Lisp; Package: TAO; -*-
(tao:tao)
(in-package #:tao-internal)


(defun *abolish (funct arity)
  (mapc #'tao.logic::retract-clause
        (remove-if-not (lambda (x)
                         (and (= arity (length (cdr (car x))))
                              (eq funct (car (car x)))))
                       (tao.logic::get-clauses funct)))
  (fmakunbound funct)
  (fmakunbound (tao.logic::make-predicate funct arity))
  T)


(defmacro tao:abolish (funct arity)
  "abolish                                関数[#!&+]

 <説明>
   形式 : abolish funct arity
 次の 2 つの条件を満足する全ての宣言を削除する。
 宣言の先頭の主ファンクタが funct の値と等しいこと。
 宣言の先頭の引数の数が arity の値と等しいこと。
 retract 参照。

 <例>
         (assert (himitsu)) -> himitsu
         (assert (himitsu _x)) -> himitsu
         (assert (himitsu _x _y)) -> himitsu
         (assert (himitsu 2 3) -> himitsu
         (assert (himitsu _x _y _z)) -> himitsu
         (abolish himitsu 3) -> t
         (himitsu _x _y _z) は削除
         (abolish himitsu 2) -> t
         (himitsu _x _y) と (himitsu 2 3) は削除"
  `(*abolish ',funct ',arity))


;;; abort                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : abort  terno
;;; ターミナル番号 terno から、現在実行して得られた全ての結果を捨て、
;;; トップレベルに戻る。

(defclsynonym tao:abs
    "abs                                    関数[#!subr]

<説明>
  形式 : abs number
number の絶対値を返す。

<例>
        (abs 10) -> 10
        (abs -10) -> 10")

(defclsynonym tao:acons
    "acons                                  関数[#!subr]

<説明>
  形式 : acons key data a-list
key と data の対を、連想リスト a-list に加える。
\(acons key data a-list) = (cons (cons key data) a-list)

<例>
        (!x '((aka . red) (shiro . white))) ->
        	((aka . red) (shiro . white))
        (acons 'kuro 'black x) ->
        	((kuro . black) (aka . red) (shiro . white))")

(defclsynonym tao:acos
    "acos                                   関数[#!subr]

<説明>
  形式 : acos number
number の逆余弦 (arc cosine) を返す。
number の絶対値が 1 より大きい場合、複素数を返す。

<例>
        (acos -1.0f0) -> 3.1415926535898f0
        (acos 1.0f0) -> 0.0f0
        (acos 0.5f0)-> 1.0471975511966f0
        (acos 2.0f0) -> #c(0.0f0 1.31635789692481f0)")

(defclsynonym tao:acosh
    "acosh                                  関数[#!subr]

<説明>
  形式 : acosh number
number の逆双曲的余弦 (hyperbolic arc cosine) を返す。
number が 1 未満の場合、複素数の値を返す。

<例>
        (acosh 1.0f0) -> 0.0f0
        (acosh 0.5f0) -> #c(0.0f0 1.0471975511966f0)
        (acos -0.5f0) -> #c(0.0f0 2.0946951023932f0)")

(defun tao:addprop (sym value key)
  "addprop                                関数[#!expr]

<説明>
  形式 : addprop p-list value key
属性リスト p-list に、key があれば、value を key の属性値と連結 (cons)
し、その結果を返す。なければ、key の属性値を value として p-list に
加え、その結果を返す。
\(addprop symbol value indicator) =
\(putprop symbol (cons value (get symbol indicator)) indicator)

<例>
        (!(plist 'aaa) '(a 1 b 2 c 3)) -> (a 1 b 2 c 3)
        (addprop 'aaa 4 'c) -> (4 . 3)
        (plist 'aaa) -> (a 1 b 2 c (4 . 3))
        (addprop 'aaa 5 'd) -> (5)
        (plist 'aaa) -> (d (5) a 1 b 2 c (4 . 3))"
  (let* ((origval (get sym key))
         (newval (cons value origval)))
    (!(get sym key) newval)
    newval))

(defclsynonym tao:adjoin
    "adjoin                                 関数[#!macro]

<説明>
  形式 : adjoin item list &key :test :test-not :key
list に、要素 item を追加する。ただし、その要素は、まだ list の要素に
含まれていないもの。関数 member,cons 参照。
<例>
        (adjoin 2 '(1 3))->(2 1 3)
        (adjoin 2 '(1 2 3))->(1 2 3)")

;;; adjust-array                           関数[#!macro]
;;;
;;; <説明>
;;;   形式 : adjust-array array dimensions
;;;         	 &key :element-type :initial-element :initial-contents
;;;                    :fill-pointer :displaced-to :displaced-index-offset
;;; 配列 array と同一の型と次元を持ち、大きさ dimensions を持つ配列を返す。
;;; :element-type はエラーチェックのために指定する。:initial-element は追加
;;; される要素の初期値を指定する。:fill-pointer は array がフィルポインタを
;;; 持っているとき、非負整数ならそれが変更後のフィルポインタになり、t なら
;;; array のフィルポインタが変更後のフィルポインタになる。:displaced-to と
;;; :displaced-index-offset は変更後の配列が displaced かどうか、displaced
;;; ならどの配列に対するもので、どのように要素が対応するかを、make-array の
;;; 場合と同様に指定する。:displaced-to が nil なら array が displaced で
;;; あっても変更後は普通の (要素を持った) 配列となる。
;;;
;;; <例>
;;;         (!x (make-array '(2 5) :adjustable t)) ->
;;;         	{vector}53313(common:array . 4)
;;;         (adjust-array x '(3 3)) -> {vector}53313(common:array . 4)
;;;         (adjust-array x 10) -> ("not same rank" adjust-array (10))
;;; ＠
;;; adjustable-array-p                     関数[#!expr]
;;;
;;; <説明>
;;;   形式 : adjustable-array-p array
;;; 配列 array が可変であれば t、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (!x (make-array '(2 5) :adjustable t))
;;;                 -> {vector}53313(common:array . 4)
;;;         (adjustable-array-p x) -> t
;;;         (!y (make-array '(5 5)))
;;;         	-> {applobj}70768(#!array . 10)
;;;         (adjustable-array-p y) -> nil
;;; ＠
;;; advise                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : advise terno &opt local-echo
;;; この関数が端末 A で呼ばれ、terno を端末 B のターミナル番号とする。
;;; 端末 A のユーザは端末 B を使うことができ、端末 B のユーザにアドバイス
;;; できる。端末 A で入力されたコマンドやフォームは、端末 B で入力された
;;; のと同様に実行される。
;;; ＠
;;; all-deleted-files                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : all-deleted-files &opt pathname
;;; ディレクトリ pathname 中で削除された全ファイルのリストを返す。
;;; pathname の既定値は、カレントディレクトリ。ただし、pathname の
;;; バージョン番号を省略すると、最新に削除されたバージョンが指定される。
;;; pathname バージョン番号がワイルドカードなら、削除された全バージョンが
;;; リストされる。
;;;
;;; <例>
;;;         (delete-file "ts.tao") -> ("bs:<dire>ts.tao.2")
;;;         (delete-file "tss.tao") -> ("bs:<dire>tss.tao.5")
;;;         (all-delete-files) ->
;;;    	("bs:<dire>tss.tao.5" "bs:<dire>ts.tao.2")
;;;         (expunge-files) -> (2 files 221 byte are expunged)
;;;         (all-delete-files) -> ()
;;; ＠
;;; all-directories                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : all-directories &opt pathname flag
;;; デバイスまたはディレクトリ pathname 中に現存する全ディレクトリのリスト
;;; を返す。flag の値が nil (既定値) なら "bs:<dir>"のように文字列表示で
;;; リストを返す。そうでなければデバイス名とディレクトリ名のドットペアの
;;; リストを返す。
;;;
;;; <例>
;;;         (all-directories "bs:") -> ("bs:<dir1>" "bs:<dir2>" ... )
;;;         (all-directories "bs:" t) ->
;;;         	(("bs" . "dir1") ("bs" . "dir2") ... )


;(defun all-files (&optional (pathname *default-pathname-defaults*)))

;;; all-files                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : all-files &opt pathname flag
;;; デバイスまたはディレクトリ pathname 中の、全てのファイルのリストを返す。
;;; pathname の既定値はカレントディレクトリ。flag の値が nil (既定値) なら、
;;; ファイル名にはバージョン番号が付けられるが、nil でなければバージョン
;;; 番号は省略される。
;;;
;;; <例>
;;;         (all-files "cs:<dir1>" t) -> ("cs:<dir1>file1.tao"
;;;         	                      "cs:<dir1>file2.tao"
;;;                       	              ... )

;;; applobj                                クラス
;;;
;;; <説明>
;;;   インスタンスが、他のオブジェクトに適用することのできるオブジェクト。
;;; 関数そのものが applobj のインスタンス。

(defun tao:applobj-of (func)
  "applobj-of                             関数[#!subr]

<説明>
  形式 : applobj-of func
関数オブジェクト func があればそれを返し、なければ nil を返す。

<例>
        (applobj-of 'member) -> {applobj}25431(#!subr-simple . 6)
        (applobj-of 'fkjdfd) -> nil
        (applobj-of {applobj}25431(#!subr-simple . 6))
            -> {applobj}25431(#!subr-simple . 6)"
  (typecase func
    (function func)
    (symbol (and (fboundp func)
                 (fdefinition func)))
    (otherwise nil)))

(defun tao:applobjp (obj)
  "applobjp                               関数[#!subr]

<説明>
  形式 : applobjp func
関数 func が以下の 14 タイプの関数オブジェクトなら、func を返し、
それ以外なら nil を返す。

#!&+ applobj              #!&+dyn applobj
#!array applobj           #!closure applobj
#!expr applobj            #!exprdyn applobj
#!expr-simple applobj     #!exprdyn-simple applobj
#!hclauses applobj        #!macro applobj
#!subr applobj            #!subr-simple applobj
#!subst applobj           #!unit-clauses applobj

<例>
      (applobjp (lambda (x) (ncons x)))
                                    -> {applobj}12345(#!exprdyn . 6)
      (applobjp (&+ ((_x . _)) _x)) -> {applobj}54321(#!&+ . 6)
      (applobjp 'car) -> nil"
  (typecase obj
    (function obj)
    (otherwise nil)))

(defun tao:apply (func list)
  "apply                                  関数[#!subr]

<説明>
 形式 : apply func list
関数 func を list に適用する。

<例>
        (apply '+ (list 1 2 3 4 5 6 7 8 9)) -> 45
        (apply (lambda  (x y) (list y x)) (list 123 456)) -> (456 123)"
  (cl:apply func list))

(defsynonym common:apply cl:apply
  "common:apply                           関数[#!macro]

<説明>
  形式 : common:apply func arg1 &rest arg2 ... argN
引数 arg1 arg2 ... argN に関数 func を適用する。

<例>
        (common:apply '+ (list 1 2 3 4 5 6 7 8 9)) -> 45
        (common:apply 'max 1 2 '(3 4 5)) -> 5")

;;  3:35pm Monday, 6 August 2007
(defun tao:apply* (func &rest args)
  "apply*                                 関数[#!subr]

<説明>
  形式 : apply* func &rest arg1 arg2 ... argN
arg1 arg2 ... argN を評価した後、それらの値に関数 func を適用する。

<例>
        (apply* '+ 1 2 3 4 5 6 7 8 9) -> 45
        (apply* (lambda (x y) (list y x)) 123 456) -> (456 123)
        (apply* (lambda (x y) (list y x)) 'b 'a) -> (a b)"
  (apply func args))

;;; applyhook                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : applyhook func1 list func2 func3 &opt env
;;; 変数 *evalhook* と *applyhook* を 関数 func2 と func3 にそれぞれ
;;; バインドし、list のすべての要素を引数として func1 を呼び出し、func1 の
;;; 返す全値を返す。デバッグを助けるためのフック機能を使うことができる。

(defclsynonym tao:apropos
  #.(string '#:|apropos                                関数[#!expr]

<説明>
  形式 : apropos string &opt pkg
パッケージ pkg において、印字名の中に string を副文字列として含む
シンボルをすべて検索し、それらのシンボル名を印字する。
pkg が省略されると、カレントパッケージにリンクされる全てのパッケージを
検索する。

<例>
        (apropos "str" sys:bas-package) は、次のものをプリントする。
        bas:*print-no-string-marker*
   	*print-string-marker
   	...
   	write-string write-to-string wstrhbo wstrhl|))

(defclsynonym tao:apropos-list
  "apropos-list                           関数[#!expr]

<説明>
  形式 : apropos-list string &opt pkg
パッケージ pkg において、印字名の中に string を副文字列として含む
シンボルをすべて検索し、それらのシンボル名のリストを返す。
pkg が省略されると、カレントパッケージにリンクされる全てのパッケージを
検索する。

<例>
        (apropos-list \"str\" sys:bas-package)
        	-> (bas:*print-no-string-marker*
        	   *print-string-marker ...
        	   write-string write-to-string wstrhbo wstrhl)")

(defclsynonym tao:aref
    "aref                                   関数[#!subr]

<説明>
  形式 : aref array &rest data
配列 array の要素 data をアクセスし、その値を返す。

<例>
        (!a (make-array 10)) ->
        	{vector}1791495(common:simple-general-vector . 10)
        (aref a 1) -> nil
        (!(aref a 1) '1) -> 1
        (aref a 1) -> 1")

(defun tao:array (&rest dimensions)
  "array                                  関数[#!expr]

<説明>
  形式 : array &rest dimension1 dimension2 ... dimensionN
配列を作り、その関数オブジェクトが返る。代入の形で使用する。
配列の次元に制限はない。dimension1 dimension2 ... dimensionN が各次元の
大きさを指定し、引数の数 N が配列の次元になる。
各次元の大きさは、第 1 インデックス first と最終インデックス last
のリスト (first last) としても指定できる。

<例>
        (!y (array 5 6)) -> {applobj}31182(#!array.10)
        (!z (array '(3 6) '(2 -2))) -> {applobj}31346(#!array.10)
        (!u (array #!8b-memblk '(1 10))) ->
              {applobj}31368(#!array . 8)
        y は 5 行 6 列の 2 次元配列で第 1 次元のインデックスは
        0 から 4 まで、第 2 次元のインデックスは 0 から 5 まで。
        z は 4 行 5 列の 2 次元配列で第 1 次元のインデックスは
        3 から 6 まで、第 2 次元のインデックスは -2 から 2 まで。
        u は 1 次元配列で 1 から 10 までのインデックスをとり、その 1
        ユニットは 8 ビット。"
  (make-array dimensions))

(defclsynonym tao:array-dimension
    "array-dimension                        関数[#!expr]

<説明>
  形式 : array-dimension array rank
配列 array において、次元 rank の大きさが返される。array が
フィルポインタを持つベクタであれば、そのベクタのトータルサイズが、
返される。

<例>
        (!b (make-array '(10 10))) -> {applobj}1287843(#!array .10)
        (array-dimension b 1) -> 10
        (array-dimension b 0) -> 10
        (array-dimension b 2) -> エラー")

;;; ＠
;;; array-dimension-limit                  定数
;;;
;;; <説明>
;;;   配列の各次元の値の上限 (その値は含まない) を表す正の整数。
;;; array-dimension-limit = 8388608
;;; ＠
;;; array-dimensions                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-dimensions array
;;; 配列 array の次元を要素とするリストが返される。
;;;
;;; <例>
;;;         (!b (make-array '(10 10))) -> {applobj}1287843(#!array .10)
;;;         (array-dimensions b) -> (10 10)
;;;         (!x (make-array 3)) ->
;;;                   {vector}1288293 (common:simple-generl-vector . 3)
;;;         (array-dimensions x) -> 3
;;; ＠
;;; array-element-type                     関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-element-type array
;;; 配列 array に格納され得るオブジェクトのセットに対する型指定子が返される。
;;;
;;; <例>
;;;         (array-element-type (make-array 5 :element-type '(mod 5)))
;;;           -> (mod 16)
;;;         (array-element-type (make-array 3 :element-type 'bit)) -> bit
;;; ＠
;;; array-has-fill-pointer-p               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-has-fill-pointer-p array
;;; 配列 array がフィルポインタを持っていれば t を返し、そうでなければ
;;; nil を返す。array が 1 次元でなければ常に nil を返す。
;;;
;;; <例>
;;;         (!x (make-array 3 :fill-pointer t)) ->
;;;         	{vector}77794(common:array . 3)
;;;         (array-has-fill-pointer-p x) -> t
;;;         (!y (make-array '(5 5)))
;;;         	-> {applobj}70768(#!array . 10)
;;;         (array-has-fill-pointer-p) -> nil
;;; ＠
;;; array-in-bounds-p                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-in-bounds-p array &rest integer1 integer2 ... integerN
;;; 添字 integer1 integer2 ... integerN がすべて、配列 array に対して正当で
;;; あるかどうかをチェックする。すべて正当であれば t 、そうでなければnil 。
;;; integer1 integer2 ... integerN の個数 N は、array のランクに等しくな
;;; ければならない。フィルポインタを無視する。
;;;
;;; <例>
;;;         (!a (make-array 10)) ->
;;;         	{vector}1791495(common:simple-general-vector . 10)
;;;         (array-in-bounds-p a 1) -> t
;;; ＠
;;; array-info                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-info array
;;; 配列 array の情報を返す。array に配列を指定しなければ nil が返される。
;;;
;;; <例>
;;;         (!x (array '(10 20))) -> {applobj}31400(#!array . 8)
;;;         (array-info x) -> (lisp-object (rank 1) (10 20))
;;;         (!y (array 10 '(-1 3))) -> {applobj}31456(#!array . 10)
;;;         (array-info y) -> (lisp-object (rank 2) (0 9) (-1 3))
;;;         (!z (array #!8b-memblk 10)) -> {applobj}31468(#!array . 8)
;;;         (array-info z) -> (#!8b-memblk (rank 1) (0 9))
;;; ＠
;;; array-rank                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-rank array
;;; 配列 array の次元の数を返す。
;;;
;;; <例>
;;;         (!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
;;;         (!b (make-array 2)) -> {vector}1288564
;;;         			  (common:simple-general-vector .2)
;;;         (array-rank a) -> 2
;;;         (array-rank b) -> 1
;;; ＠
;;; array-rank-limit                       定数
;;;
;;; <説明>
;;;   配列の次元に関する上限 (その値を含まない) を表す正の整数。
;;; array-rank-limit = 64
;;; ＠
;;; array-row-major-index                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-row-major-index array
;;;                               &rest integer1 integer2 ... integerN
;;; 配列 array について、添字 integer1 integer2 ... integerN が指し示す要素
;;; を列順に識別している非負の整数を返す。
;;;
;;; <例>
;;;         (!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
;;;         (!b (make-array 2)) ->
;;;               {vector}1288564(common:simple-general-vector .2)
;;;         (array-row-major-index a 1 1) -> 4
;;;         (array-row-major-index b 1) -> 1
;;; ＠
;;; array-total-size                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-total-size array
;;; 配列 array の要素の全個数を返す。ゼロ次元の配列の全体の大きさは 1 。
;;; 1 次元配列の全体の大きさは、フィルポインタに関係なく計算される。
;;;
;;; <例>
;;;         (!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
;;;         (!b (make-array 2)) ->
;;;               {vector}1288564(common:simple-general-vector .2)
;;;         (array-total-size a) -> 9
;;;         (array-total-size b) -> 2
;;; ＠
;;; array-total-size-limit                 定数
;;;
;;; <説明>
;;;   1 つの配列の中の要素の総数の上限 (その値を含まない) を表す正の整数。
;;; array-total-size-limit = 8388608
;;; ＠
;;; array-type                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : array-type array
;;; 配列 array のタイプを返す。配列のタイプは、ストリング、
;;; simple-bit-vector、simple-vector、bit-vector、ベクタ、simple-bit-array、
;;; simple-array、bit-array、配列 がある。
;;;
;;; <例>
;;;         (!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
;;;         (!b (make-array 2)) ->
;;;             {vector}1288564(common:simple-general-vector .2)
;;;         (array-type a) -> (simple-array t)
;;;         (array-type b) -> (simple-vector t)

(defun tao:arrayp (arg)
  "arrayp                                 関数[#!expr]

<説明>
  形式 : arrayp arg
arg が配列なら t 、それ以外なら nil を返す。

<例>
        (!x (array '(10 20))) -> {applobj}31400(#!array . 8)
        (arrayp x) -> t"
  (and (cl:arrayp arg) 'T))

(defun common:arrayp (arg)
  "common:arrayp                          関数[#!expr]

<説明>
  形式 : common:arrayp arg
arg が配列なら関数オブジェクト、それ以外なら nil を返す。;arrayオブジェクト?

<例>
        (!x (array '(2 2))) -> {applobj}1773079(#!array . 8)
        (common:arrayp x) -> {applobj}1773079(#!array . 8)"
  (and (typep arg 'array) arg))

(defun tao:as-char (integer)
  "as-char                                関数[#!expr]

<説明>
  形式 : as-char integer
十進数 integer を対応する文字に変換し、その結果を返す。

<例>
        (as-char 97) -> \"a\"
        (as-char 65) -> \"A\""
  (code-char integer))


(defun tao:as-shortnum (arg)
  "as-shortnum                            関数[#!expr]

<説明>
  形式 : as-shortnum arg
arg のアドレス部を shortnum と見なし、その値を返す。
Lisp オブジェクトの実アドレスと文字のコードを見ることができる。

<例>
        (as-shortnum \"a\") -> 97
        (as-shortnum 'a) -> 1115112"
  (typecase arg
    (character (char-code arg))
    (T #+sbcl (sb-kernel:get-lisp-obj-address arg))))


;;; ＠
;;; ash                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : ash integer1 integer2
;;; integer2 を評価し、その結果が正なら、integer1 をその数だけ左に
;;; (負なら右に) ビットシフトし、その結果を返す。サインビットは変更しない。
;;; 左へのシフトで空白になったビット部分には 0 をつめる。右へのシフトで空白
;;; になったビット部分にはサインビットの値をつめる。
;;; 元のオブジェクトは変更しない。
;;;
;;; <例>
;;;         (ash 4 1) -> #10
;;;         (ash 10 4) -> #240
;;;         (ash 123456 -5) -> #7422
;;; ＠
;;; ashift                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : ashift integer1 integer2
;;; integer2 の値を評価し、その結果が正なら、integer1 をその数だけ左に
;;; (負なら右に) ビットシフトし、その結果を返す。サインビットは変更しない。
;;; 左へのシフトで空白になったビット部分には 0 をつめる。右へのシフトで空白
;;; になったビット部分にはサインビットの値をつめる。元のオブジェクトは変更
;;; しない。ロカティブデータ型では ash と同じ。
;;;
;;; <例>
;;;         (ashift 4 1) -> #10
;;;         (ashift 10 4) -> #240
;;;         (ashift 123456 -5) -> #7422
;;; ＠
;;; ashift                                 ロカティブオペレータ
;;;
;;; <説明>
;;;   形式 : loc1 ashift loc2
;;; 第 1 引数の算術シフトを行う。第 2 引数で左へシフトするビット数が指定
;;; される。サインビットは常に、その値を保持してる。
;;;
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- #5252) -> 2730
;;;         (q <- #7070) -> 3640
;;;         (r <- #1111) -> 585
;;;         (s <- (p ashift 3 )) -> 21840 (#52520)
;;;         s -> 21840
;;;         (s <- (p ashift -3)) -> 455 (#707)
;;;         s -> 455
;;;         (q <- (r ashift 2 and# q)) -> 2080 (#4040)
;;;         q -> 2080
;;; ＠
;;; asin                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : asin number
;;; number の逆正弦 (arc sine) を返す。
;;; number が 1 より大きい場合、複素数の値を返す。
;;;
;;; <例>
;;;         (asin 1.0f0) -> 1.5707963267949f0
;;;         (asin 0.5f0) -> 0.523598775598299f0
;;;         (asin -1.0f0) -> -1.5707963267949f0
;;;         (asin 2.0f0) -> #c(1.570963267949f0 -1.31695789692481f0)
;;; ＠
;;; asinh                                  関数[#!subr]
;;;
;;; <説明>
;;;  形式 : asinh number
;;; number の逆双曲的正弦 (hyperbolic arc sine) を返す。
;;; number に複素数指定可。
;;;
;;; <例>
;;;         (asinh -1.0f0) -> -0.881373587019543f0
;;;         (asinh 0.5f0) -> 0.481211825059603f0
;;;         (asinh 1.0f0) -> 0.881373587019543f0
;;;         (asinh #c(2 3)) -> #c(1.98668702991653f0 0.5706527843211f0)
;;; ＠

(defun tao:ass (pred data a-list)
  "ass                                    関数[#!subr]

<説明>
  形式 : ass  pred data a-list
連想リスト a-list の要素を左から右へ順に調べていき、car 部が条件
pred を満足する要素を見つけたらその要素を返し、後はもう調べない。なけ
れば nilを返す。pred は 2 引数をとる述語であり、その第 1 引数は data
で、第 2 引数は a-list の各要素の car 部。
        (ass 'eq data a-list) = (assq data a-list)
        (ass 'equal data a-list) = (assqu data a-list)

<例>
        (ass '> 5 '((6 . six) (3 . three) (4 . four))) -> (3 . three)"
  (assoc data a-list :test pred))


(defmacro define-predicate-in-lisp-world (name)
  `(defmacro ,name (&rest args)
     (let ((exit (gensym "exit-")))
       `(block ,exit
          ,(tao.logic::compile-body
            `((,',name ,@args))
            `(lambda () (return-from ,exit T))
            tao.logic::no-bindings)))))

(defun ensure-predicate-in-lisp-world (name)
  (eval `(define-predicate-in-lisp-world ,name)))

(defmacro tao:assert (&rest clauses)
  "assert                                 関数[#!macro]

<説明>
  形式 : assert &rest clause
ホーン節を定義する。この定理宣言と関連した関数の名前は、主ファンクタと
呼ばれる。同じ主ファンクタにおいて、複数の節を定義するために assert を
複数回使う時は、順番は保証されない。

<例>
\(assert (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
\(assert (concatenate ()  _x _x) )
\concatenate は、主ファンクタ。最初に定理宣言された節が最初に適用され、
2 番目に定理宣言された節が 2 番目に実行されるということは、保証されない。"
  (let ((pred (caar clauses)))
    `(progn
       (define-predicate-in-lisp-world ,pred)
       (tao.logic::prolog-compile 
        (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                               :asserta nil)))))


(defmacro tao:asserta (&rest clauses)
  " asserta                                関数[#!macro]

 <説明>
   形式 : asserta &rest clause
 節の実行の順序が指定されるということ以外は、関数 assert と同じ。
 後に言明された節ほど先に適用される。

 <例>
 (asserta (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
 (asserta (concatenate ()  _x _x) )
 2 番目に言明された節 (concatenate () _x _x) が最初に適用され、
 最初に言明された節 (concatenate (_a . _x) _y (_a . _z))
 (concatenate _x _y _z) が 2 番目に適用される。"
  (let ((pred (caar clauses)))
    `(progn
       (define-predicate-in-lisp-world ,pred)
       (tao.logic::prolog-compile 
        (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                               :asserta T)))))


(defmacro tao:assertz (&rest clauses)
  " assertz                                関数[#!macro]

 <説明>
   形式 : assertz &rest clause
 節の実行の順序が指定される以外は、関数 assert と同じ。
 先に言明された節ほど先に適用される。

 <例>
 (assertz (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
 (assertz (concatenate ()  _x _x) )
 最初に言明された節 (concatenate (_a . _x) _y (_a . _z))
 (concatenate _x _y _z) が最初に適用され、2 番目に言明された節
 (concatenate () _x _x) が 2 番目に適用される。
"
  (let ((pred (caar clauses)))
    `(progn
       (define-predicate-in-lisp-world ,pred)
       (tao.logic::prolog-compile 
        (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                               :asserta nil)))))


(defmacro tao:assign-cons (object list)
  "assign-cons                            関数[#!expr]

<説明>
  形式 : assign-cons object list
list を object へ代入する代入式を作る。(!object list)

<例>
        (assign-cons 'x '(y)) -> (!x y)
        (assign-cons 'x '(1)) -> (!x 1)
        (eval (assign-cons 'y '('(1 2 3)))) -> (1 2 3)
        y -> (1 2 3)
        (!x (1 2 3))
        (eval (assign-cons '(car x) '(4))) -> 4
        x -> (4 2 3)"
  (let ((unq-object (cadr object))
	(unq-list   (cadr list)))
    `(list 'setf ,@(mapcar #'(lambda (item)
			       `(quote ,item))
			   (cons unq-object unq-list)))))

;;; assign-list                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : assign-list object1 object2
;;; object2 を object1 へ代入する代入式を作る。(!object1 object2)
;;;
;;; <例>
;;;         (assign-list 'x 1) -> (!x 1)

;;; assign-logical-name                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : assign-logical-name logical physical &opt host globalp
;;; フィジカル名 phygical を、ロジカル名 logical に代入する。logical の最終
;;; キャラクタは : でなければならない。ホスト名 host の既定値は、
;;; *default-pathname-defaults* による。host の値が "tops::" なら
;;; DEC2060(Tops) におけるフィジカル名が代入される。代入は globalp の値が
;;; nil なら、カレントプロセスに閉じたものとなる。globalp の既定値は nil 。
;;;
;;; <例>
;;;         (assign-logical-name "n:" 'cs:<nanja-monja>)
;;;         (assign-logical-name "mine:" "ps:<pin-pon-pan>" "tops::")

(defun tao:assignee-cons (sym)
  "assignee-cons                          関数[#!expr]

<説明>
  形式 : assignee-cons object
object の先頭に ! を付けて返す。

<例>
        (assignee-cons 'x) -> !x"
  (values (intern (concatenate 'string "!" (string sym)))))

(defun tao:assigneep (form)
  "assigneep                              関数[#!subr]

<説明>
  形式 : assigneep object
object が先頭に ! がついた表現になっていれば、それを返し、
そうでなければ nil を返す。

<例>
        (assigneep (caddr '(!!cons 1234 !x))) -> !x"
  (and (symbolp form)
       (string= "!"
                (subseq (string form) 0 1))))

(defun tao:assignp (form)
  "assignp                                関数[#!subr]

<説明>
  形式 : assignp object
object が代入式ならば、それを返し、そうでなければ nil を返す。

<例>
      (assignp '(!x (x + 1))) -> (!x (x + 1))"
  (and (consp form)
       (eq 'setf (car form))))

(defclsynonym tao:assoc
    "assoc                                  関数[#!macro]

<説明>
  形式 : assoc key a-list &key :test :test-not :key
連想リスト a-list 中で、key と一致するキーを持つ対のうち最初の対を返す。

<例>
        (assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
        -> (r . x)
        (assoc 'goo '((foo . bar) (zoo . goo))) -> nil
        (assoc '2 '((1 a b c) (2 b c d) (-7 x y z))) -> (2 b c d)")

(defclsynonym tao:assoc-if
    "assoc-if                               関数[#!expr]

<説明>
  形式 : assoc-if pred a-list
連想リスト a-list 中で、その car 部が、述語 pred を満足する対のうち最初
の対を返す。なければ nil を返す。

<例>
        (assoc-if #'integerp '((ichi . one) (1 . 0) (2.9 . 3.8)))
        ->  (1 . 0)
        (assoc-if #'symbolp '((boku . i) (kimi . you) (1 . 3)))
        ->  (boku . i)")

(defclsynonym tao:assoc-if-not
    "assoc-if-not                           関数[#!expr]

<説明>
  形式 : assoc-if-not pred a-list
連想リスト a-list 中で、その car 部が、述語 pred を満足しない対のうち
最初の対を返す。なければ nil を返す。

<例>
        (assoc-if-not #'integerp '((ichi . one) (1 . 0)))
      	-> (ichi . one)
        (assoc-if-not #'symbolp '((boku . i) (kimi . you) (1 . 3)))
        -> (1 . 3)")

(defun tao:assq (key a-list)
  "assq                                   関数[#!subr]

<説明>
  形式 : assq key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と eq な car 部
をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ nil を
返す。(assq key a-list) = (ass eq key a-list)

<例>
        (assq 'blue '((red . nil) (green . 2) (blue . 3)))
        -> (blue . 3)
        (assq 'white '((red . nil) (green . 2) (blue . 3))) -> nil
        (assq 'red  '((red . nil) (green . 2) (blue . 3))) -> (red)"
  (assoc key a-list :test #'eq))

(defun tao:assql (key a-list)
  "assql                                  関数[#!macro]

<説明>
  形式 : assql key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と eql な car
部をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ
nil を返す。(assql key a-list) = (ass eql key a-list)

<例>
        (assq 'blue '((red . nil) (green . 2) (blue . 3)))
        -> (blue  . 3)
        (assq 'this '((this . nil) (that . z) (there . y))) -> (this)"
  (assoc key a-list :test #'eql))

(defun tao:assqu (key a-list)
  "assqu                                  関数[#!subr]

<説明>
  形式 : assqu key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と equal な
car 部をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ
nil を返す。(assqu key a-list) = (ass equal key a-list)

<例>
        (assqu 'blue '((red . nil) (green . 2) (blue . 3))) ->
        	(blue . 3)
        (assqu '1 '((1.0 . 3) (5 . 2) (1 . 6))) -> (1 . 6)"
  (assoc key a-list :test #'equal))


(defclsynonym tao:atan
    "atan                                   関数[#!subr]

<説明>
  形式 : atan number1 &opt number2
number1 を number2 で割った値の逆正接 (arc tan) をラジアン形式で返す。
number1 number2 は、複素数以外の数値。

<例>
        (atan 1.0f0) -> 0.785398163397449f0
        (atan 0.5f0) -> 0.463647609000807f0
        (atan -0.5f0) -> -0.46364760900087f0")

(defclsynonym tao:atanh
    "atanh                                  関数[#!subr]

<説明>
  形式 : atanh number
number の逆双曲的正接 (hyperbolic arc tan) を返す。
number の絶対値が 1 以上の場合、複素数を返す。

<例>
        (atanh -0.5f0) -> 0.54936144334054f0
        (atanh 0.5f) -> 0.549306144334054f0
        (atanh 0.8f0) -> 1.0986122886681f0")

(defclsynonym tao:atom
    "atom                                   関数[#!subr]

<説明>
  形式 : atom object
object がアトムで、かつ次にあげるものの 1 つとして定義されているならば
t 、そうでなければ nil を返す。
    nil, id, logic variable (itself), codnum, integer,
    ratio number, real number, string, jstring, vector,
    locative, memblk, applobj, undef, udo
\(atom x) = (not (listp x))  (x が nil 以外のとき)

<例>
        (atom nil) -> t
        (atom 'a) -> t
        (atom 123456789) -> t
        (atom '(a b)) -> nil
        (atom (car '(a b c))) -> t")
