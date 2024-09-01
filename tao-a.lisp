;; -*- Mode: Lisp; Package: TAO; -*-

(tao:common-lisp)


(in-package #:tao-internal)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun *abolish (funct arity)
  (mapc #'tao.logic::retract-clause
        (remove-if-not (lambda (x)
                         (and (= arity (length (cdr (car x))))
                              (eq funct (car (car x)))))
                       (tao.logic::get-clauses funct)))
  (fmakunbound funct)
  (fmakunbound (tao.logic::make-predicate funct arity))
  T))


(define
 "abolish"
 (macro (funct arity)
     `(*abolish ',funct ',arity))
 :documentation
 "形式 : abolish funct arity
次の 2 つの条件を満足する全ての宣言を削除する。
宣言の先頭の主ファンクタが funct の値と等しいこと。
宣言の先頭の引数の数が arity の値と等しいこと。
retract 参照。"
 :example
 "(assert (himitsu)) -> himitsu
        (assert (himitsu _x)) -> himitsu
        (assert (himitsu _x _y)) -> himitsu
        (assert (himitsu 2 3) -> himitsu
        (assert (himitsu _x _y _z)) -> himitsu
        (abolish himitsu 3) -> t
        (himitsu _x _y _z) は削除
        (abolish himitsu 2) -> t
        (himitsu _x _y) と (himitsu 2 3) は削除")


(define
 "abort"
 #'abort
 :documentation
 "形式 : abort  terno
ターミナル番号 terno から、現在実行して得られた全ての結果を捨て、
トップレベルに戻る。"
 :example
 "")


(define
 "abs"
 #'abs
 :documentation
 "形式 : abs number
number の絶対値を返す。"
 :example
 "(abs 10) -> 10
        (abs -10) -> 10")


(define
 "acons"
 #'acons
 :documentation
 "形式 : acons key data a-list
key と data の対を、連想リスト a-list に加える。
(acons key data a-list) = (cons (cons key data) a-list)"
 :example
 "(!x '((aka . red) (shiro . white))) -> 
        	((aka . red) (shiro . white))
        (acons 'kuro 'black x) -> 
        	((kuro . black) (aka . red) (shiro . white))")


(define
 "acos"
 #'acos
 :documentation
 "形式 : acos number
number の逆余弦 (arc cosine) を返す。
number の絶対値が 1 より大きい場合、複素数を返す。"
 :example
 "(acos -1.0f0) -> 3.1415926535898f0
        (acos 1.0f0) -> 0.0f0
        (acos 0.5f0)-> 1.0471975511966f0
        (acos 2.0f0) -> #c(0.0f0 1.31635789692481f0)")


(define
 "acosh"
 #'acosh
 :documentation
 "形式 : acosh number
number の逆双曲的余弦 (hyperbolic arc cosine) を返す。
number が 1 未満の場合、複素数の値を返す。"
 :example
 "(acosh 1.0f0) -> 0.0f0
        (acosh 0.5f0) -> #c(0.0f0 1.0471975511966f0)
        (acos -0.5f0) -> #c(0.0f0 2.0946951023932f0)")


(define
 "addprop"
 (expr (sym value key)
   (let* ((origval (get sym key))
          (newval (cons value origval)))
     (setf (get sym key) newval)
     newval))
 :documentation
 "形式 : addprop p-list value key
属性リスト p-list に、key があれば、value を key の属性値と連結 (cons)
し、その結果を返す。なければ、key の属性値を value として p-list に
加え、その結果を返す。
(addprop symbol value indicator) =
(putprop symbol (cons value (get symbol indicator)) indicator)"
 :example
 "(!(plist 'aaa) '(a 1 b 2 c 3)) -> (a 1 b 2 c 3)
        (addprop 'aaa 4 'c) -> (4 . 3)
        (plist 'aaa) -> (a 1 b 2 c (4 . 3))
        (addprop 'aaa 5 'd) -> (5)
        (plist 'aaa) -> (d (5) a 1 b 2 c (4 . 3))")


(define
 "adjoin"
 #'adjoin
 :documentation
 "形式 : adjoin item list &key :test :test-not :key
list に、要素 item を追加する。ただし、その要素は、まだ list の要素に
含まれていないもの。関数 member,cons 参照。"
 :example
 "(adjoin 2 '(1 3))->(2 1 3)
        (adjoin 2 '(1 2 3))->(1 2 3)")


(define
 "adjust-array"
 #'adjust-array
 :documentation
 "形式 : adjust-array array dimensions 
        	 &key :element-type :initial-element :initial-contents
                   :fill-pointer :displaced-to :displaced-index-offset
配列 array と同一の型と次元を持ち、大きさ dimensions を持つ配列を返す。
:element-type はエラーチェックのために指定する。:initial-element は追加
される要素の初期値を指定する。:fill-pointer は array がフィルポインタを
持っているとき、非負整数ならそれが変更後のフィルポインタになり、t なら
array のフィルポインタが変更後のフィルポインタになる。:displaced-to と
:displaced-index-offset は変更後の配列が displaced かどうか、displaced
ならどの配列に対するもので、どのように要素が対応するかを、make-array の
場合と同様に指定する。:displaced-to が nil なら array が displaced で
あっても変更後は普通の (要素を持った) 配列となる。"
 :example
 "(!x (make-array '(2 5) :adjustable t)) -> 
        	{vector}53313(common:array . 4)
        (adjust-array x '(3 3)) -> {vector}53313(common:array . 4)
        (adjust-array x 10) -> (\"not same rank\" adjust-array (10))")


(define
 "adjustable-array-p"
 #'adjustable-array-p
 :documentation
 "形式 : adjustable-array-p array
配列 array が可変であれば t、そうでなければ nil を返す。"
 :example
 "(!x (make-array '(2 5) :adjustable t))
                -> {vector}53313(common:array . 4)
        (adjustable-array-p x) -> t
        (!y (make-array '(5 5)))
        	-> {applobj}70768(#!array . 10)
        (adjustable-array-p y) -> nil")


(define
 "advise"
 (expr nil)
 :documentation
 "形式 : advise terno &opt local-echo
この関数が端末 A で呼ばれ、terno を端末 B のターミナル番号とする。
端末 A のユーザは端末 B を使うことができ、端末 B のユーザにアドバイス
できる。端末 A で入力されたコマンドやフォームは、端末 B で入力された
のと同様に実行される。"
 :example
 "")


(define
 "all-deleted-files"
 (expr nil)
 :documentation
 "形式 : all-deleted-files &opt pathname
ディレクトリ pathname 中で削除された全ファイルのリストを返す。
pathname の既定値は、カレントディレクトリ。ただし、pathname の
バージョン番号を省略すると、最新に削除されたバージョンが指定される。
pathname バージョン番号がワイルドカードなら、削除された全バージョンが
リストされる。"
 :example
 "(delete-file \"ts.tao\") -> (\"bs:<dire>ts.tao.2\")
        (delete-file \"tss.tao\") -> (\"bs:<dire>tss.tao.5\")
        (all-delete-files) ->
   	(\"bs:<dire>tss.tao.5\" \"bs:<dire>ts.tao.2\")
        (expunge-files) -> (2 files 221 byte are expunged)
        (all-delete-files) -> ()")


(define
 "all-directories"
 (expr nil)
 :documentation
 "形式 : all-directories &opt pathname flag
デバイスまたはディレクトリ pathname 中に現存する全ディレクトリのリスト
を返す。flag の値が nil (既定値) なら \"bs:<dir>\"のように文字列表示で
リストを返す。そうでなければデバイス名とディレクトリ名のドットペアの
リストを返す。"
 :example
 "(all-directories \"bs:\") -> (\"bs:<dir1>\" \"bs:<dir2>\" ... )
        (all-directories \"bs:\" t) -> 
        	((\"bs\" . \"dir1\") (\"bs\" . \"dir2\") ... )")


(define
 "all-files"
 (expr (&optional (pathname *default-pathname-defaults*) flag)
   (declare (ignore flag))
   (directory (merge-pathnames #P"*.*" pathname)))
 :documentation
 "形式 : all-files &opt pathname flag
デバイスまたはディレクトリ pathname 中の、全てのファイルのリストを返す。
pathname の既定値はカレントディレクトリ。flag の値が nil (既定値) なら、
ファイル名にはバージョン番号が付けられるが、nil でなければバージョン
番号は省略される。"
 :example
 "(all-files \"cs:<dir1>\" t) -> (\"cs:<dir1>file1.tao\"
        	                      \"cs:<dir1>file2.tao\"
                      	              ... )")


(define
 "all-processes"
 (expr nil)
 :documentation
 "現在プロセスプールにある全てのプロセスのリストを返す。"
 :example
 "(all-processes) -> 
          ({udo}259534process {udo}2558221process {udo}2557603process)")


(define
 "alpha-char-p"
 #'alpha-char-p
 :documentation
 "形式 : alpha-char-p char
char が、英字 (大文字及び小文字) であれば char を返し、そうでなければ
nil を返す。standard-char-p 参照。"
 :example
 "(alpha-char-p \"a\") -> \"a\"
        (alpha-char-p \"1\") -> nil
        (alpha-char-p \"ab\") -> エラー")


(define
 "alphanumericp"
 #'alphanumericp
 :documentation
 "形式 : alphanumericp char
char が英字 (大文字及び小文字) 又は数字ならば char を返し、
そうでなければ nil を返す。
(alphanumericp x) =
   (or (alpha-char-p x) (not (null (digit-char-p x))))
standard-char-p 参照。"
 :example
 "(alphanumericp \"a\") -> \"a\"
        (alphanumericp \"1\") -> \"1\"
        (alphanumericp \"@\") -> nil")


(define
 "and"
 (cl-macro and)
 :documentation
 "形式 : and &rest form1 form2 ... formN
あるフォームの評価結果が nil になるまで左から右へ順に評価し、nil と
なれば and の値は nil。最後まで nil と評価されなければ formN の値を返す。"
 :example
 "(and ((x mod 5) = 0) ((x mod 7) = 0)) -> 0
        (and (stringp x) (prins x))")


(define
 "and#"
 (locative-operator nil)
 :documentation
 "形式 : loc1 abd# loc2
2 つの引数のビット and 操作を行う。"
 :example
 "(signed-integer-locatives p q r s) -> (p q r s)
        (p <- #5252) -> 2730
        (q <- #2525) -> 1365
        (r <- #2522) -> 1362
        (s <- (p and# q )) -> 0
        s -> 0
        (r <- (p and# r)) -> 2
        r -> 2
        (q <- (p + q and# 1234)) -> 1234
        q -> 1234")


(define
 "ansi$backspace"
 (expr nil)
 :documentation
 "カーソルを現在の位置から 1 カラム左に移動させる。"
 :example
 "(defun fa () (prin1 1234567890)
                     (ansi$backspace) (prin1 'abc) (terpri)) -> fa
        fa -> 123456789abc")


(define "ansi$bell" (expr nil) :documentation "ターミナルのベルを鳴らす。" :example "")


(define
 "ansi$caution"
 (expr nil)
 :documentation
 "ユーザに適切な注意を促す。注意の促し方は、ansi$set-caution-type
で決められる。"
 :example
 "(ansi$set-caution-type :bell) -> :bell
        (ansi$caution) -> bas:caution (ベルが鳴る)")


(define
 "ansi$clear-all-tab-stop"
 (expr nil)
 :documentation
 "全てのタブストップロケーションを削除する。
ansi$clear-tab-stop ansi$set-tab-stop 参照。"
 :example
 "")


(define
 "ansi$clear-tab-stop"
 (expr nil)
 :documentation
 "現在カーソル位置のタブストップを削除する。
関数 ansi$clear-all-tab-stop,ansi$set-tab-stop 参照。"
 :example
 "")


(define
 "ansi$cr"
 (expr nil)
 :documentation
 "カーソルを現在の行の先頭に移動させる。"
 :example
 "(defun fa () (prin1 1234567890)
                     (ansi$cr) (prin1 'abc) (terpri))  -> fa
        fa -> abc34567890")


(define
 "ansi$crlf"
 (expr nil)
 :documentation
 "カーソルを新しい次行の先頭に移動させる。"
 :example
 "(defun fa () (prin1 1234567890)
                     (ansi$crlf) (prin1 'abc) (terpri))  -> fa
        fa -> 12334567890
              abc")


(define
 "ansi$cursor-down"
 (expr nil)
 :documentation
 "形式 : ansi$cursor-down &opt integer
カーソルを現在の位置から integer 行下に移動させる。
integer の既定値は 1 。"
 :example
 "")


(define "ansi$cursor-home" (expr nil) :documentation "カーソルをホームポジション (最上段の左端) に移動させる。" :example "")


(define
 "ansi$cursor-left"
 (expr nil)
 :documentation
 "形式 : ansi$cursor-left &opt integer
カーソルを現在位置から integer カラム左へ移動させる。
integer の既定値は 1 。"
 :example
 "(defun fa () (prin1 1234567890)
                    (ansi$cursor-left 5) (prin1 'abc) (terpri))  -> fa
        fa -> 123345abc90")


(define
 "ansi$cursor-position"
 (expr nil)
 :documentation
 "形式 : ansi$cursor-position integer1 integer2
カーソルを integer1 行 integer2 カラムへ移動させる。"
 :example
 "")


(define
 "ansi$cursor-position-p"
 (expr nil)
 :documentation
 "カーソルの位置をリストにして返す。第 1 要素は行 No を、第 2 要素は
列 No を表す。"
 :example
 "(ansi$cursor-position-p) -> (23 0)  (23 行 0 カラム)")


(define
 "ansi$cursor-right"
 (expr nil)
 :documentation
 "形式 : ansi$cursor-right &opt integer
カーソルを現在位置から integer カラム右へ移動させる。
integer の既定値は 1 。"
 :example
 "(defun fa () (prin1 12345)
                 (ansi$cursor-right 5) (prin1 'abc) (terpri))  -> fa
    fa -> 123345     abc")


(define
 "ansi$cursor-up"
 (expr nil)
 :documentation
 "形式 : ansi$cursor-up &opt integer
カーソルを現在位置から integer 行上へ移動させる。
integer の既定値は 1 。"
 :example
 "")


(define
 "ansi$enter-keypad-application-mode"
 (expr nil)
 :documentation
 "ユーザは下表で決められたスペシャルモードでのキーパット使用ができる。

        +-------+--------------------------------+
        |       |         meanings               |
        | keys  |-------------+------------------|
        |       | normal mode | application mode |
        |-------|-------------|------------------|
        |   ,   |      ,      |     ESC 0 l      |
        |   -   |      -      |     ESC 0 m      |
        |   .   |      .      |     ESC 0 n      |
        |   0   |      0      |     ESC 0 p      |
        |   1   |      1      |     ESC 0 q      |
        |   2   |      2      |     ESC 0 r      |
        |   3   |      3      |     ESC 0 s      |
        |   4   |      4      |     ESC 0 t      |
        |   5   |      5      |     ESC 0 u      |
        |   6   |      6      |     ESC 0 v      |
        |   7   |      7      |     ESC 0 w      |
        |   8   |      8      |     ESC 0 x      |
        |   9   |      9      |     ESC 0 y      |
        | ENTER | RETURN key  |     ESC 0 M      |
        |  PF1  |    ESC P    |     ESC 0 P      |
        |  PF2  |    ESC Q    |     ESC 0 Q      |
        |  PF3  |    ESC R    |     ESC 0 R      |
        |  PF4  |    ESC S    |     ESC 0 S      |
        +-------+-------------+------------------+"
 :example
 "")


(define "ansi$enter-keypad-numeric-mode" (expr nil) :documentation "ユーザはノーマルモードでのキーパット使用ができる。" :example "")


(define
 "ansi$erase-entire-line"
 (expr nil)
 :documentation
 "カーソルの現在カラム位置に関係なく、カーソルのある行の先頭から末尾
までを削除する。"
 :example
 "(defun fa () (prin1 12345) (ansi$erase-entire-line) 
                     (prin1 'abc) (terpri)) -> fa
        fa ->      abc")


(define "ansi$erase-entire-screen" (expr nil) :documentation "スクリーン上の全ての文字を削除する。" :example "")


(define
 "ansi$erase-from-tol"
 (expr nil)
 :documentation
 "現在カーソルのある行の先頭からカーソルの現在カラム位置までを削除
する。"
 :example
 "(defun fa () (prin1 12345) (ansi$erase-from-tol)
                     (prin1 'abc) (terpri)) -> fa
        fa ->      abc")


(define "ansi$erase-from-tos" (expr nil) :documentation "スクリーンの最上部から現在カーソル位置まで全ての文字を削除する。" :example "")


(define "ansi$erase-to-eol" (expr nil) :documentation "カーソルの現在カラム位置から行の末尾までを削除する。" :example "")


(define "ansi$erase-to-eos" (expr nil) :documentation "現在カーソル位置から画面の終わり (最下の右端) までを削除する。" :example "")


(define
 "ansi$file-out"
 (expr nil)
 :documentation
 "形式 : ansi$file-out &opt file
現在の画面全体を file (既定値は screen.txt) に書き出す。
関数 screen を起動した後にのみ有効。"
 :example
 "")


(define
 "ansi$form-feed"
 (expr nil)
 :documentation
 "改行する。ansi$lf と同じ。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$form-feed)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890
                        abc")


(define
 "ansi$get-character"
 (expr nil)
 :documentation
 "関数 screen が起動されていれば、現在カーソル位置の文字を返し、
そうでなければ、nil を返す。"
 :example
 "")


(define
 "ansi$get-line"
 (expr nil)
 :documentation
 "関数 screen が起動されていれば、現在カーソルのある行を文字列として
返し、そうでなければ、nil を返す。"
 :example
 "")


(define
 "ansi$index"
 (expr nil)
 :documentation
 "カーソルを 1 行下に移動させる。カーソルのある行が一番下なら、画面
を 1 行スクロールアップする。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$index)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890
                        abc")


(define "ansi$init" (expr nil) :documentation "ターミナルを初期化する。" :example "")


(define
 "ansi$lf"
 (expr nil)
 :documentation
 "改行する。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$lf)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890
                        abc")


(define
 "ansi$new-line"
 (expr nil)
 :documentation
 "カーソルを 1 行下に移動する。カーソルが一番下の行にあるなら、画面
を 1 行スクロールアップする。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$new-line)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890
              abc")


(define
 "ansi$reset-cursor-application-mode"
 (expr nil)
 :documentation
 "カーソルアプリケーションモードから抜ける。
ansi$set-cursor-application-mode 参照。"
 :example
 "")


(define
 "ansi$reset-to-normal-screen-mode"
 (expr nil)
 :documentation
 "反転画面モードから抜け、通常の画面モードにする。
ansi$set-to-reverse-screen-mode 参照。"
 :example
 "")


(define
 "ansi$restore-cursor"
 (expr nil)
 :documentation
 "カーソルをセーブしてあった位置に復帰させる。ansi$save-cursor 参照。"
 :example
 "(defun fa () (ansi$save-cursor) (prin1 1234567890)
                    (ansi$restore-cursor) (prin1 'abc) (terpri)) -> fa
        fa -> abc4567890")


(define
 "ansi$reverse-index"
 (expr nil)
 :documentation
 "カーソルを 1 行上に移動させる。カーソルが 1 番上の行にあるときは、
画面を 1 行スクロールダウンさせる。"
 :example
 "(defun fa () (terpri) (prin1 1234567890)
                     (ansi$reverse-index) (prin1 'abc) (terpri)) -> fa
        fa ->           abc
              1234567890")


(define "ansi$rubout" (expr nil) :documentation "カーソルを 1 文字戻し、カーソルの新しい位置の文字を削除する。" :example "")


(define
 "ansi$save-cursor"
 (expr nil)
 :documentation
 "現在のカーソルの状態をセーブする。"
 :example
 "(defun fa () (ansi$save-cursor) (prin1 1234567890)
                    (ansi$restore-cursor) (prin1 'abc) (terpri)) -> fa
       fa -> abc4567890")


(define "ansi$screen-height" (expr nil) :documentation "現在の画面の高さを返す。" :example "(ansi$screen-height) -> 24")


(define "ansi$screen-width" (expr nil) :documentation "現在の画面の幅を返す。" :example "(ansi$screen-width) -> 80")


(define
 "ansi$scroll-down"
 (expr nil)
 :documentation
 "形式 : ansi$scroll-down &opt integer
画面を integer 行スクロールダウンさせる。integer の既定値は、1 。"
 :example
 "")


(define
 "ansi$scroll-up"
 (expr nil)
 :documentation
 "形式 : ansi$scroll-up &opt integer
画面を integer 行スクロールアップさせる。 integer の既定値は、1 。"
 :example
 "")


(define
 "ansi$set-caution-type"
 (expr nil)
 :documentation
 "形式 : ansi$set-caution-type type
caution タイプ type を設定する。type には、:nop, :flush, :bell がある。
ansi$caution を参照。"
 :example
 "")


(define
 "ansi$set-character-attribute"
 (expr nil)
 :documentation
 "形式 : ansi$set-character-attribute &rest property
文字の属性 property を変更する。以降の文字はこの属性に従って表示される。
property には以下のものがある。属性の複数設定も可能。
  :under       下線        :bold        強調        :blink       点滅
  :reverse     反転        :normal      通常モード"
 :example
 "")


(define
 "ansi$set-cursor-application-mode"
 (expr nil)
 :documentation
 "カーソルの通常モードをやめ、カーソルのアプリケーションモードにする。
下表にエスケープシーケンスを示す。通常のキーボードには、up arrow、
down arrow、right arrow、left arrow、home arrow の 5 つの cursor key
がある。ansi$reset-cursor-application-mode 参照。
        +-------------+--------------------------------+
        |             |         meanings               |
        |    keys     |-------------+------------------|
        |             | normal mode | application mode |
        |-------------|-------------|------------------|
        | up arrow    |   ESC [ A   |     ESC 0 A      |
        | down arrow  |   ESC [ B   |     ESC 0 B      |
        | right arrow |   ESC [ C   |     ESC 0 C      |
        | left arrow  |   ESC [ D   |     ESC 0 D      |
        |    HOME     |   ESC [ H   |     ESC [ H      |
        +-------------+-------------+------------------+"
 :example
 "")


(define
 "ansi$set-cursor-attribute"
 (expr nil)
 :documentation
 "形式 : ansi$set-cursor-attribute &rest property
カーソルの属性 property を変更する。property には、以下のものがある。
  :visible :invisible    カーソルを表示するかしないか
  :under :block          アンダーラインかブロック形か
  :blink :nonblink       点滅するかしないか"
 :example
 "")


(define
 "ansi$set-line-attribute"
 (expr nil)
 :documentation
 "形式 : ansi$set-line-attribute &rest property
行の属性 property を変更する。property には以下のものがある。
  :ss (:single-width-single-height)
    カーソルがある行を普通の行として指定。
  :ds (:double-width-single-height)
    カーソルのある行を 2 倍幅の文字が表示される行として指定。
  :ddt (:double-width-double-height-top-half)
    2 倍幅 2 倍高の文字を表示する上半分の行としてカーソルのある行を指定。
  :ddb (:double-width-double-height-bottom-half)
    2 倍幅 2 倍高の文字を表示する下半分の行としてカーソルのある行を指定。"
 :example
 "")


(define
 "ansi$set-scroll-region"
 (expr nil)
 :documentation
 "形式 : ansi$set-scroll-region &opt integer1 integer2
画面のスクロール領域を設定する。integer1 行から integer2 行までが
スクロール領域になる。"
 :example
 "")


(define "ansi$set-tab-stop" (expr nil) :documentation "現在のカーソル位置よりタブを設定する。" :example "")


(define
 "ansi$set-to-reverse-screen-mode"
 (expr nil)
 :documentation
 "反転スクリーンモード (全ての文字が反転表示) にする。
ansi$reset-to-normal-screen-mode 参照。"
 :example
 "")


(define
 "ansi$special-graphics"
 (expr nil)
 :documentation
 "通常の小文字キーをたたくことによってグラフィック文字を表示可能に
する。このモードでユーザはターミナルに小文字を表示することはできない。
ansi$standard-character 参照。"
 :example
 "")


(define "ansi$standard-character" (expr nil) :documentation "グラフィックモードを中止し通常のモードにする。" :example "")


(define
 "ansi$tab"
 (expr nil)
 :documentation
 "カーソルを次のタブストップまで移動させる。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$tab)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890        abc")


(define
 "ansi$vertical-tab"
 (expr nil)
 :documentation
 "改行する。ansi$lf と同じ。"
 :example
 "(defun fa () (prin1 1234567890) (ansi$vertical-tab)
                     (prin1 'abc) (terpri)) -> fa
        fa -> 1234567890
        	        abc")


(define
 "append"
 #'append
 :documentation
 "形式 : append  &rest list1 list2 ... listN
list1 list2 ... listN を連結したリストを返す。concatenate nconc 参照。"
 :example
 "(append) -> nil
        (append '(1 2 3)) -> (1 2 3)
        (append '(1 2 3) '(4 5 6)) -> (1 2 3 4 5 6)
        (append '(1 2) '(3 4) '(5 6)) -> (1 2 3 4 5 6)
        (append '(a b) nil '(c d)) -> (a b c d)")


(define
 "append!"
 (macro nil)
 :documentation
 "形式 : append! list1 list2
list2 と list1 を連結したリストを list1 の値とする。
(append! list1 list2) = (!list1 (append list2 list1))"
 :example
 "(!x '(a b))
          (!y '(c d))
          (append! x y) -> (c d a b)
          x -> (c d a b) 
          y -> (c d)")


(define
 "append2"
 (subr nil)
 :documentation
 "形式 : append2 list1 list2
list1 と list2 を結合したリストを返す。
(append list1 list2) より速い。 [list1 .. list2] と同じ。"
 :example
 "(append2 '(1 2 3) '(4 5)) -> (1 2 3 4 5)
        ['(a b c) .. '(d e)] -> (a b c d e)")


(define
 "applobj"
 (class function)
 :documentation
 "インスタンスが、他のオブジェクトに適用することのできるオブジェクト。
関数そのものが applobj のインスタンス。"
 :example
 "")


(define
 "applobj-of"
 (subr (func)
   (typecase func
     (function func)
     (symbol (and (fboundp func)
                  (fdefinition func)))
     (otherwise nil)))
 :documentation
 "形式 : applobj-of func
関数オブジェクト func があればそれを返し、なければ nil を返す。"
 :example
 "(applobj-of 'member) -> {applobj}25431(#!subr-simple . 6)
        (applobj-of 'fkjdfd) -> nil
        (applobj-of {applobj}25431(#!subr-simple . 6))
            -> {applobj}25431(#!subr-simple . 6)")


(define
 "applobjp"
 (subr (obj)
   (typecase obj
     (function obj)
     (otherwise nil)))
 :documentation
 "形式 : applobjp func
関数 func が以下の 14 タイプの関数オブジェクトなら、func を返し、
それ以外なら nil を返す。

#!&+ applobj              #!&+dyn applobj
#!array applobj           #!closure applobj
#!expr applobj            #!exprdyn applobj
#!expr-simple applobj     #!exprdyn-simple applobj
#!hclauses applobj        #!macro applobj
#!subr applobj            #!subr-simple applobj
#!subst applobj           #!unit-clauses applobj"
 :example
 "(applobjp (lambda (x) (ncons x)))
                                    -> {applobj}12345(#!exprdyn . 6)
      (applobjp (&+ ((_x . _)) _x)) -> {applobj}54321(#!&+ . 6) 
      (applobjp 'car) -> nil")


(define
 "apply"
 (subr (func list)
   (apply func list))
 :documentation
 "形式 : apply func list
関数 func を list に適用する。"
 :example
 "(apply '+ (list 1 2 3 4 5 6 7 8 9)) -> 45
        (apply (lambda  (x y) (list y x)) (list 123 456)) -> (456 123)")


(define
 "common:apply"
 #'cl:apply
 :documentation
 "形式 : common:apply func arg1 &rest arg2 ... argN
引数 arg1 arg2 ... argN に関数 func を適用する。"
 :example
 "(common:apply '+ (list 1 2 3 4 5 6 7 8 9)) -> 45
        (common:apply 'max 1 2 '(3 4 5)) -> 5")


(define
 "apply*"
 ;;  3:35pm Monday, 6 August 2007
 (subr (func &rest args)
   (apply func args))
 :documentation
 "形式 : apply* func &rest arg1 arg2 ... argN
arg1 arg2 ... argN を評価した後、それらの値に関数 func を適用する。"
 :example
 "(apply* '+ 1 2 3 4 5 6 7 8 9) -> 45
        (apply* (lambda (x y) (list y x)) 123 456) -> (456 123)
        (apply* (lambda (x y) (list y x)) 'b 'a) -> (a b)")


(define
 "applyhook"
 (expr nil)
 :documentation
 "形式 : applyhook func1 list func2 func3 &opt env
変数 *evalhook* と *applyhook* を 関数 func2 と func3 にそれぞれ
バインドし、list のすべての要素を引数として func1 を呼び出し、func1 の
返す全値を返す。デバッグを助けるためのフック機能を使うことができる。"
 :example
 "")


(define
 "apropos"
 #'apropos
 :documentation
 "形式 : apropos string &opt pkg
パッケージ pkg において、印字名の中に string を副文字列として含む
シンボルをすべて検索し、それらのシンボル名を印字する。
pkg が省略されると、カレントパッケージにリンクされる全てのパッケージを
検索する。"
 :example
 "(apropos \"str\" sys:bas-package) は、次のものをプリントする。
        bas:*print-no-string-marker*
   	*print-string-marker
   	...
   	write-string write-to-string wstrhbo wstrhl")


(define
 "apropos-list"
 #'apropos-list
 :documentation
 "形式 : apropos-list string &opt pkg
パッケージ pkg において、印字名の中に string を副文字列として含む
シンボルをすべて検索し、それらのシンボル名のリストを返す。
pkg が省略されると、カレントパッケージにリンクされる全てのパッケージを
検索する。"
 :example
 "(apropos-list \"str\" sys:bas-package) 
        	-> (bas:*print-no-string-marker*
        	   *print-string-marker ...
        	   write-string write-to-string wstrhbo wstrhl)")


(define
 "aref"
 #'aref
 :documentation
 "形式 : aref array &rest data
配列 array の要素 data をアクセスし、その値を返す。"
 :example
 "(!a (make-array 10)) -> 
        	{vector}1791495(common:simple-general-vector . 10)
        (aref a 1) -> nil
        (!(aref a 1) '1) -> 1
        (aref a 1) -> 1")


(progn
  (setf (fdefinition '(setf tao:aref))
        (fdefinition '(setf aref)))
  (setf (documentation '(setf tao:aref) 'function)
        "aref                                   関数[#!subr]

<説明>
  形式 : aref array &rest data
配列 array の要素 data をアクセスし、その値を返す。

<例>
        (!a (make-array 10)) ->
        	{vector}1791495(common:simple-general-vector . 10)
        (aref a 1) -> nil
        (!(aref a 1) '1) -> 1
        (aref a 1) -> 1"))


(define
 "array"
 (expr (&rest dimensions)
   (make-array dimensions))
 :documentation
 "形式 : array &rest dimension1 dimension2 ... dimensionN
配列を作り、その関数オブジェクトが返る。代入の形で使用する。
配列の次元に制限はない。dimension1 dimension2 ... dimensionN が各次元の
大きさを指定し、引数の数 N が配列の次元になる。
各次元の大きさは、第 1 インデックス first と最終インデックス last 
のリスト (first last) としても指定できる。"
 :example
 "(!y (array 5 6)) -> {applobj}31182(#!array.10)
        (!z (array '(3 6) '(2 -2))) -> {applobj}31346(#!array.10)
        (!u (array #!8b-memblk '(1 10))) ->
              {applobj}31368(#!array . 8)
        y は 5 行 6 列の 2 次元配列で第 1 次元のインデックスは
        0 から 4 まで、第 2 次元のインデックスは 0 から 5 まで。
        z は 4 行 5 列の 2 次元配列で第 1 次元のインデックスは 
        3 から 6 まで、第 2 次元のインデックスは -2 から 2 まで。
        u は 1 次元配列で 1 から 10 までのインデックスをとり、その 1 
        ユニットは 8 ビット。")


(define
 "array-dimension"
 #'array-dimension
 :documentation
 "形式 : array-dimension array rank
配列 array において、次元 rank の大きさが返される。array が
フィルポインタを持つベクタであれば、そのベクタのトータルサイズが、
返される。"
 :example
 "(!b (make-array '(10 10))) -> {applobj}1287843(#!array .10)
        (array-dimension b 1) -> 10
        (array-dimension b 0) -> 10
        (array-dimension b 2) -> エラー")


(define
 "array-dimension-limit"
 (constant array-dimension-limit)
 :documentation
 "配列の各次元の値の上限 (その値は含まない) を表す正の整数。
array-dimension-limit = 8388608"
 :example
 "")


(define
 "array-dimensions"
 #'array-dimensions
 :documentation
 "形式 : array-dimensions array
配列 array の次元を要素とするリストが返される。"
 :example
 "(!b (make-array '(10 10))) -> {applobj}1287843(#!array .10)
        (array-dimensions b) -> (10 10)
        (!x (make-array 3)) -> 
                  {vector}1288293 (common:simple-generl-vector . 3)
        (array-dimensions x) -> 3")


(define
 "array-element-type"
 #'array-element-type
 :documentation
 "形式 : array-element-type array
配列 array に格納され得るオブジェクトのセットに対する型指定子が返される。"
 :example
 "(array-element-type (make-array 5 :element-type '(mod 5)))
          -> (mod 16)
        (array-element-type (make-array 3 :element-type 'bit)) -> bit")


(define
 "array-has-fill-pointer-p"
 #'array-has-fill-pointer-p
 :documentation
 "形式 : array-has-fill-pointer-p array
配列 array がフィルポインタを持っていれば t を返し、そうでなければ 
nil を返す。array が 1 次元でなければ常に nil を返す。"
 :example
 "(!x (make-array 3 :fill-pointer t)) -> 
        	{vector}77794(common:array . 3)
        (array-has-fill-pointer-p x) -> t
        (!y (make-array '(5 5)))
        	-> {applobj}70768(#!array . 10)
        (array-has-fill-pointer-p) -> nil")


(define
 "array-in-bounds-p"
 #'array-in-bounds-p
 :documentation
 "形式 : array-in-bounds-p array &rest integer1 integer2 ... integerN
添字 integer1 integer2 ... integerN がすべて、配列 array に対して正当で
あるかどうかをチェックする。すべて正当であれば t 、そうでなければnil 。
integer1 integer2 ... integerN の個数 N は、array のランクに等しくな
ければならない。フィルポインタを無視する。"
 :example
 "(!a (make-array 10)) -> 
        	{vector}1791495(common:simple-general-vector . 10)
        (array-in-bounds-p a 1) -> t")


(define
 "array-info"
 (expr nil)
 :documentation
 "形式 : array-info array
配列 array の情報を返す。array に配列を指定しなければ nil が返される。"
 :example
 "(!x (array '(10 20))) -> {applobj}31400(#!array . 8)
        (array-info x) -> (lisp-object (rank 1) (10 20))
        (!y (array 10 '(-1 3))) -> {applobj}31456(#!array . 10)
        (array-info y) -> (lisp-object (rank 2) (0 9) (-1 3))
        (!z (array #!8b-memblk 10)) -> {applobj}31468(#!array . 8)
        (array-info z) -> (#!8b-memblk (rank 1) (0 9))")


(define
 "array-rank"
 #'array-rank
 :documentation
 "形式 : array-rank array
配列 array の次元の数を返す。"
 :example
 "(!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
        (!b (make-array 2)) -> {vector}1288564
        			  (common:simple-general-vector .2)
        (array-rank a) -> 2
        (array-rank b) -> 1")


(define
 "array-rank-limit"
 (constant array-rank-limit)
 :documentation
 "配列の次元に関する上限 (その値を含まない) を表す正の整数。
array-rank-limit = 64"
 :example
 "")


(define
 "array-row-major-index"
 #'array-row-major-index
 :documentation
 "形式 : array-row-major-index array 
                              &rest integer1 integer2 ... integerN
配列 array について、添字 integer1 integer2 ... integerN が指し示す要素
を列順に識別している非負の整数を返す。"
 :example
 "(!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
        (!b (make-array 2)) -> 
              {vector}1288564(common:simple-general-vector .2)
        (array-row-major-index a 1 1) -> 4
        (array-row-major-index b 1) -> 1")


(define
 "array-total-size"
 #'array-total-size
 :documentation
 "形式 : array-total-size array
配列 array の要素の全個数を返す。ゼロ次元の配列の全体の大きさは 1 。
1 次元配列の全体の大きさは、フィルポインタに関係なく計算される。"
 :example
 "(!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
        (!b (make-array 2)) -> 
              {vector}1288564(common:simple-general-vector .2)
        (array-total-size a) -> 9
        (array-total-size b) -> 2")


(define
 "array-total-size-limit"
 (constant array-total-size-limit)
 :documentation
 "1 つの配列の中の要素の総数の上限 (その値を含まない) を表す正の整数。
array-total-size-limit = 8388608"
 :example
 "")


(define
 "array-type"
 (expr nil)
 :documentation
 "形式 : array-type array
配列 array のタイプを返す。配列のタイプは、ストリング、
simple-bit-vector、simple-vector、bit-vector、ベクタ、simple-bit-array、
simple-array、bit-array、配列 がある。"
 :example
 "(!a (make-array '(3 3)) -> {applobj}1288510(#!array . 10)
        (!b (make-array 2)) -> 
            {vector}1288564(common:simple-general-vector .2)
        (array-type a) -> (simple-array t)
        (array-type b) -> (simple-vector t)")


(define
 "arrayp"
 (expr (arg)
   (and (typep arg 'array) arg))
 :documentation ;; ??? arrayp <-> common:arrayp
 "形式 : common:arrayp arg
arg が配列なら関数オブジェクト、それ以外なら nil を返す。"
 :example
 "(!x (array '(2 2))) -> {applobj}1773079(#!array . 8)
        (common:arrayp x) -> {applobj}1773079(#!array . 8)")


(define
 "common:arrayp"
 #'cl:arrayp
 :documentation
 "形式 : arrayp arg
arg が配列なら t 、それ以外なら nil を返す。"
 :example
 "(!x (array '(10 20))) -> {applobj}31400(#!array . 8)
        (arrayp x) -> t")


(define
 "as-char"
 (expr (integer)
   (code-char integer))
 :documentation
 "形式 : as-char integer
十進数 integer を対応する文字に変換し、その結果を返す。"
 :example
 "(as-char 97) -> \"a\"
        (as-char 65) -> \"A\"")


(define
 "as-shortnum"
 (expr (arg)
   (typecase arg
     (character (char-code arg))
     (T (or #+sbcl (sb-kernel:get-lisp-obj-address arg)
            #+lispworks (sys:object-address arg)))))
 :documentation
 "形式 : as-shortnum arg
arg のアドレス部を shortnum と見なし、その値を返す。
Lisp オブジェクトの実アドレスと文字のコードを見ることができる。"
 :example
 "(as-shortnum \"a\") -> 97
        (as-shortnum 'a) -> 1115112")


(define
 "ash"
 #'ash
 :documentation
 "形式 : ash integer1 integer2
integer2 を評価し、その結果が正なら、integer1 をその数だけ左に
(負なら右に) ビットシフトし、その結果を返す。サインビットは変更しない。
左へのシフトで空白になったビット部分には 0 をつめる。右へのシフトで空白
になったビット部分にはサインビットの値をつめる。
元のオブジェクトは変更しない。"
 :example
 "(ash 4 1) -> #10
        (ash 10 4) -> #240
        (ash 123456 -5) -> #7422")


(define
 "ashift"
 (subr nil)
 :documentation
 "形式 : ashift integer1 integer2
integer2 の値を評価し、その結果が正なら、integer1 をその数だけ左に 
(負なら右に) ビットシフトし、その結果を返す。サインビットは変更しない。
左へのシフトで空白になったビット部分には 0 をつめる。右へのシフトで空白
になったビット部分にはサインビットの値をつめる。元のオブジェクトは変更
しない。ロカティブデータ型では ash と同じ。"
 :example
 "(ashift 4 1) -> #10
        (ashift 10 4) -> #240
        (ashift 123456 -5) -> #7422")


(define
 "ashift"
 (locative-operator nil)
 :documentation
 "形式 : loc1 ashift loc2
第 1 引数の算術シフトを行う。第 2 引数で左へシフトするビット数が指定
される。サインビットは常に、その値を保持してる。"
 :example
 "(signed-integer-locatives p q r s) -> (p q r s)
        (p <- #5252) -> 2730
        (q <- #7070) -> 3640
        (r <- #1111) -> 585
        (s <- (p ashift 3 )) -> 21840 (#52520)
        s -> 21840
        (s <- (p ashift -3)) -> 455 (#707)
        s -> 455
        (q <- (r ashift 2 and# q)) -> 2080 (#4040)
        q -> 2080")


(define
 "asin"
 #'asin
 :documentation
 "形式 : asin number
number の逆正弦 (arc sine) を返す。
number が 1 より大きい場合、複素数の値を返す。"
 :example
 "(asin 1.0f0) -> 1.5707963267949f0
        (asin 0.5f0) -> 0.523598775598299f0
        (asin -1.0f0) -> -1.5707963267949f0
        (asin 2.0f0) -> #c(1.570963267949f0 -1.31695789692481f0)")


(define
 "asinh"
 #'asinh
 :documentation
 "形式 : asinh number
number の逆双曲的正弦 (hyperbolic arc sine) を返す。
number に複素数指定可。"
 :example
 "(asinh -1.0f0) -> -0.881373587019543f0
        (asinh 0.5f0) -> 0.481211825059603f0
        (asinh 1.0f0) -> 0.881373587019543f0
        (asinh #c(2 3)) -> #c(1.98668702991653f0 0.5706527843211f0)")


(define
 "ass"
 (subr (pred data a-list)
   (assoc data a-list :test pred))
 :documentation
 "形式 : ass  pred data a-list
連想リスト a-list の要素を左から右へ順に調べていき、car 部が条件 
pred を満足する要素を見つけたらその要素を返し、後はもう調べない。なけ
れば nilを返す。pred は 2 引数をとる述語であり、その第 1 引数は data 
で、第 2 引数は a-list の各要素の car 部。
        (ass 'eq data a-list) = (assq data a-list)
        (ass 'equal data a-list) = (assqu data a-list)"
 :example
 "(ass '> 5 '((6 . six) (3 . three) (4 . four))) -> (3 . three)")


(defmacro tao.logic::dynamic-assert (type &rest clauses)
  (let ((pred (caar clauses)))
    `(progn
       (define-predicate-in-lisp-world ,pred)
       (tao.logic::prolog-compile 
        (tao.logic::add-clause ,(unquotify clauses)
                               :asserta ,(eq 'tao:asserta type)))
       ',pred)))


(defmacro define-predicate-in-lisp-world (name)
  `(defmacro ,name (&rest args)
     (let ((cont (gensym "cont")))
       `(with-return-from-reval ,cont (nil ,args)
          ,(tao.logic::compile-body
            `((,',name ,@args))
            `#',cont
            tao.logic::no-bindings)))))


(defmacro define-dynamic-predicate-in-lisp-world (name)
  `(setf (macro-function ',name)
         (lambda (&rest args)
           (let ((cont (gensym "cont")))
             `(with-return-from-reval ,cont (nil ,args)
                ,(tao.logic::compile-body
                  `((,',name ,@args))
                  `#',cont
                  tao.logic::no-bindings))))))


(defmacro define-method-predicate-in-lisp-world (name)
  `(defmacro ,name (obj &rest args)
     (let ((cont (gensym "cont")))
       `(with-return-from-reval ,cont (nil ,obj ,args)
          ,(tao.logic::compile-body
            `((,',name (tao:unquote ,obj) ,@args))
            `#',cont
            tao.logic::no-bindings)))))


(defun ensure-predicate-in-lisp-world (name)
  (eval `(define-predicate-in-lisp-world ,name)))


(define
 "assert"
 (macro (&rest clauses)
     (let ((pred (caar clauses)))
       `(progn
          (define-predicate-in-lisp-world ,pred)
          (tao.logic::prolog-compile 
           (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                                  :asserta nil))
          ',pred)))
 :documentation
 "形式 : assert &rest clause
ホーン節を定義する。この定理宣言と関連した関数の名前は、主ファンクタと
呼ばれる。同じ主ファンクタにおいて、複数の節を定義するために assert を
複数回使う時は、順番は保証されない。"
 :example
 "(assert (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
(assert (concatenate ()  _x _x) )
concatenate は、主ファンクタ。最初に定理宣言された節が最初に適用され、
2 番目に定理宣言された節が 2 番目に実行されるということは、保証されない。")


(define
 "asserta"
 (macro (&rest clauses)
     (let ((pred (caar clauses)))
       `(progn
          (define-predicate-in-lisp-world ,pred)
          (tao.logic::prolog-compile 
           (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                                  :asserta T))
          ',pred)))
 :documentation
 "形式 : asserta &rest clause
節の実行の順序が指定されるということ以外は、関数 assert と同じ。
後に言明された節ほど先に適用される。"
 :example
 "(asserta (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
(asserta (concatenate ()  _x _x) )
2 番目に言明された節 (concatenate () _x _x) が最初に適用され、
最初に言明された節 (concatenate (_a . _x) _y (_a . _z)) 
(concatenate _x _y _z) が 2 番目に適用される。")


(define
 "assertz"
 (macro (&rest clauses)
     (let ((pred (caar clauses)))
       `(progn
          (define-predicate-in-lisp-world ,pred)
          (tao.logic::prolog-compile 
           (tao.logic::add-clause ',(tao.logic::make-anonymous clauses)
                                  :asserta nil))
          ',pred)))
 :documentation
 "形式 : assertz &rest clause
節の実行の順序が指定される以外は、関数 assert と同じ。
先に言明された節ほど先に適用される。"
 :example
 "(assertz (concatenate (_a . _x) _y (_a . _z)) (concatenate _x _y _z) )
(assertz (concatenate ()  _x _x) )
最初に言明された節 (concatenate (_a . _x) _y (_a . _z))
(concatenate _x _y _z) が最初に適用され、2 番目に言明された節
(concatenate () _x _x) が 2 番目に適用される。")


(define
 "assign-cons"
 (subr (object list)
   (let ((unq-object (cadr object))
         (unq-list   (cadr list)))
     `(list 'setf ,@(mapcar #'(lambda (item)
                                `(quote ,item))
                            (cons unq-object unq-list)))))

 :documentation
 "形式 : assign-cons object list
list を object へ代入する代入式を作る。(!object list)"
 :example
 "(assign-cons 'x '(y)) -> (!x y)
        (assign-cons 'x '(1)) -> (!x 1)
        (eval (assign-cons 'y '('(1 2 3)))) -> (1 2 3)
        y -> (1 2 3)
        (!x (1 2 3))
        (eval (assign-cons '(car x) '(4))) -> 4
        x -> (4 2 3)")


(define
 "assign-list"
 (expr nil)
 :documentation
 "形式 : assign-list object1 object2
object2 を object1 へ代入する代入式を作る。(!object1 object2)"
 :example
 "(assign-list 'x 1) -> (!x 1)")


(define
 "assign-logical-name"
 (expr nil)
 :documentation
 "形式 : assign-logical-name logical physical &opt host globalp
フィジカル名 phygical を、ロジカル名 logical に代入する。logical の最終
キャラクタは : でなければならない。ホスト名 host の既定値は、
*default-pathname-defaults* による。host の値が \"tops::\" なら
DEC2060(Tops) におけるフィジカル名が代入される。代入は globalp の値が
nil なら、カレントプロセスに閉じたものとなる。globalp の既定値は nil 。"
 :example
 "(assign-logical-name \"n:\" 'cs:<nanja-monja>)
        (assign-logical-name \"mine:\" \"ps:<pin-pon-pan>\" \"tops::\")")


(define
 "assignee-cons"
 (expr (sym)
   (values (intern (concatenate 'string "!" (string sym)))))
 :documentation
 "形式 : assignee-cons object
object の先頭に ! を付けて返す。"
 :example
 "(assignee-cons 'x) -> !x")


(define
 "assigneep"
 (subr (form)
   (and (symbolp form)
        (string= "!"
                 (subseq (string form) 0 1))))
 :documentation
 "形式 : assigneep object
object が先頭に ! がついた表現になっていれば、それを返し、
そうでなければ nil を返す。"
 :example
 "(assigneep (caddr '(!!cons 1234 !x))) -> !x")


(define
 "assignp"
 (subr (form)
   (and (consp form)
        (eq 'setf (car form))))
 :documentation
 "形式 : assignp object
object が代入式ならば、それを返し、そうでなければ nil を返す。"
 :example
 "(assignp '(!x (x + 1))) -> (!x (x + 1))")


(define
 "assoc"
 #'assoc
 :documentation
 "形式 : assoc key a-list &key :test :test-not :key
連想リスト a-list 中で、key と一致するキーを持つ対のうち最初の対を返す。"
 :example
 "(assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
        -> (r . x)
        (assoc 'goo '((foo . bar) (zoo . goo))) -> nil
        (assoc '2 '((1 a b c) (2 b c d) (-7 x y z))) -> (2 b c d)")


(define
 "assoc-if"
 #'assoc-if
 :documentation
 "形式 : assoc-if pred a-list
連想リスト a-list 中で、その car 部が、述語 pred を満足する対のうち最初
の対を返す。なければ nil を返す。"
 :example
 "(assoc-if #'integerp '((ichi . one) (1 . 0) (2.9 . 3.8)))
        ->  (1 . 0)
        (assoc-if #'symbolp '((boku . i) (kimi . you) (1 . 3)))
        ->  (boku . i)")


(define
 "assoc-if-not"
 #'cl:assoc-if-not
 :documentation
 "形式 : assoc-if-not pred a-list
連想リスト a-list 中で、その car 部が、述語 pred を満足しない対のうち
最初の対を返す。なければ nil を返す。"
 :example
 "(assoc-if-not #'integerp '((ichi . one) (1 . 0)))
      	-> (ichi . one)
        (assoc-if-not #'symbolp '((boku . i) (kimi . you) (1 . 3))) 
        -> (1 . 3)")


(define
 "assq"
 (subr (key a-list)
   (assoc key a-list :test #'eq))
 :documentation
 "形式 : assq key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と eq な car 部
をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ nil を
返す。(assq key a-list) = (ass eq key a-list)"
 :example
 "(assq 'blue '((red . nil) (green . 2) (blue . 3)))
        -> (blue . 3)
        (assq 'white '((red . nil) (green . 2) (blue . 3))) -> nil
        (assq 'red  '((red . nil) (green . 2) (blue . 3))) -> (red)")


(define
 "assql"
 (subr (key a-list)
   (assoc key a-list :test #'eql))
 :documentation
 "形式 : assql key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と eql な car
部をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ 
nil を返す。(assql key a-list) = (ass eql key a-list)"
 :example
 "(assq 'blue '((red . nil) (green . 2) (blue . 3)))
        -> (blue  . 3)
        (assq 'this '((this . nil) (that . z) (there . y))) -> (this)")


(define
 "assqu"
 (subr (key a-list)
   (assoc key a-list :test #'equal))
 :documentation
 "形式 : assqu key a-list
連想リスト a-list の要素を左から右へ順に調べてゆき、key と equal な
car 部をもつ要素を見つけたらその要素を返し、後はもう調べない。なければ
nil を返す。(assqu key a-list) = (ass equal key a-list)"
 :example
 "(assqu 'blue '((red . nil) (green . 2) (blue . 3))) -> 
        	(blue . 3)
        (assqu '1 '((1.0 . 3) (5 . 2) (1 . 6))) -> (1 . 6)")


(define
 "atan"
 #'atan
 :documentation
 "形式 : atan number1 &opt number2
number1 を number2 で割った値の逆正接 (arc tan) をラジアン形式で返す。
number1 number2 は、複素数以外の数値。"
 :example
 "(atan 1.0f0) -> 0.785398163397449f0
        (atan 0.5f0) -> 0.463647609000807f0
        (atan -0.5f0) -> -0.46364760900087f0")


(define
 "atanh"
 #'atanh
 :documentation
 "形式 : atanh number
number の逆双曲的正接 (hyperbolic arc tan) を返す。
number の絶対値が 1 以上の場合、複素数を返す。"
 :example
 "(atanh -0.5f0) -> 0.54936144334054f0
        (atanh 0.5f) -> 0.549306144334054f0
        (atanh 0.8f0) -> 1.0986122886681f0")


(deftype tao:atom () 'cl:atom)


(define
 "atom"
 #'atom
 :documentation
 "形式 : atom object
object がアトムで、かつ次にあげるものの 1 つとして定義されているならば
t 、そうでなければ nil を返す。
    nil, id, logic variable (itself), codnum, integer,
    ratio number, real number, string, jstring, vector,
    locative, memblk, applobj, undef, udo
(atom x) = (not (listp x))  (x が nil 以外のとき)"
 :example
 "(atom nil) -> t
        (atom 'a) -> t
        (atom 123456789) -> t
        (atom '(a b)) -> nil
        (atom (car '(a b c))) -> t")


;;; *EOF*
