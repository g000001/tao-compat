(tao:tao)

(in-package #:tao-internal)

;;; ;;; ＠
;;; ;;; random                                 関数[#!expr]
;;; ;;;
;;; ;;; <説明>
;;; ;;;   形式 : random number &opt state
;;; ;;; 状態オブジェクト state をもとに、0 と number 間の乱数
;;; ;;; (number と同一形式の数)を発生し返す。
;;; ;;;
;;; ;;; <例>
;;; ;;;         (random 10) -> 3
;;; ;;;         (random 10.01) -> 5.95167
;;; ;;;         (random 5/12) -> 2/12
;;; ;;; ＠
;;; random-state                           クラス
;;;
;;; <説明>
;;;   乱数は、乱数発生機構により生成される。乱数発生機構に同じ状態が与え
;;; られれば、同じ乱数シーケンスは再生成できる。
;;; クラス random-state のインスタンスは、乱数発生機構の状態を示す。
;;; ジェネレータにより乱数が生成されると、乱数発生機構の状態は変化する。
;;; 乱数が生成されると、random-state のインスタンスは、それ自身を変化させる。
;;; インスタンスは、関数 make-random-state により生成され、コピーされる。
;;; 2 つの異なるインスタンスは、2 つの異なる乱数シーケンスを与える。
;;; ＠
;;; random-state-p                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : random-state-p object
;;; object が乱数オブジェクトであれば nil 以外の値、そうでなければ nil を
;;; 返す。
;;;
;;; <例>
;;;         (!a (make-random-state)) -> {udo}1192781random-state
;;;         (random-state-p a) -> {udo}1192781random-state
;;; ＠


(declaim (inline tao:rass))
(defun tao:rass (pred item a-list)
  "rass                                   関数[#!subr]

<説明>
  形式 : rass pred item a-list
連想リスト a-list 中に、第 2 要素が item と共に条件 pred を満足する
ペアがあれば、最初に発見したペアの値を返し、なければ、nil を返す。
       (rass 'eq item a-list) = (rassq item a-list)
       (rass 'equal item a-list) = (rassqu item a-list)

<例>
        (rass 'memq 'a '((1 . (q w e)) (2 . (a s d)) (3 . (z x c))))
              -> (2 a s d)"
  (rassoc item a-list :test pred))

;;; ＠
;;; rassoc                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : rassoc item a-list &key :test :test-not :key
;;; 連想リスト a-list 中に、第 2 要素が item に等しいペアがあったら、
;;; 最初に発見したペアの内容を返す。なければ、nil を返す。
;;; 関数 assoc の逆形式。
;;;
;;; <例>
;;;         (rassoc 'a '((a . b) (b . c) (c . a) (z . a)))
;;;               -> (c . a)
;;; ＠
;;; rassoc-if                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rassoc-if pred a-list
;;; 連想リスト a-list 中に、第 1 要素が、条件 pred を満足するペアがあれば、
;;; 最初のペアの値を返し、なければ nil を返す。
;;;
;;; <例>
;;;         (rassoc-if #'symbolp
;;;             '((1 . 3) (eagle . 2) (3 . washi) (taka . washi)))
;;;         	-> (3 . washi)
;;;         (rassoc-if #'floatp '((flower . hana) (5.9 . 6) (1 . 3)))
;;;         	-> nil
;;; ＠
;;; rassoc-if-not                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rassoc-if-not pred a-list
;;; 連想リスト a-list 中に、第 1 要素が、条件 pred を満足しないペアがあれば、
;;; 最初のペアの値を返す。全ての対が満足すれば nil を返す。
;;;
;;; <例>
;;;         (rassoc-if-not #'symbolp '((this . that) (kore . 1)
;;;         	(9.9 . are))) -> (this . that)
;;;         (rassoc-if-not #'floatp '((2.9 3.0) (apple . 8.9) (2 . 2.0)))
;;;               		-> (apple . 8.90002)
;;; ＠

(declaim (inline tao:rassq))
(defun tao:rassq (item a-list)
  "rassq                                  関数[#!subr]

<説明>
  形式 : rassq item a-list
連想リスト a-list 中に、第 2 要素が、item の値に eq なペアがあれば、
そのペアの値を返し、なければ nil を返す。item の値は、equalble でなけれ
ばならない。
  (rassq item a-list) = (rass eq item a-list)

<例>
        (rassq 2 '((red . nil) (green . 2) (blue . 3))) -> (green . 2)
        (rassq 34 '((john . 34) (tom . 34))) -> (john . 34)
        (rassq 5 '((red . nil) (green . 2) (blue . 3))) -> nil"
  (rassoc item a-list :test #'eq))

(declaim (inline tao:rassql))
(defun tao:rassql (item a-list)
  "rassql                                 関数[#!subr]

<説明>
  形式 : rassql item a-list
連想リスト a-list 中に、第 2 要素が、item の値に eql なペアがあれば、
そのペアの値を返し、なければ nil を返す。
  (rassql item a-list) = (rass eql item a-list)

<例>
        (rassql 'boku '((you . kimi) (boku . nil) (i . boku))) ->
        	(i . boku)
        (rassql 10 '((10 . 100) (ten . 10) (0 . zero))) -> (ten . 10)"
  (rassoc item a-list :test #'eql))

(declaim (inline tao:rassqu))
(defun tao:rassqu (item a-list)
  "rassqu                                 関数[#!subr]

<説明>
  形式 : rassqu item a-list
連想リスト a-list 中に、第 2 要素が、item の値に equal なペアが
あれば、そのペアの値を返し、なければ nil を返す。
  (rassqu item alist) = (rass equal item alist)

<例>
        (rassqu 'a '((10 . a) (a . nil))) -> (10 . a)
        (rassqu 'your '((i . my) (you . your))) -> (you . your)"
  (rassoc item a-list :test #'equal))

;;; ＠
;;; ratio                                  クラス
;;;
;;; <説明>
;;;  2/3, 123/123456, -65872/1235 のように整数を整数で割ったもの。
;;; ＠
;;; rational                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rational number
;;; 複素数以外の数 number を有理数に変換し、その結果を返す。
;;; 浮動小数点数が完全に正確であることを前提とする。
;;;
;;; <例>
;;;         (rational 0.37) -> 193987/524288
;;;         (rational -0.37) -> 193987/524288
;;;         (rational 2.3) -> 150733/65536
;;; ＠
;;; rationalize                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rationalize number
;;; 複素数以外の数 number を有理数に変換し、その結果を返す。
;;; 浮動小数点数の精度だけが完全に正確であることを前提とする。
;;;
;;; <例>
;;;         (rationalize -0.37) -> -37/100
;;;         (rationalize 0.37) -> 37/100
;;;         (rationalize 2) -> 2
;;; ＠
;;; rationalp                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : rationalp number
;;; number が有理数なら、number を返し、それ以外なら nil を返す。
;;;   (rationalp x) = (or (integerp x) (ratiop x))
;;;
;;; <例>
;;;         (rationalp 4) -> 4
;;;         (rationalp 2/3) -> 2/3
;;;         (rationalp 4.5) -> nil
;;; ＠
;;; ratiop                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : ratiop number
;;; number が分数なら、number 返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (ratiop 1.23) -> nil
;;;         (ratiop 123) -> nil
;;;         (ratiop 12/3) -> nil  (12/3 を評価すると 4)
;;;         (ratiop 2/3) -> 2/3
;;;         (ratiop 4/7) -> 4/7
;;;         (ratiop 5) -> nil
;;;         (ratiop 3.6) -> nil
;;;         (ratiop 216/60) -> 18/5
;;; ＠
;;; read                                   関数[#!exprdyn]
;;;
;;; <説明>
;;;   形式 : read &opt stream
;;; stream から s 式を読み込む。stream に読むものがなければ、:eof を返す。
;;; stream が省略されると、変数 *standard-input* の値、(通常、コンソール
;;; ターミナル) が指定されたものと見なす。
;;; パッケージ指定を除いては、関数 vread と同じ。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1175483file-stream
;;;         (read aa) -> qwe
;;;         (read aa) -> :eof

(declaim (inline tao:read))
(defun tao:read (&optional (stream *standard-input*))
  (cl:read stream nil :eof))

;;; common:read                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:read &opt input-stream eof-error-p
;;;                           eof-value recursive-p
;;; input-stream (既定値は変数 *standard-input* の値) からオブジェクトの
;;; 印字表現を読み込み、対応するオブジェクトを作成し、そのオブジェクトを
;;; 返す。eof-error-p が t (既定値) であれば、ファイルの終わりではエラー
;;; を警告する。nil であれば、エラーを警告しないで、eof-value の値を返す。
;;; recursive-p が nil 以外の場合、トップレベルの呼び出しではなく、埋め込ま
;;; れた呼び出しであることを指定している。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1173637file-stream
;;;         (common:read aa nil 'owari) -> qwe
;;;         (common:read aa nil 'owari) -> owari
;;;         (common:read aa t 'owari) -> eof-encountered
;;; ＠
;;; read-byte                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : read-byte stream
;;; stream から 1 バイトを読み込み、それを ASCII コードとして返す。
;;; stream に読むものがなければ、0 (ゼロ) を返す。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1172432file-stream
;;;         (read-byte aa) -> 113  ("q")
;;;         (read-byte aa) -> 119  ("w")
;;;         (read-byte aa) -> 101  ("e")
;;;         (read-byte aa) -> 0    (:eof)
;;; ＠
;;; read-char                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : read-char &opt stream
;;; stream から 1 字を読み込み、それを文字として返す。
;;; stream の既定値は、変数 *standard-input* の値 (通常はコンソール
;;; ターミナル) 。
;;; stream に読まれる文字がなければ、null-string を返す。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1172432file-stream
;;;         (read-char aa) -> "q"
;;;         (read-char aa) -> "w"
;;;         (read-char aa) -> "e"
;;;         (read-char aa) -> #¥space
;;;         (read-char aa) -> "N"
;;;         (read-char aa) -> ""
;;; ＠
;;; read-char-no-hang                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : read-char-no-hang &opt input-stream
;;; input-stream (既定値は変数 *standard-output* の値) のバッファに文字が
;;; あれば、1 文字を読み、文字として返す。stream のバッファに 1 文字もな
;;; ければ、即座に待つことなしに nil を返す。ファイルの終わりでは、
;;; null-string を返す。
;;;   (read-char-no-hang s) = (tyi-no-hang s)
;;;
;;; <例>
;;;         (read-char-no-hang) -> nil
;;;         ファイル asd.tao に "qwe N" が書かれている
;;;         (!aa (open "asd.tao")) -> {udo}1172432file-stream
;;;         (read-char-no-hang aa) -> "q"
;;;         (read-char-no-hang aa) -> "w"
;;;         (read-char-no-hang aa) -> "e"
;;;         (read-char-no-hang aa) -> #space
;;;         (read-char-no-hang aa) -> "N"
;;;         (read-char-no-hang aa) -> ""
;;; ＠
;;; read-delimited-list                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : read-delimited-list char &opt input-stream recursive-p
;;; input-stream から、空白文字や注釈を無視して、文字 char の前まで
;;; オブジェクトを読み込み、その結果を返す。
;;; ＠
;;; read-from-string                       関数[#!subr]
;;;
;;; <説明>
;;;   形式 : read-from-string string &opt package
;;; リターン値は、2 つ。最初に返される値は、string の第 1 要素の s 式。
;;; 2 番目に返される値は、最初に返されなかった、残りの文字。
;;;
;;; <例>
;;;         (read-from-string "(a b)") -> !((a b) "")
;;;         (read-from-string "nil") -> !(nil "")
;;;         (read-from-string "1234") -> !(1234 "")
;;;         (read-from-string "abc def ghi") -> !(abc " def ghi")
;;;         (read-from-string "(a b) c d") -> !((a b) " c d")
;;;         (read-from-string "") -> !(:eof "")
;;;         If x = "(a b) 123", なら
;;;         (multiple-value-setq (obj1 res1) (read-from-string x)) ->
;;;                   (a b)
;;;         ここで obj1 = (a b) で res1 = " 123"
;;; ＠
;;; common:read-from-string                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:read-from-string string &opt eof-error-p eof-value
;;;                             &key :start :end :preserve-white-space
;;;   string が、連続して Lisp リーダに与えられる。そして Lisp オブジェクト
;;; が、リーダによって組み立てられ、それが返される。この関数は、2 値を返す。
;;; 最初の値は、読まれたオブジェクトであり、2 番目の値は、読まれていない
;;; ストリングの中の最初の文字の添字。
;;; eof-error-p が真 (既定値) であればファイルの終わりではエラーが警告
;;; される。偽であればエラーは警告されないで、eof-value の値が返される。
;;;
;;; &key
;;; :start :end  :start で指定された文字で始まり :end で指定された文字
;;;   まで (:end で指定された文字自身は含まない) が、string の副文字列
;;;   として読み込み対象となる。。
;;;   :start の既定値は 0、:end の既定値はフォーム (length string) の評価値。
;;; :preserve-white-space    この値が、nil でなければ、この関数操作は空白を
;;;   保つ。既定値は nil 。
;;;
;;; <例>
;;;         (common:read-from-string "abcd adc") -> !(abcd 4)
;;;         (common:read-from-string "abcd adc" nil 'owari
;;;         	:start 2 :end 5) -> !(cd 4)
;;; ＠
;;; read-line                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : read-line &opt input-stream
;;; input-stream から、改行によって終了された行を読み、その行を文字列として
;;; 返す。改行文字は含まない。
;;; input-stream が省略されると変数 *standard-input* の値が使われる。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}80905file-stream
;;;         (read-line aa) -> "qwwertyuiop asdfghjkl"
;;;         (read-line aa) -> "1234567890"
;;;         (read-line aa) -> :eof
;;; ＠
;;; common:read-line                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:read-line &opt input-stream eof-error-p
;;;                                eof-value recursive-p
;;; input-stream から、改行によって終了された行を読み、その行を文字列として
;;; 返す。改行文字は含まない。
;;; input-stream が省略されると変数 *standard-input* の値が使われる。
;;; eof-error-p が真 (既定値) であればファイルの終わりではエラーが警告
;;; される。偽であればエラーは警告されないで、eof-valu の値が返される。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1211244file-stream
;;;         (common:read-line aa nil 'owari) -> "qwwertyuiop asdfghjkl"
;;;         (common:read-line aa nil 'owari) -> owari
;;;         (common:read-line aa nil) -> :eof
;;;         (common:read-line aa) -> eof-encountered
;;; ＠
;;; read-preserving-whitespace             関数[#!subst]
;;;
;;; <説明>
;;;   形式 : read-preserving-whitespace &rest 'x
;;; ストリームから文字列を読むとき、区切り文字としての空白 (シンボルに続く
;;; 空白) を捨てないで読み込む。それに対して、関数 read は、区切り文字が
;;; 空白であれば、それを捨てる。
;;; ＠


(defun tao:readl (&optional (stream *standard-input*) (package *package*))
  "readl                                  関数[#!subr]

<説明>
  形式 : readl &opt stream package
stream から 1 行を読み、読んだ行の要素で構成されるリストを返す。
stream に読むものがなければ、:eof を返す。
stream が省略されると、変数 *standard-input* の値 (通常、コンソール
ターミナル) が指定されたものと見なす。
package が省略された場合、*package* の値 (通常カレントパッケージ)
を使う。関数 readline と同じ使い方をする。

<例>
        次の様に { } で囲まれた文字をタイプすると,
        {(readl)} {crlf}
        {a b c d (a b) ((sd nil)) nil} {crlf}
        返される値として
        (a b c d (a b) ((sd nil)) nil) がコンソールターミナル上に
        表示される.
        {(readl)} {crlf} {crlf} とタイプすると結果は、() である."
  (declare (ignore package))		;どう対処するのか分からない
  (let ((line (read-line stream nil :eof)))
    (with-input-from-string (str line)
      (do ((s (cl:read str nil :eof) (cl:read str nil :eof))
	   res)
	  ((eql s :eof) (nreverse res))
	(push s res)))))

(declaim (inline tao:readline))
(defun tao:readline (&optional (stream *standard-input*) (package *package*))
  "readline                               関数[#!subr]

<説明>
  形式 : readline &opt stream package
stream から 1 行を読み、読んだ行の要素で構成されるリストを返す。
stream が省略されると、変数 *standard-input* の値 (通常コンソール
ターミナル) が指定されたと見なされる。
package が省略されると、変数 *package* の値が使われる。
stream に読むものがなければ、:eof を返す。

<例>
        次の様に { } で囲まれた文字をタイプすると,
        {(readline)} {crlf}
        {a b c d (a b) ((sd nil)) nil} {crlf}
        返される値として
        (a b c d (a b) ((sd nil)) nil) がコンソールターミナル上に
        表示される.
        {(readline)} {crlf} {crlf} とタイプすると結果は、() である."
  (tao:readl stream package))

(declaim (inline tao:readline-to-string))
(defun tao:readline-to-string (&optional stream)
  "readline-to-string                     関数[#!subr]

<説明>
  形式 : readline-to-string &opt stream
stream から 1 行を読み、読んだ行にある文字で構成される文字列を返す。
\(最後の crlf は、含まれない)。
stream を省略すると、変数 *standard-input* の値 (通常コンソール
ターミナル) が指定されたと見なされる。
stream に読まれるものがなければ :eof を返す。

<例>
        次の様に { } で囲まれた文字をタイプすると
        {(readline-to-string)} {crlf}
        {a b c d (a b) ((sd nil)) nil} {crlf}
        返される値
        \"a b c d (a b) ((sd nil)) nil\" が、コンソールターミナル上に
        表示される."
  (read-line stream nil :eof nil))

#|(with-input-from-string (str "a b c d (a b) ((sd nil)) nil")
  (readline-to-string str))|#

(declaim (inline tao:reads))
(defun tao:reads (&optional stream)
  "reads                                  関数[#!subr]

<説明>
  形式 : reads &opt stream
関数 readline-to-string と同じ働きをする。
stream から 1 行分の文字列を読み、それを返す(最後の crlf は、含まない)。
stream を省略すると、変数 *standard-input* の値 (通常コンソールターミナ
ル) が指定されたと見なされる。
stream に読まれるものがなければ :eof を返す。

<例>
        次の様に { } で囲まれた文字をタイプすると
        {(reads)} {crlf}
        {a b c d (a b) ((sd nil)) nil} {crlf}
        返される値
        \"a b c d (a b) ((sd nil)) nil\" が、コンソールターミナル上に
        表示される."
  (tao:readline-to-string stream))

;;; ＠
;;; readtablep                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : readtablep object
;;; object が読み込み表ならば t 、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (readtablep *readtable*) -> t
;;; ＠
;;; real                                   クラス
;;;
;;; <説明>
;;;   インスタンスは 1.2, 123.45, -908.3 のように float も big-float も可。
;;; short-float (25 ビット表現) は、先頭ビットは符号ビット、次の 7 ビット
;;; は符号ビットを含めた指数部、最後の 17 ビットは仮数部として用いられる。
;;; k = (1.0 + m) * (n-th power of 2) -64 <= n <= 63
;;; この式で、1.0 は落とされた最左端の数字をリカバーするためのものである。
;;; 10 進基数の式では k は 10 の約 19 乗に達し、およそ 5 桁の有効数字を持つ。
;;; 浮動小数 (64 ビット表現) は、先頭ビットは符号ビット、次の 11 ビットは
;;; 符号ビットを含む指数部、最後の 52 ビットは仮数部として用いられる。
;;;   k = (1.0 + m) * (n-th power of 2)
;;;         -1024 <= n <= 1023
;;; 10 進基数では、k は 10 の約 308 乗になり、約 16 桁の有効数字を持つ。
;;; 任意の浮動小数点数は bigfloat によって表される。それは任意の桁数の
;;; 有効数字を持ちうる。bigfloat number はリストとして内部的に表される。
;;; ＠
;;; realp                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : realp real
;;; real が実数なら、real を返し、それ以外なら nil を返す。
;;;   (realp x) = (or (floatp x) (bigfloatp x))
;;;
;;; <例>
;;;         (realp 7.9) -> 7.9
;;;         (realp 10) -> nil
;;;         (realp 5/7) -> nil
;;; ＠
;;; realpart                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : realpart number
;;; number の型が複素数の場合は、指定された数の実部を、
;;; 浮動小数点数の場合は、同一形式の浮動小数点数の 0 を、
;;; その他の場合は、number の値を返す。
;;;
;;; <例>
;;;         (realpart 0) -> 0
;;;         (realpart -1) -> -1
;;;         (realpart #c(2 0.3)) -> 2
;;; ＠
;;; receive-mail                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : receive-mail mailbox
;;; mailbox に送られてくるメイルを受け取る。受け取りの待ち時間に制限はなく、
;;; プロセスはメイルを受け取るまで待つ。
;;;
;;; <例>
;;;         (!m-box (make-instance 'mailbox)) -> {udo}128768mailbox
;;;         (send-mail m-box "abcdefg") -> "abcdefg"
;;;         (send-mail m-box '(p q r)) -> (p q r)
;;;         (receive-mail m-box) -> "abcdefg"
;;;         (receive-mail m-box) -> (p q r)
;;; ＠
;;; receive-mail-with-timeout              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : receive-mail-with-timeout n mailbox
;;; mailbox に送られてくるメイルを受け取る。
;;; ただし、待ち時間は
;;; (n * (1/internal-time-units-per-second)) 秒で時間切れとなる。
;;; 時間切れ前にプロセスがメイルを受け取れば、そのメイルを返す。
;;; さもなければ nil を返す。internal-time-units-per-second は、60 。
;;;
;;; <例>
;;;         (!m-box (make-instance 'mailbox)) -> {udo}128768mailbox
;;;         (send-mail m-box "abcdefg") -> "abcdefg"
;;;         (receive-mail m-box) -> "abcdefg"
;;;         (receive-mail-with-timeout 300 m-box) -> nil （5秒後）
;;; ＠
;;; recover-sstatus                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : recover-sstatus saved &opt terno
;;; 関数 save-sstatus によってセーブされた saved をターミナル状態に設定する。
;;; ターミナル状態とはキーボード上のキーの代入式を意味する。
;;;     (save-sstatus 参照 )
;;;
;;; <例>
;;; (!aa (save-sstatus))
;;; -> ((#0 #1 #2 #3 #4 #5 #6 #7 #8 #9 #10 #11 #12 #13 #14 #15 #16 #17
;;;       #18 #19 #20 #21 #22 #23 #24 #25 #26 #27 #28 #29 #30 #31 #32
;;;       #33 #34 #35 #36 #37 #38 #39 #40 #41 #42 #43 #44 #45 #46 #47
;;;       #48 #49 #50 #51 #52 #53 #54 #55 #56 #57 #58 #59 #60 #61 #62
;;;       #63 #64 #65 #66 #67 #68 #69 #70 #71 #72 #73 #74 #75 #76 #77
;;;       #78 #79 #80 #81 #82 #83 #84 #85 #86 #87 #88 #89 #90 #91 #92
;;;       #93 #94 #95 #96 #97 #98 #99 #100 #101 #102 #103 #104 #105 #106
;;;       #106 #107 #108 #109 #110 #111 #112 #113 #114 #115 #116 #117
;;;       #118 #119 #120 #121 #122 #123 #124 #125 #126 #127 #128 #129
;;;       #130 #131 #132 #133 #134 #135 #136 #137 #138 #139 #140 #141
;;;       #142 #143 #144 #145 #146 #147 #148 #149 #150 #151 #152 #153
;;;       #154 #155 #156 #157 #158 #159 #160 #161 #162 #163 #164 #165
;;;       #166 #167 #168 #169 #170 #171 #172 #173 #174 #175 #176 #177)
;;;    {udo}1594854login ((28 abort 2)))
;;;    (recover-sstatus aa 6) -> ok
;;; ＠
;;; reduce                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : reduce test seq &key :from-end :start :end :initial-value
;;; 関数 test を利用して、シーケンス seq の :start と :end で指定された
;;; 間の要素の演算を行い、結果を返す。
;;;
;;; <例>
;;;         (reduce #'+ '(1 2 3 4) -> 10
;;;         (reduce #'- '(1 2 3 4) :from-end t) -> -2
;;;         (reduce #'+ '(foo)) -> foo
;;;         (reduce #'list '(1 2 3 4) -> (((1 2) 3) 4)
;;;         (roduce #'list '(1 2 3 4) :initial-value 'foo)
;;;         			-> ((((foo 1) 2) 3) 4)
;;; ＠
;;; register-global-package                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : register-global-package name size &rest files
;;; files の関数及びデータを、データサイズが size で名前が name の
;;; 大域パッケージに登録する。この関数を使うときには指定されるファイルは
;;; すべてロードされなければならない。
;;; size はこの大域パッケージで作られるシンボルの個数の 1/2 より大きく、
;;; また、2 のべき乗で表される数値でなくてはならない。
;;; files (file1, file2 ,...) は bs:<tools>fedit のようにディレクトリシス
;;; テムのファイル名である。
;;;
;;; <例>
;;;         パッケージ "edi" を最新のものにするためには次のようにする。
;;;         (register-global-package "edi" 32
;;;         			"bs:<tools>zen" "bs:<tools>zendsp")
;;; ＠

(declaim (inline tao:rem))
(defun tao:rem (pred object list &optional n)
  "tao:rem                                関数[#!macro]

<説明>
  形式 : rem pred object list &opt n
list の要素を順に、object とともに条件 pred に適用し、満足する要素を
取り除く。pred は、引数を 2 つとる関数。その第 1 引数は、object で、
第 2 引数は、list の各要素。
取り除く要素の個数は、n 。n が、省略されたり、条件を満足する要素の個数
以上の数の場合、あるいは負の場合は、条件を満足するすべての要素を取り
除く。取り除かれて小さくなったリストを返す。
list は、破壊されない。関数 del 参照。

<例>
        x = (1 2 3 4 5 6 7)  なら
        (tao:rem < 4 x) -> (1 2 3 4)  で
        x = (1 2 3 4 5 6 7)
        (tao:rem < 4 x 2) -> (1 2 3 4 7)  そして
        x = (1 2 3 4 5 6 7)"
  (remove-if (lambda (x) (funcall pred object x)) list :count n))

#|(mc:my x '(1 2 3 4 5 6 7)
  (rem #'< 4 x 2))|#

(declaim (inline tao:rem-if))
(defun tao:rem-if (pred list)
  "rem-if                                 関数[#!macro]

<説明>
  形式 : rem-if pred list
list から、条件 pred を満足する要素を削除し、その結果を返す。
\(list は破壊されない)。pred は引数を 1 つしかとらない。
すなわち、list の各要素。関数 del-if 参照。

<例>
        x = (1 a 2 b 3 c 4 d 5)  とすると
        (rem-if integerp x) -> (a b c d)  そして
        x = (1 a 2 b 3 c 4 d 5)"
  (remove-if pred list))

(declaim (inline tao:rem-if-not))
(defun tao:rem-if-not (pred list)
  "rem-if-not                             関数[#!macro]

<説明>
  形式 : rem-if-not pred list
list から、 条件 pred を満足しない要素をすべて削除し、その結果を返す。
\(list は破壊されない)。pred は、引数を 1 つしかとらない。
関数 del-if-not 参照。
<例>
        x = (1 a 2 b 3 c 4 d 5)  なら
        (rem-if-not integerp x) -> (1 2 3 4 5)  そして
        x = (1 a 2 b 3 c 4 d 5)"
  (remove-if-not pred list))

;;; ＠
;;; remainder                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : remainder number1 number2
;;; number1 を、number2 で割ったときの剰余を返す。
;;; (remainder x y) = (mod x y)。
;;;
;;; <例>
;;;         (remainder 5 3) -> 2
;;;         (remainder 10 2) -> 0
;;; ＠
;;; remf                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : remf place ind
;;; 属性リスト place に、ind と eq なインディケータがあれば、その
;;; インディケータ と対応する属性値のペアを破壊的に削除し、t を返し、
;;; なければ、nil を返す。第 1 引数の仕様とリターン値を除いて remprop
;;; と同じ。
;;;
;;; <例>
;;;         (!(plist 'xxx) '(p 1 q 2 r 3)) -> (p 1 q 2 r 3)
;;;         (remf (plist 'xxx) 'q) -> t
;;;         (plist 'xxx) -> (p 1 r 3)
;;;         (remf (plist 'xxx) 's) -> nil
;;;         (remf (plist 'xxx) 'p) -> t
;;;         (plist 'xxx) -> (r 3)
;;;         (remf (plist 'xxx) 'r) -> t
;;;         (plist 'xxx) -> nil
;;; ＠
;;; remhash                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : remhash key table
;;; ハッシュ表 table 中に key に対応するエントリがあればそのエントリを
;;; 削除し、t を返す。なければ、nil を返す。
;;;
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}81749(hash-table . 8)
;;;         (!(gethash 'color a) red) -> red
;;;         (remhash 'red a) -> nil
;;;         (remhash 'color a) -> 0
;;;         (remhash 'color a) -> nil
;;; ＠
;;; remove                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : remove item seq &key :from-end :test :test-not
;;;                                    :start :end :count :key
;;; シーケンス seq の :start から :end までの範囲内の要素をコピーし、
;;; seq 内の item を :count 個削除し、その結果を返す。
;;;
;;; <例>
;;;         (x '(1 2 4 1 3 4 5))
;;;         (remove 4 x) -> (1 2 1 3 5)
;;;         (remove 4 x :count 1) -> (1 2 1 3 4 5)
;;;         (remove 4 x :count 1 :from-end t) -> (1 2 4 1 3 5)
;;;         (remove 3 x :test #'>) -> (4 3 4 5)
;;; ＠
;;; remove-duplicates                      関数[#!macro]
;;;
;;; <説明>
;;;   形式 : remove-duplicates seq &key :from-end :test :test-not
;;;                                     :start :end :key
;;; シーケンス seq の :start から :end までの要素のうち重複する要素を前方
;;; から (:from-end が nil 以外なら後方から) 最後の要素を残し、取り除きそ
;;; の結果を返す。
;;;
;;; <例>
;;;         (!x '(a b c b d d e)) -> (a b c b d d e)
;;;         (remove-duplicates x) -> (a c b d e)
;;;         x -> (a b c b d d e)
;;;         (remove-duplicates '((foo #¥a) (bar #¥%) (baz #¥A))
;;;              		:test #'char-equal :key #'car)
;;;                 -> ((foo "a") (bar "%") (baz "A"))
;;; ＠
;;; remove-if                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : remove-if test seq &key :from-end :start :end :count :key
;;; シーケンス seq の :start から :end までの範囲内で、条件 test を満足す
;;; る要素を :count 個だけ削除し、その結果を返す。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5))
;;;         (remove-if #'oddp x) -> (2 4 4)
;;;         (remove-if #'evenp x :count 1 :from-end t) -> (1 2 4 1 3 5)
;;; ＠
;;; remove-if-not                          関数[#!macro]
;;;
;;; <説明>
;;;   形式 : remove-if-not test seq &key :from-end :start :end :count :key
;;; シーケンス seq の :start から :end までの範囲内で、条件 test を満足し
;;; ない要素を :count 個だけ削除し、その結果を返す。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5))
;;;         (remove-if-not #'oddp x) -> (1 1 3 5)
;;;         (remove-if-not #'evenp x :lount 1 :from-end) -> (1 2 4 1 3 4)
;;; ＠
;;; remplist                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : remplist p-list ind
;;; 属性リスト p-list から、ind と eq なインディケータを持つ属性 (ペア)
;;; を削除して、その変更された属性リストを返す。p-list に ind と eq な
;;; インディケータがなければ、p-list をそのまま返す。常に破壊的というわけ
;;; ではないので、代入と共に用いる。
;;;
;;; <例>
;;;         (!(plist 'xxx) '(p 1 q 2 r 3)) -> (p 1 q 2 r 3)
;;;         (remplist (plist 'xxx) 'q) -> (p 1 r 3)
;;;         (plist 'xxx) -> (p 1 r 3)
;;;         (remplist (plist 'xxx) 'p) -> (r 3)
;;;         (plist 'xxx) -> (p 1 r 3)
;;; ＠
;;; remprop                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : remprop p-list ind
;;; 属性リスト p-list が、ind と eq なインディケータを持つ、属性 (ペア)
;;; を破壊的に削除し、その結果 (変更された属性リストの内容) を返す。
;;; インディケータがなければ、元の p-list の内容をそのまま返す。
;;;
;;; <例>
;;;         (!(plist 'xxx) '(p 1 q 2 r 3)) -> (p 1 q 2 r 3)
;;;         (remprop 'xxx 'q) -> (p 1 r 3)
;;;         (plist 'xxx) -> (p 1 r 3)
;;;         (remprop 'xxx 's) -> (p 1 r 3)
;;;         (remprop 'xxx 'p) -> (r 3)
;;;         (plist 'xxx) -> (r 3)
;;;         (remprop 'xxx 'r) -> nil
;;; ＠
;;; common:remprop                         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:remprop symbol ind
;;; symbol が指定する属性リストから、ind に eq なインディケータを持つ属性
;;; (ペア) を削除する。
;;;
;;; 例
;;;         (!(plist 'xxx) '(color blue height 6 weight 35))
;;;         		    -> (color blue height 6 weight 35)
;;;         (common:remprop 'xxx 'color) -> t
;;;         (plist 'xxx) -> (height 6 weight 35)


(declaim (inline tao:remq))
(defun tao:remq (item list &optional n)
  "remq                                   関数[#!macro]

<説明>
  形式 : remq item list &opt n
list から、item の値と eq な要素を、n 個だけ削除し、その結果のリストを
返す (元のリストは破壊されない)。
n が省略された時、または n が負もしくは item と eq な要素の個数以上の
ときは、条件を満足するすべての要素をリストから取り除く。
関数 tao:rem において引数 pred で関数 eq を指定したものと同じ。
\(remq item list n) = (tao:rem eq item list n)

<例>
        x = (a b c d e f)  とすると
        (remq 'b x ) -> (a c d e f)  で
        x = (a b c d e f)
        (remq 'b '(x b y b z b) 2) -> (x y z b)
        (remq 'b '(x b y b z b) 10) -> (x y z)
        (remq 'b '(x b y b z b) -2) -> (x y z)"
  (remove item list :count n :test #'eq))

(declaim (inline tao:remql))
(defun tao:remql (item list &optional n)
  "remql                                  関数[#!macro]

<説明>
  形式 : remql item list &opt n
list から、item の値と eql な要素を、n 個だけ削除し、その結果のリスト
を返す (元のリストは破壊されない)。
n が省略された時、または n が負もしくは item と eql な要素の個数以上の
ときは、条件を満足するすべての要素をリストから取り除く。
関数 tao:rem において引数 pred に関数 eql を指定した場合と同じ。
\(remql x y) = (tao:rem eql x y)

<例>
        (remql 2 '(1 2 3)) -> (1 3)"
  (remove item list :count n))

(declaim (inline tao:remqu))
(defun tao:remqu (item list &optional n)
  "remqu                                  関数[#!macro]

<説明>
  形式 : remqu item list &opt n
list から、item の値と equal な要素を、n 個だけ削除し、その結果のリスト
を返す (元のリストは破壊されない)。
n が省略された時、または n が負もしくは item と equal な要素の個数以上
のときは、条件を満足するすべての要素をリストから取り除く。
関数 tao:rem において引数 pred に関数 equal を指定した場合と同じ。

<例>
        (remqu 2 '(1 2 3)) -> (1 3)"
  (remove item list :count n :test #'equal))

;;; ＠
;;; rename-file                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rename-file file new-name
;;; file の名前を、new-name に変更する。
;;; file は、文字列、パス名の udo、あるいはストリームの udo をとる。
;;; もしそれがファイルと結合されたオープンされたストリームであれば、
;;; そのストリーム自身とそれに関連するファイルが影響を受ける。
;;; 変更に成功すれば、3 つの値を返す。
;;; 1 番目の値は、new-name 。
;;; 2 番目の値は、名前が変更される前のファイル名。
;;; 3 番目の値は、変更された後のファイル名。
;;; もし名前変更操作がうまくいかなければ、エラーが警告される。
;;;
;;; <例>
;;;         (rename-file "anc.tao" "cba.tao") ->
;;;         !("Ho::bs:<dire>abc.tao" "Ho::bs:<dire>abc.tao"
;;;           "Ho::bs:<dire>cba.tao")
;;; ＠
;;; rename-package                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rename-package package new-name &optn new-nicknames
;;; package の古い名前と全てのニックネームを取り除き、new-name と
;;; new-nicknames で置き換える。
;;;
;;; <例>
;;;         (!a (make-package "abc")) -> {vector}76620(package . 12)
;;;         (package-name (rename-package a "def")) -> "def"
;;; ＠
;;; replace                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : replace seq1 seq2 &key :start1 :end1 :start2 :end2
;;; シーケンス seq1 の :start1 から :end1 までの内容を、seq2 の:start2
;;; から :end2 までの内容に置き換え、その結果を返す。seq1 の型 と seq2
;;; の型は同一でなければならない。:start1 :start2 の既定値は 0、
;;; :end1 :end2 の既定値は各シーケンスの長さ。
;;;
;;; <例>
;;;         (!x '(1 2 3 4 5)) -> (1 2 3 4 5)
;;;         (!y '(a b c d e f g )) -> (a b c d e f g)
;;;         (replace x y :start1 3 :end1 5 :start2 1 :end2 3) ->
;;;                 (1 2 3 b c)
;;;         x -> (1 2 3 b c), y -> (a b c d e f g)
;;;         (!x "asdfg")
;;;         (!y "qwert")
;;;         (replace x y :start1 2 :end1 4) -> "asqwg"
;;;         x -> "asqwg", y -> "qwert
;;; ＠
;;; reply                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : reply line-num
;;; 関数 talk のための応答ルーチンを呼ぶ。関数 talk により talk された
;;; ユーザは、それに答えるために 関数 reply を使う。
;;; ＠
;;; require                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : require name &optn pathname
;;; 大文字と小文字を区別した比較によって既に名前が name のモジュールが存
;;; 在しているかどうか調べる。
;;;
;;; <例>
;;;         (provide 'abc) -> ("abc")
;;;         (require 'abc) -> nil
;;;         (require 'xyz) -> エラー
;;; ＠
;;; reset-io                               関数[#!expr]
;;;
;;; <説明>
;;;   入出力をリセットする。
;;;
;;; <例>
;;;         (reset-io) -> ok
;;; ＠
;;; rest                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : rest list
;;; list の最初の要素を削除し、その結果のリストを返す。
;;;
;;; <例>
;;;         (rest '(1 2 3)) -> (2 3)
;;;         (rest 1 )       ->  ()
;;; ＠
;;; tao:rest1                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : tao:rest1 list
;;; list の最初の要素を削除し、その結果のリストを返す。
;;;
;;; <例>
;;;         (tao:rest1 ()) -> ()
;;;         (tao:rest1 '(1 2 3)) -> (2 3)
;;; ＠
;;; tao:rest2                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : tao:rest2 list
;;; list の最初から 2 個要素を削除し、その結果のリストを返す。
;;;
;;; <例>
;;;         (tao:rest2 '(1 2 3)) -> (3)
;;;         (tao:rest2 ())       -> ()
;;; ＠
;;; tao:rest3                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : tao:rest3 list
;;; list の最初から 3 個要素を削除し、その結果のリストを返す。
;;;
;;; <例>
;;;         (tao:rest3 '(1 2 3 4)) -> (4)
;;;         (tao:rest3 '(1 2 3))   -> ()
;;;         (tao:rest3 ())         -> ()
;;; ＠
;;; tao:rest4                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : tao:rest4 list
;;; list の最初から 4 個要素を削除し、その結果のリストを返す。
;;;
;;; <例>
;;;         (tao:rest4 '(1 2 3 4 5)) -> (5)
;;;         (tao:rest4 '(1 2 3 4))   -> ()
;;;         (tao:rest4 ())           -> ()
;;; ＠
;;; retrace                                関数[#macro]
;;;
;;; <説明>
;;;   形式 : retrace &opt flag
;;; 変数 *untraced-fns* の値となっている関数を再び変数 *traced-fns* の値
;;; にし、すべて再びトレースされるべき関数とする。*untraced-fns* の値は、
;;; nil となる。*untraced-fns* の初めの値を返す。flag は、副関数を指定する。
;;; flag が nil(既定値) なら、現在のトレースのためにキーワードパラメータ
;;; の前の値が、nil でなければ、キーワードパラメータの既定値が使われる。
;;;
;;; <例>
;;;         (defun fact (x)
;;;                (cond ((= x 0) 1)
;;;         	     (t (* x (fact (1- x)))))) -> fact
;;;         (trace fact) -> (fact)
;;;         (untrace) -> (fact)
;;;         (fact 3) -> 6
;;;         (retrace ) -> (fact)
;;;         (fact 3) -> 1 <IN > (fact [x = 3])
;;;         	    .2 <IN > (fact [x = 2])
;;;         	    . 3 <IN > (fact [x = 1])
;;;         	    . .4 <IN > (fact [x = 0])
;;;         	    . .4 <OUT> (fact 1)
;;;         	    . 3 <OUT> (fact 1)
;;;            	    .2 <OUT> (fact 2)
;;;         	    1 <OUT> (fact 6)
;;;         	    6

(defun *retract (clause)
  (etypecase clause
    (SYMBOL
     (mapc (lambda (c)
             (tao.logic::retract-clause c)
             (let ((head (car c)))
               (fmakunbound (print (tao.logic::make-predicate (car head) (length (cdr head)))))))
           (tao.logic::get-clauses clause))
     (tao.logic::prolog-compile clause)
     (fmakunbound clause)
     T)
    (CONS
     (let ((pred (car clause)))
       (dolist (c (tao.logic::get-clauses (car clause)))
         (when (or (equal (car c) clause)
                   (null clause))
           (tao.logic::retract-clause c)))
       (tao.logic::prolog-compile pred)
       T))))

(defmacro tao:retract (&optional clause)
  "retract 未インプリメント               関数[#!exprdyn]

 <説明>
   形式 : retract 'id
 id を主ファンクタとして、関数 assert, 関数 asserta, 関数 assertz で定義
 した定理を除去する。id に関する全定理の除去だけが現在サポートされている。

 <例>
         (retract concatenate)
         主ファンクタが concatenate の定理を全て除去する。
         (retract (concatenate () _x _x))
  	(concatenate () _x _x) に関する定理を除去する。"
  `(*retract ',clause))

;;; return                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : return &opt val 'exit-id
;;; prog, do, do*, block のいずれかから強制的に脱出する関数。
;;; exit-id なしの prog 、name の略された block からはこの関数で脱出する。
;;; val があれば val を返し、それ以外なら {undef}0 を返す。
;;; 関数 return-from 参照。
;;;
;;; <例>
;;;         (defmacro return (value) '(return-from nil ,value))
;;;         (de my-member (item list)
;;;             (prog (more)
;;;         	  (!more list)
;;;         	 loop
;;;         	  (cond ((null more)
;;;         		 (return nil))
;;;         		(eql item (car more))
;;;         	 	 (return more)))
;;;         	  (!!cdr !more)
;;;         	  (go loop)))
;;; ＠
;;; return-from                            関数[#!subr]
;;;
;;; <説明>
;;;   形式 : return-from name value
;;; prog, do-named, do*-named, block いずれかから強制的に脱出させる。
;;; exit-id を使った prog、と name を持った block からはこの関数で脱出する。
;;; value がある時は value を返し、それ以外なら nil を返す。
;;; 関数 return 参照。
;;;
;;; <例>
;;;         (do-named owari ((x 1 (+ x 1)))
;;;          		((x > 100))
;;;         		(!y (+ (x ** 3) (x ** 2) x 1))
;;;         		(cond ((y <= 200) (write y))
;;;         		      (t (return-from owari 'end))))
;;;         	-> 4
;;;         	  15
;;;         	  40
;;;         	  85
;;;         	  156
;;;          	  end
;;; ＠
;;; revappend                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : revappend list1 list2
;;; list1 の要素を逆順に並べた新たなリストと list2 を連結 (append) し、
;;; そのリストを返す (list1 はコピーされ破壊されない)。
;;;   (revappend x y) = (append (reverse x) y)
;;;
;;; <例>
;;;         (revappend '(a (b) c) '(1 2))  ->  (c (b) a 1 2)
;;;         (revappend '(a (b) c) '())  ->  (c (b) a)
;;; ＠
;;; common:reverse                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:reverse seq
;;; シーケンス seq の要素を逆順に並びかえ、その結果を返す。
;;; seq は、TAO ではリストでなければならないが、Common Lisp では、列でも
;;; よい (TAO では引数がリストでなかったら、nil を返す)。
;;; 関数 common:nreverse,reverse 参照。
;;;
;;; <例>
;;;         (!p '(a b c)) -> (a b c)
;;;         (!q (common:reverse p)) -> (c b a)
;;;         q -> (c b a)
;;;         p -> (a b c)
;;; ＠
;;; reverse                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : reverse list
;;; list の要素を逆順に並べなおしたリストを返す (list は破壊されない)。
;;;
;;; <例>
;;;         list x = (you did so)  のとき
;;;         (reverse x) -> (so did you)
;;;         x = (you did so)
;;;         list y = ((a b) (c d)) のとき
;;;         (reverse y) = ((c d) (a b))
;;;         y = ((a b) (c d))
;;;         (reverse nil) -> nil
;;; ＠
;;; room               未インプリメント    関数[#expr]
;;;
;;; <説明>
;;;   形式 : room &opt x
;;; 内部の記憶域の状態と、その管理の状態についての情報を、変数
;;; *standard-output* のストリームにプリントする。(room nil) は最小限の量
;;; の値の情報をプリントする。(room t) は最大限の情報をプリントする。
;;; (room) は中間的な量の情報をプリントする。
;;; ＠
;;; rotatef                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : rotatef &rest obj1 obj2 ... objN
;;; 各値を右から左へシフトし、obj1 の値は objN にシフトされる。つまり、
;;; objN <- obj1, obj1 <- obj2, obj2 <- obj3, ...となる。nil を返す。
;;;
;;; <例>
;;;         a = 1 ,b = 2 ,c = 3
;;;         (rotatef a b c) -> nil
;;;         a = 2, b = 3, c = 1
;;;         (!x '(1 2 3 4)) -> (1 2 3 4)
;;;         (rotatef (car x) (cadr x) (caddr x) (cadddr)) -> nil
;;;         x -> (2 3 4 1)
;;; ＠
;;; round                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : round number1 &opt number2
;;; number1 の値を number2 の値で割った結果の値の小数点以下を四捨五入した
;;; 整数 (2 つの整数のまん中の場合は、偶数側) に変換して返す。
;;; (number2 が省略された場合、number1 の値をそのまま返す。)
;;;
;;; <例>
;;;         (round 2.3 0.4) -> !(6 -0.0999947)
;;;         (round 2.3f0 3.7) -> !(1 -1.39999694824219f0)
;;; ＠
;;; rplaca                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : rplaca list item
;;; list の第一要素 (car 部) を、item に置き換え、その結果のリストを返す。
;;; (元のリストは破壊される)。(replaca x y) の代わりに (!(car x)  y) を使
;;; うとよい。
;;;
;;; <例>
;;;         x = (a b c) の時,
;;;         (rplaca x 1234) -> (1234 b c) を実行すると,
;;;         x は (1234 b c) になる。
;;; ＠
;;; rplacd                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : rplacd list item
;;; list の第 1 要素を除いてできるリスト(cdr 部)を、item に置き換え、
;;; その結果のリストを返す (元のリストは破壊される)。(rplacd x y) の代わ
;;; りに (!(cdr x)  y) を使うとよい。
;;;
;;; <例>
;;;          x = (a b c)、y = (s o b i) の時、
;;;         (rplacd x y) を実行すると (a s o b i) が返され,
;;;          x は (a s o b i) になる。
;;; ＠
;;; rtname                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : rtname pathname
;;; ELISシステムのファイル名 pathname に対応する fep (front-end-processor)
;;; システムのユニークファイル名 (q124.438 等) を返す。
;;;
;;; <例>
;;;         (rtname "qwe.tao") -> "dm1:q26.721"
;;; ＠
