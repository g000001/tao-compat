;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)


(in-package #:tao-internal)



(define
 "octal-numberp"
 (subr nil)
 :documentation
 "形式 : octal-numberp number
number が先頭に # 記号を付与された 8 進数ならば、その値を返し、
それ以外なら nil を返す。"
 :example
 "(octal-numberp #1) -> #1
        (octal-numberp #1112222) -> #1112222
        (octal-numberp 1) -> nil")
(define
 "octnum"
 (expr nil)
 :documentation
 "形式 : octnum number
number を 8 進数に変換し、それを返す。"
 :example
 "(octnum 12) -> #14
        (octnum 2) -> #2")
(define
 "oddp"
 #'oddp
 :documentation
 "形式 : oddp integer
integer が奇数なら、その値を返し、それ以外なら nil を返す。
integer が整数でない場合、エラー。"
 :example
 "(oddp 2) -> nil
        (oddp 3) -> 3")
(define
 "off#"
 (locative-operator nil)
 :documentation
 "形式 : loc off# n
ロカティブ loc1 の n 番目のビットがクリアされる。"
 :example
 "(signed-integer-locatives p q r s) -> (p q r s)
        (p <- #5252) -> 2730
        (s <- (p off# 1 )) -> 2728 (#5250)
        (s <- (p off# 3 )) -> 2722 (#5242)
        (s <- (p off# 5 )) -> 2698 (#5212)")
(define
 "on#"
 (locative-operator nil)
 :documentation
 "形式 : loc on# n
ロカティブ loc の n 番目のビットがセットされる。"
 :example
 "(signed-integer-locatives p q r s) -> (p q r s)
        (p <- #5252) -> 2730
        (s <- (p on# 0 )) -> 2731 (#5253)
        (s <- (p on# 2 )) -> 2722 (#5256)
        (s <- (p on# 4 )) -> 27 (#5272)")
(define
 "open"
 #'open
 :documentation
 "形式 : open file &key :direction :element-type
                        :if-exists :if-does-not-exists
file をオープンし、そのファイルに結合されたストリームを返す。file は、
文字列、シンボル、パス名の udo、あるいはストリームの udo のいずれで
あってもよい。
キーワードは、以下のように、エラーを処理する方法や生成すべきストリーム
の種類等を指定する。

:direction  そのストリームが、入力、出力、のどれを扱うべきか指定。
  :input    入力。(既定値)
  :output   出力。
  :io       双方向。
  :probe    ファイルが存在するかどうかを決定。

:element-type  入出力単位を表すデータの型を指定。
  string-char   既定値。
  (unsigned-byte n)   単位は、大きさ n の符号無しのバイト(非負の整数)。
  unsigned-byte   単位は符号無しのバイト(非負の整数)。
  (signed-byte n)   単位は大きさ n の符号付きのバイト。
  signed-byte   単位は符号付きの 1 バイト。
  character   単位は任意の文字。
  bit   単位はビット (値0または1) 。
  (mod n)   単位は非負の n より小さい整数。

:default  型は関数 stream-element-type によって決定できる。

:if-exists
  :direction が :output あるいは :io で、file すでに存在していれば、
  とられるべきコードを指定。
  :input あるいは :probe であれば、:if-exists は無視される。
  :error   エラーを警告。これは、file のバージョン構文要素が :newest で
        ない場合の既定値。
  :new-version   同一のファイル名を持つ新しいファイルを生成。
    ただし、より大きいバージョン番号を持つ。
    file のバージョン構文要素が :newest である場合の既定値。
  :rename    現在あるファイルを他の名前に変更。
    その後、新しいファイルを指定された名前で生成。
  :rename-and-delete    すでにあるファイルを別の名前に変え、
    それを削除。その後、新しいファイルを指定された名前で生成。
  :overwrite   存在するファイルを使用。そして、そのストリーム上での出力
    操作は破壊的なファイルの修正。
    :direction が :io であれば、そのファイルは双方向モードで開かれる。
    ファイルポインタは、最初に、ファイルの先頭に位置付けられる。
  :append    現在存在するファイルを使用。そして、そのストリームへの出力
    操作は破壊的なファイルの修正。
    ファイルポインタは、最初に、そのファイルの最後に位置付けられる。
    :direction が :io であれば、そのファイルは双方向で開かれる。
  :supersede    古い名前と同一の名前のファイルを生成。
    より大きいバージョン番号を持つファイル名ではない。
    nil ファイルやストリームさえも作らない。単に nil を返す。

:if-does-not-exist
  file が、既に存在していない場合にとられる行動を指定。
  :error    エラーを警告。これは、:direction が :input あるいは、 
    :if-exists が、:overwrite あるいは :append である場合の既定値。
  :create    空ファイル file を生成。その後、file がすでに存在している
    かのように続行。
    (:if-exists によって指示されたすべての処理は実行しない) 
    :direction が :output あるいは :io であるか、:if-exists が
    :overwrite あるいは :append 以外のものである場合の既定値。
    nil ファイルあるいはストリームさえ作らない。単に nil を返す。
    :direction が :probe である場合の既定値。"
 :example
 "(!aa (open \"anpon.tan\" :direction :output)) 
        		-> {udo}51702file-stream
        (write \"ahondara\" aa) -> \"ahondara\"
        メッセージ \"ahondara\" がファイル \"anpon.tan\" に書き込まれる。
        (!bb (open \"test1.tao\" :direction :output :if-exists :append))
        	-> {udo}1827104file-stream
        (!cc (open \"test2.tao\" :direction :output :if-does-not-exist 
        	:create))
        	-> {udo}1825190file-stream")
(define
 "operation-handle-p"
 (message nil)
 :documentation
 "形式 : operation-handle-p message
operation-handle-p のレシーバは、message を受け取ることができるなら、
message のアドレスを返し、できなければ nil を返す。"
 :example
 "")
(define
 "or"
 (cl-macro cl:or)
 :documentation
 "形式 : or &rest form1 form2 ... formN
form1 form2 ... formN を順次評価し、nil 以外ならその値を返す。
nil でない値がなければ、or は nil を返す。"
 :example
 "(x が 5 か 7 の倍数なら)
        (or ((x mod 5) = 0) ((x mod 7) = 0)) -> 0 
        (x = 0 ならば)
        (or (x = 1) (x = 2) (x = 3) (x = 4)) -> nil
        (or (x = 7) (x = 8) (x = 9) (x = 0)) -> t")
(define
 "or#"
 (locative-operator nil)
 :documentation
 "形式 : loc1 or# loc2
loc1 と loc2  のビット or 操作を行う。"
 :example
 "(signed-integer-locatives p q r s) -> (p q r s)
        (p <- #5252) -> 2730
        (q <- #7070) -> 3640
        (r <- (p or# q )) -> 3770 (#7272)
        r -> 3770")
(define
 "otherwise"
 #'otherwise
 :documentation
 "selectq と caseq 関数において指定した条件が満足しない場合の動作を
指定する。"
 :example
 "(selectq 3
             ((1 2) 'foo)
             ((3 4) 'bar)
             (otherwise nil)) -> bar")
(define
 "output-stream-p"
 #'output-stream-p
 :documentation
 "形式 : output-stream-p stream
stream が出力操作を扱えるのであれば真、そうでなければ偽を返す。"
 :example
 "(!aa (open \"asd.tao\")) -> {udo}71499file-stream
        (bb (open \"qwe.tao\" :direction :output))
        			-> (udo}66087file-stream
        (outut-stream-p aa) -> ()
        (outut-stream-p bb) -> {udo}66087file-stream")
(define
 "own-file"
 (expr nil)
 :documentation
 "形式 : own-file unique-name new-name &opt old-fid
ELIS システムのファイル名 new-name を、fep システムのファイル名
unique-name に一致させる。
ファイルが fep システムで作られ、ELIS のファイルシステムに登録され
ていない時に使う。
ユーザは、ELIS システムにある new-name でファイルにアクセスすることが
可能になる。
fep システムは、バージョン管理機能がないので、この関数を適用された
ファイルは、常に出力のために :supersede モードでオープンされる。"
 :example
 "(own-file \"def\" \"<fak>kkk.ddd\")  
        ELIS システムの中で <fak>kkk.ddd という名前によって、
        fep システムの def という名のファイルをアクセスできるようにする。
        (own-file \"abc.tao\" \"<dir1>\") は、<dir>abc.tao を登録する。")


;;; *EOF*
