(in-package #:tao-compat)

;;; #### CL
;;; machine-instance                       関数[#!expr]
;;; 
;;; <説明>
;;;   Common Lisp が走行するコンピュータハードウェアの特定の機械を識別する
;;; 文字列を返す。ELIS システムでは、ホスト名が返る。
;;; 
;;; <例>
;;;         "MIT-MC"
;;;         "CMU GP-VAX"

;;; #### CL
;;; machine-type                           関数[#!expr]
;;; 
;;; <説明>
;;;   Common Lisp が走行するコンピュータハードウェアの形式を識別する
;;; 文字列を返す。ELIS システムでは、"ELIS" が返る。
;;; 
;;; <例>
;;;         "ELIS"
;;;         "DEC PDP-10"
;;;         "DEC VAX-11/780"
;;; ＠
;;; machine-version                        関数[#!expr]
;;; 
;;; <説明>
;;;   Common Lisp が実行されるコンピュータハードウェアのバージョンを識別
;;; する文字列を返す。ELIS システムでは、"ELIS-TTL" が返る。
;;; 
;;; <例>
;;;         "ELIS-TTL"
;;;         "KL 10, microcode 9"
;;; ＠
;;; macro-function                         関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : macro-function symbol
;;; symbol がグローバルマクロ定義を持つかをチェック。
;;; ローカル関数定義を持つなら nil を返す。
;;; グローバル関数定義でもあるマクロ定義を持つなら、マクロに特有な展開さ
;;; れた関数を返す。展開された関数はその引数としてマクロ式をとり、その
;;; マクロの展開された式を返す。
;;; 
;;; <例>
;;;         (defmacro f (x) ｀(car ,x)) -> f
;;;         (applobj-of 'f) -> {applobj}68184(#!macro . 6)
;;;         (macro-function 'f) -> {applobj}67330(#!exptdyn . 6)
;;;         (macroexpand '(f '(10 20 30))) -> !((car '(10 20 30)) t)
;;;         (funcall (macro-function 'f) '(f '(10 20 30)))
;;;         	                       -> (car '(10 20 30))
;;;         (eval (funcall (macro-function 'f) '(f '(10 20 30)))) -> 10
;;; ＠
;;; macroexpand                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : macroexpand form
;;; form を可能な限りマクロ展開を繰り返し、その展開結果を第 1 の値、t を
;;; 第 2 の値として返す。
;;; form がマクロ式でないと form を第 1 の値、nil を第 2 の値として返す。
;;; form の car 部がマクロでないと展開をストップする。
;;; 
;;; <例>
;;;         (macroexpand '(setq a 2 b 3)) -> !((seq (!a 2) (!b 3)) t)
;;;         (defmacro f (x) ｀(cdr ,x)) -> f
;;;         (defmacro g (y) ｀(car (f ,y))) -> g
;;;         (defmacro h (z) ｀(f (car ,z))) -> h
;;;         (macroexpand '(g '(1 2 3))) -> !((car (f '(1 2 3))) t)
;;;         (macroexpand '(h '((1 2) (3 4))))
;;;         			    -> !((cdr (car '((1 2) (3 4)))) t)
;;;         (defun f0 (x) (car x)) -> f0
;;;         (macroexpand '(f0 '(1 2 3))) -> !((f0 '(1 2 3)) nil)
;;; ＠
;;; macroexpand-1                          関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : macroexpand-1 form
;;; form を 一度だけマクロ展開し、その展開結果を第 1 の値、t を第 2 の値
;;; として返す。 form がマクロでなければ form を第 1 の値、nil を第 2 の値
;;; として返す。
;;; 
;;; <例>
;;;         (macroexpand-1 '(setq a 2)) -> !((!a 2) t)
;;;         (defmacro fm (x) ｀(car ,x)) -> fm
;;;         (defmacro gm (y) ｀(fm (cdr ,y))) -> gm
;;;         (macroexpand-1 '(gm '(1 2 3))) -> !((fm (cdr '(1 2 3))) t)
;;;         (defun h (x) (car x)) -> h
;;;         (macroexpand-1 '(f '(1 2 3))) -> !((f '(1 2 3)) nil)
;;; ＠
;;; macrolet                               関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : macrolet ((name1 (arg11 arg12 ...) body1)
;;;                    (name2 (arg21 arg22 ...) body2)
;;;         	   ...)
;;;                   form1 form2 ... formN
;;; name1 name2 ... を名前、、arg11 arg12 ...、arg21 arg22 ...、を引数、
;;; body1 body2 ... を本体とするローカルマクロを定義し、form1 form2 ...
;;; formN を評価し、最後のフォーム formN の評価結果を返す。これらのフォーム
;;; が全く省略された時は nil を返す。
;;; body1 body2 ... で定義されたマクロは、form1 form2 ... formN で用いる
;;; ことができる。
;;; form1 form2 ... formN 内では、変数は、この関数の外のローカル変数を参照
;;; できるが、body1 body2 ... では外のローカル変数を参照することはできない。
;;; macrolet がローカルマクロ用であることを除いて flet と同じ。
;;; 
;;; <例>
;;;         (prog (p) (!p '(1 2 3 4))
;;;         	  (macrolet ((my-car (x) ｀(car ,x))
;;;         	  	     (my-cdr (y) ｀(cdr ,y)))
;;;         		     (cons (my-car p) (my-cdr p))))
;;;         		     -> (1 2 3 4)
;;;         (prog (p) (!p '(1 2 3 4))
;;;         	  (macrolet ((my-car (x) ｀(car ,x))
;;;         	  	     (my-cdr (y) ｀(cdr ,y))  (write p)))
;;;         		     (cons (my-car p) (my-cdr p))))
;;;         		     -> (unbound-variable p)
;;; ＠
;;; mailbox                                クラス
;;; 
;;; <説明>
;;;   インスタンスは異なるプロセス間のコミニュケーションのための
;;; プリミティブなオブジェクト。インスタンスでは、インスタンス変数
;;; :name  sys:mailbox-process-queue  sys:mail-queue を指定できる。
;;; インスタンスでは、メイルの待ち行列の長さに明示的な限界はない。
;;; ＠
;;; make-array                             関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-array dimensions
;;;         	 &key :element-type :initial-element :initial-contents
;;;                       :adjustable   :fill-pointer    :displaced-to
;;;                       :displaced-index-offset
;;; 配列を作るための基本的な関数。dimensions で指定した非負の整数のリスト
;;; が次元となる配列を作る。そのリストの長さが配列の次元数となる。
;;; 各次元の値は、定数 array-dimension-limit の値より小さくなければならない。
;;; すべての次元の積は、定数 array-total-size-limit の値より小さくなければ
;;; ならない。
;;; dimensions で nil を指定すれば、ゼロ次元の配列が作られる。
;;; 
;;; &key
;;; :element-type    配列の要素の型の名前。
;;; :initial-element    配列の各要素の初期化。この値は、:element-type で
;;;   指定された型でなければならない。
;;; :initial-contents    配列の内容の初期化。値は列のネストされた構造。
;;; :adjustable    nil 以外に指定されると、配列は生成された後に動的にその
;;;   大きさを変えられることができる。既定値は、nil 。
;;; :fill-pointer    配列がフィルポインタを持つことを指定。既定値は nil 。
;;; :displaced-to    nil 以外が指定されると、配列は共有 (displace) された
;;;   配列であることを示す。既定値は、nil 。
;;; displaced-index-offset    :displaced-to と共に関連してだけ用いる。
;;;   作りだされた共有される配列のインデックスオフセットとなる。
;;;   この値は非負の整数でなければならず、既定値は 0。
;;; :initial-element  :initial-contents  :displaced-to は、それぞれ単独
;;;   でしか用いることができない。他のどれかと共に用いてはいけない。
;;; 
;;; <例>
;;;         (make-array 5)  1 次元配列
;;;         (make-array '(3 4) :element-type '(mod 16))
;;;            4 ビットの要素を持つ 3*4 の 2 次元配列
;;;         (make-array 5 :element-type 'single-float)
;;;            single-float を持つ配列
;;;         (make-array '(4 2 3) :initial-contents
;;;          '(((a b c) (1 2 3)) ((d e f) (3 1 2))
;;;            ((g h i) (2 3 1)) ((j k l) (0 0 0)))  配列の内容を初期化
;;;         (setq a (make-array '(4 3)))
;;;         (setq b (make-array  8 :displaced-to a 
;;;         	:displaced-index-offset 2)))  共有配列
;;;         ここで
;;;         (aref b 0) = (aref a 0 2) (aref b 1) = (aref a 1 0)
;;;         (aref b 2) = (aref a 1 1) (aref b 3) = (aref a 1 2)
;;;         (aref b 4) = (aref a 2 0) (aref b 5) = (aref a 2 1)
;;;         (aref b 6) = (aref a 2 2) (aref b 7) = (aref a 3 0)
;;; ＠
;;; make-broadcast-stream                  関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-broadcast-stream stream1 stream2 ... streamN
;;; 出力方向でだけ働くブロードキャストストリームを生成し、返す。
;;; このストリームへ送られるすべての出力は、stream1 stream2 ... streamN の
;;; すべてに送られる。
;;; ストリーム操作によって返される結果は、streamN の上での操作の実行から
;;; 生じる値。引数が与えられていなければ、結果のストリームへのすべての
;;; 出力は捨てられる。
;;; ＠
;;; make-char                              関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : make-char char &opt bit-attr font-attr
;;; 文字型 char に対応するコード属性で、bit-attr のビット属性か、または 
;;; font-attr のフォント属性と一致する文字型のオブジェクトが生成できれば、
;;; そのオブジェクトを返し、生成できなければ nil を返す。
;;; ＠
;;; make-concatenated-stream               関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-concatenated-stream stream1 stream2 ... streamN
;;; 入力方向でのみ働くストリームを生成し、返す。
;;; 入力は、stream1 から取られ、end-of-file に到達すると、入力は、stream2
;;; に切り替わり、以下、同じように取られる。
;;; ストリームが与えられなければ、内容を持たないストリームを生成し、
;;; すべての入力は end-of-file となる。
;;; ＠
;;; make-dispatch-macro-character          関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-dispatch-macro-character char
;;;         	 &opt non-terminating-p readtable
;;; 文字 char が、読み込み表 readtable 中のディスパッチマクロ文字になるよう
;;; にし、t を返す。
;;; readtable の既定値は現在の読み込み表 (変数 *readtable* の値) 。
;;; non-terminating-p が nil でない (既定値は nil) なら char は非終端マクロ
;;; 文字となり、それは、拡張されたトークンの中に組み込むことができる。
;;; set-dispatch-macro-charactor get-dispatch-macro-charactor 参照。
;;; ＠
;;; make-echo-stream                       関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-echo-stream input-stream output-stream
;;; input-stream から入力を得、output-stream に出力を送るような
;;; 双方向ストリームを生成し、返す。
;;; input-stream から取られるすべての入力は、output-stream へエコーされる。
;;; ＠
;;; make-fatstring                         関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : make-fatstring string font &opt start end
;;; string にフォント情報を付け加える。フォント情報は、font によりビット
;;; パターンで表される。ビットパターンは、次のようになる。
;;;   全ビットがオフ  (font = 0)  通常のフォント
;;;   bit-0   がオン  (font = 1)  反転のフォント
;;;   bit-1   がオン  (font = 2)  ブリンクのフォント
;;;   bit-2   がオン  (font = 4)  アンダーラインが引かれる
;;;   bit-3   がオン  (font = 8)  強調(明るくなる)フォント
;;; これらのフォントすべての組み合わせも可能。例えば、font が 7 なら、その
;;; キャラクタは、反転し、ブリンクし、アンダーラインが引かれる。
;;; string は、fatstring も可。この場合、fatstring  のフォント情報は、font
;;; と置き換えられる。
;;; start の既定値は 0、end の既定値は string の長さ。
;;; 
;;; <例>
;;;         (make-fatstring "abcdefghij" 1 2 4) -> "abcdefghij"
;;;         "c" "d" は、反転
;;;         (make-fatstring "abcdefghij" 2 4 7) -> "abcdefghij"
;;;         "e" "f" "g" は、ブリンク
;;; ＠
;;; make-hash-table                        関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-hash-table &key :test :size :rehash-size
;;;                               :rehash-threshold
;;; 新しいハッシュ表を生成し、返す。
;;; 各引数の意味及び指定方法は次のとおり。
;;;   (1) :test  #'eq, #'eql, #'equal 又は eq, eql, equal のシンボルの
;;;              いずれかで指定する。
;;;   (2) :size  ハッシュ表エントリ数の初期値
;;;   (3) :rehash-size  エントリが不足した場合の 1 回の増分値
;;;   (4) :rehash-threshold  最大エントリ数
;;; 
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}81749(hash-table . 8)
;;;         (!b (make-hash-table :rehash-size 1.5 :size 10)) -> 
;;;         	{vector}81727(hash-table . 8)
;;; ＠
;;; make-instance                          関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-instance class-name init-iv1 value1
;;;                                   init-iv2 value2
;;;                                   ...
;;;                                   init-ivN valueN
;;; クラス class-name の udo を作成し、それを返す。
;;; 第 2 引数以降を指定することにより、作成された udo のインスタンス変数
;;; が初期化される。
;;; init-iv1 init-iv2 ... init-ivN は、作成された udo のインスタンス変数
;;; 名である。value1 は init-iv1 の初期値、value2 は init-iv2 の初期値、
;;; ...である。
;;; 
;;; <例>
;;;         (defclass a () (x y) () :gettable :settable) -> a
;;;         (!bb (make-instance 'a x 5 y 6)) -> {udo}43848a
;;;         [bb x] -> 5
;;;         [bb y] -> 6
;;; ＠
;;; make-kanji-char                        関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : make-kanji-char code
;;; code の 7 ビット目と 15 ビット目を "1" にし、code の値がキャラクタ
;;; コードである jcharactor (2 バイト文字) を生成し、返す。
;;; code の値に対応する jcharactor がなければ、nil を返す。
;;; ＠
;;; make-list                              関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-list length &key :initial-element
;;; length の長さのリストを作り、返す。そのリストの各要素は
;;; :initial-element で指定された値 (既定値は nil) で初期化される。
;;; 
;;; <例>
;;;         (make-list 5) -> (nil nil nil nil nil)
;;;         (make-list 5 :initial-element 'abc) -> (abc abc abc abc abc)
;;;         (!a '(1 2 3)) -> (1 2 3)
;;;         (!b (make-list 2 :initial-element a)) -> ((1 2 3) (1 2 3))
;;;         b -> ((1 2 3) (1 2 3))
;;; ＠
;;; make-mutant                            関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-mutant instance instance1 instance2 ... instanceN
;;; instance と同じクラスのインスタンス instance1 instance2 ... instanceN
;;; を新たに作る。
;;; 関数 make-instance では作ることの出来ない古いバージョンのクラスの
;;; インスタンスを作りたいとき有効である。
;;; 
;;; <例>
;;;         (defclass a () (x y) () :gettable :settable) -> a
;;;         (!bb (make-instance 'a x 5 y 6)) -> {udo}43848a
;;;         [bb x] -> 5
;;;         [bb y] -> 6
;;;         (defclass a () (x y z) () :gettable :settable) -> a
;;;         これを実行すると "Class a incompatibly changed."
;;;         という警告を発する。
;;;         (!cc (make-instance 'a x 50 y 60 z 70)) -> {udo}43900a
;;;         [cc x] -> 50
;;;         [cc y] -> 60
;;;         [cc z] -> 70
;;;         (!dd (make-mutant bb x 100)) -> {udo}43940a
;;;         [dd x] -> 100
;;;         [dd y] -> nil
;;;         [dd z] signals an error
;;; ＠
;;; make-package                           関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-package package
;;;         	 &key :nickname :use :parent :size :sublink
;;; package を新たに作り、それを返す。
;;; :nickname   package のニックネームを指定。
;;; :use        package 内で使いたいパッケージのリストを指定。
;;; :parent     package の親パッケージを指定。
;;; :size       ハッシュテーブルの大きさを指定 (2 の n 乗で示される数値で
;;;             package 中のシンボル数の 1/2 以上)。
;;; :sublink    既定値は t 。 nil の時は package は親パッケージのサブ
;;;             パッケージとして登録されない。
;;; 
;;; <例>
;;;         "gonbe" パッケージ内で TAO を使っていると仮定すると....
;;;         (!gg (make-package 'gonta)) -> {vector}54303(package . 10)
;;;         (package-name gg) -> "gonta"
;;;         (package-name (parent-package gg)) -> "gonbe"
;;;         ここで
;;;         (!pac-sys (global-package "sys"))
;;;                                 -> {vector}34202(package . 10)
;;;         (!pac-net (global-package "net"))
;;;                                 -> {vector}36204(package . 10),
;;;         とすると
;;;         (!hh (make-package 'hanako gg 64 (list pac-sys pac-net)))
;;;                                -> {vector}59439(package . 10)
;;;         (package-name (parent-package hh)) -> "gonta"
;;;         (package-name (package-use-list hh)) -> ("sys" "net")
;;; ＠
;;; make-pathname                          関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-pathname &key :defaults :host :devise :directory
;;;         		    :name :type :version
;;; ある構文要素を与えられると、パス名を組み立てて、それを返す。
;;; パス名の udo (user defined object) を作成し、構文要素を満たす。
;;; 構文要素のうち :host :device :directory :name :type :version で指定
;;; されたものはそれらで構文要素を満たす。
;;; 欠けた構文要素は、:defaults で指定された既定値から満たす。
;;; :defaults の既定値は、そのホスト構文要素は *default-pathname-defaults*
;;; の値のホスト構文要素と同一であり、その他の構文要素は nil であるパス名。
;;; 
;;; <例>
;;;         (make-pathname :host "Ho") -> {udo}1798779pathname
;;;         (namestring (make-pathname :host "Ho" :device "bs"
;;;         			   :directory "dire" :name "test"
;;;         			   :type "tao" :version "5"))
;;;         	-> "Ho::bs:<dire>test.tao.5"
;;;         (namestring (make-pathname :defaults 
;;;           *default-pathname-defaults*)) -> "Ho::bs:<dire>foo.tao"
;;; ＠
;;; make-process                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-process process &rest params &key :priority
;;;                       :interprocess-closure :login :quantum :job 
;;;                       :bottom :package :resumer :initial-function 
;;;                       :initial-argument-list :plist :variable-cache 
;;;                       :error :default-sysmode :readtable
;;; process を生成して返す。process は、その計算状態を初期化するために
;;; リセットまたはプリセットされた後、処理を開始する。
;;; キーワードパラメータの既定値は、以下のようになる。
;;;         :priority   2
;;;         :interprocess-closure   nil
;;;         :login   この関数を作ったログインユーザ名
;;;         :quantum   5 
;;;         :job   nil
;;;         :bottom   15
;;;         :package   カレントパッケージのコピー
;;;         :resumer   nil
;;;         :initial-function   sys:process-stop
;;;         :initial-argument-list   nil
;;;         :plist   ('sys:workbuf1 (get-memblk #!8b-memblk 1024)
;;;                   'sys:workbuf2 (get-memblk #!8b-memblk 1024))
;;;         :variable-cache   6
;;;         :error   vanilla-error
;;;         :default-sysmode   #4004
;;;         :readtable   tao-standard-readtable のコピー
;;; 
;;; <例>
;;;         (!process1
;;;          (make-process
;;;          'abc
;;;          :priority (1- [sys:current-process :priority])
;;;          :readtable *readtable*
;;;          :interprocess-closure nil
;;;          :initial-function 'initial-fun
;;;          :bottom (max2 4 (1- 
;;;           [sys:current-process sys:bottom-stack-block#])))))
;;; ＠
;;; make-random-state                      関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-random-state &opt state
;;; 乱数状態データを生成する。state が nil または省略された場合には、変数 
;;; *random-state* に格納されている値をコピーして返す。
;;; 引数で状態オブジェクトが指定されれば、その状態オブジェクトの値をコピー
;;; して返す。
;;; 
;;; <例>
;;;         (make-random-state) -> {udo}1749564random-state
;;;         (make-random-state {udo}1749564) -> {udo}1192629random-state
;;; ＠
;;; make-row-buf                           関数[#!subr-simple]
;;; 
;;; <説明>
;;;   形式 : make-row-buf str number 8b-memblk fs-width
;;; 内部関数のため一般ユーザには開放しない。
;;; メモリブロック 8b-memblk に、string の number 番目の文字からの
;;; 文字コードを格納する。8b-memblk に全部の文字が格納できない場合、
;;; 格納できなかった文字列の最初の文字位置を返す。
;;; 文字が余分にない場合は 0 を返す。8b-memblk 内の全ての領域に文字が格納
;;; できなかった場合、fs-width 番目のコラムは ! のマークで埋められ、
;;; その他のコラムはすべてクリアされる。8b-memblk のサイズは 8 の倍数になる
;;; ことが多い。日本語の場合、そのコードは 2 つの 8-bit コードに分けられ、
;;; その各々のコードの 最上位ビットは 1 にセットされる。
;;; ただし、分かれた 2 つのコードは 2 行にわたってはならない。
;;; つまり fs-width が 79 の場合、78 番目のコラムは分かれた 2 つのコードの
;;; 最初の部分であってはならない。
;;; fs-width が 79 の場合、1 行の長さが fs-width を越えてしまったときには、
;;; ! マークは 78 番目のコラムに書かれなければならない。
;;; ZEN の画面上で 2 行以上にまたがる文字列を ! を付けて表示するための処理
;;; をする。
;;; ＠
;;; make-sequence                          関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : make-sequence type size &key :initial-element
;;; 型 type、及び大きさ size を持ったシーケンスを生成し、:initial-element
;;; で各要素を初期化し、その結果を返す。
;;; 
;;; <例>
;;;         #|(!v (make-sequence '(vector integer) 100) ->
;;;         	{vector}1834071(common:simple-general-vector . 100)
;;;         (show-vector v) ->
;;;         	vector: common:simple-general-vector vsize: 100
;;;         	    0 kdr: {undef} 0
;;;                        .     .
;;;                        .     .
;;;         	   99 kar: {undef} 0 
;;;         	{vector}1834071(common:simple-general-vector . 100)
;;; ＠

(defun make-string (size &optional (character #\Space))
  "make-string                            関数[#!subr]

<説明>
  形式 : make-string size &opt character
長さが size の文字列を生成し、character の値で初期化する。
common:make-string と同じ。character の既定値は \" \"。

<例>
        (make-string 3 \"a\") -> \"aaa\"
        (make-string 2) -> \"  \"
        (make-string 3 \"ab\") はエラーを示す。
        (make-string 5 \"え\") -> \"えええええ\""
  (cl:make-string size :initial-element (if (characterp character) 
					    character 
					    (coerce character 'character))))

;;; ＠
;;; common:make-string                     関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : common:make-string size &key :initial-element
;;; 長さが size の文字列を生成し、:initial-element の値で初期化する。
;;; :initial-element の既定値は #¥space。
;;; 
;;; <例>
;;;         (common:make-string 5 :initial-element #¥a) -> "aaaaa"
;;;         (common:make-string 10 :initial-element #¥え) -> 
;;;         				"ええええええええええ"
;;;         (common:make-string 5) -> "     "
;;; ＠
;;; make-string-input-stream               関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-string-input-stream string &opt start end
;;; 入力ストリームを生成し、返す。生成された入力ストリームは、start と
;;; end で区切られた string の副文字列の中の文字を順に与える。
;;; 最後の文字を与えた後に、このストリームは end-of-file となる。
;;; ＠
;;; make-string-output-stream              関数[#!expr]
;;; 
;;; <説明>
;;;   与えられたすべての出力を累積するような出力ストリームを生成し、返す。
;;; これにより、関数 get-output-stream-string を利用できる。
;;; ＠


(defun make-string-with-fill-pointer (size initial-element frame-size)
  "make-string-with-fill-pointer          関数[#!expr]

<説明>
  形式 : make-string-with-fill-pointer size initial-element frame-size
フィルポインタ付きの文字列 (長さ可変ストリングとして定義) を生成して、
それを返す。その文字列は、長さが size で initial-element によって初期化
される。frame-size は、生成された文字列が持つことができる文字の最大数。
フィルポインタは、文字が存在する最後の箇所 (最初は sizeに等しい) を
ポイントする。フィルポインタは、関数 string-fill-pointer または
fill-pointer によりアクセス可能。
frame-size の値は、関数 array-dimensions または array-dimension に
より得ることができる。最初のフィルポインタ付き文字列に文字列を付け加
えるには関数 vector-push を使う。

<例>
        (!x (make-string-with-fill-pointer 10 \"A\" 20)) → \"AAAAAAAAAA\"
        (string-fill-pointer x) → 10
        (make-string-with-fill-pointer 30 \"A\" 20) → エラー"
  (make-array frame-size
	      :element-type 'character
	      :initial-element (if (characterp initial-element) 
				   initial-element
				   (coerce initial-element 'character))
	      :fill-pointer size))

;;; ＠
;;; make-symbol                            関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : make-symbol print-name
;;; 印字名 print-name の先頭に #: 記号を付与し、その結果を新しいシンボル
;;; として返す。#: はそのシンボルがインターンされないことを意味する。
;;; 
;;; <例>
;;;         (make-symbol 'a) -> #:a
;;;         (make-symbol 'xyz) -> #:xyz
;;; ＠
;;; make-synonym-stream                    関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-synonym-stream symbol
;;; 変数ストリームを生成し返す。
;;; 変数ストリームへの入出力操作は symbol を名前とする動的変数の値である
;;; ストリームへの入出力操作となる。
;;; ＠
;;; make-two-way-stream                    関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : make-two-way-stream input-stream output-stream
;;; input-stream から入力を得、output-stream に出力を送るような双方向
;;; ストリームを生成し、返す。
;;; ＠
;;; makunbound                             関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : makunbound symbol
;;; グローバル変数 symbol を値を持たない状態にし、symbol を返す。
;;; ローカル変数には適用不可。
;;; 
;;; <例>
;;;         (boundp 'aa) -> nil
;;;         (!aa '(1 2 3)) -> (1 2 3)
;;;         aa -> (1 2 3)
;;;         (boundp 'aa) -> t
;;;         (makunboundp 'aa) -> aa
;;;         (boundp 'aa) -> nil (unbound-variable aa nil)
;;; ＠
;;; map                                    関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : map type func seq1 &rest seq2 ... seqN
;;; シーケンス seq1 seq2 ... seqN に、関数 func を順番に適用し、その結果を
;;; 型 type で返す。
;;; 
;;; <例>
;;;         (map 'list 'write (list 1 2 3 4))
;;;              -> 1
;;;                 2
;;;                 3
;;;                 4
;;;                 (1 2 3 4)
;;;  	(map 'list '- '(1 2 3 4)) -> (-1 -2 -3 -4)
;;;         (map 'string #'(lambda (x) (if (oddp x) #¥1 #¥0)) '(1 2 3 4))
;;;                         -> "1010"
;;;         (map 'list 'index '(1 5) '(4 9 13)) -> ((1 2 3 4) (5 6 7 8 9))
;;;         (map 'string 'index '(1 5) '(4 9 13)) -> 
;;;         		"(1 2 3 4 ) (5 6 7 8 9)"
;;; ＠

(defun mapatoms (func &optional (package *package*))
  "mapatoms                               関数[#!expr]

<説明>
  形式 : mapatoms func &opt package
関数 func を package (既定値はカレントパッケージ) 中の全てのシンボルに
適用し、nil を返す。func は引数が 1 つの関数。

<例>
        (mapatoms 'print pkg) prints all of the symbols in pkg.
        (mapatoms 'print sys:key-package) -> (:until :name :input ...)"
  (do-symbols (i (find-package package))
    (funcall func i)))
      
#|(mapatoms (lambda(x) (and (eq 'with-gensym x)(print x)) :key))
よくわかんね!!
|#


;;; ＠
;;; mapc                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mapc func &rest list1 list2 ... listN
;;; list1 list2 ... listN の各要素を引数として、関数 func を繰返し呼び出し、
;;; list1 をそのまま返す。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (mapc 'index '(1 2 3 4) '(2 3 4 5)) -> (1 2 3 4)
;;;         (mapc 'write (list 1 2 3 4)) ->
;;;           1
;;;           2
;;;           3
;;;           4
;;;           (1 2 3 4)
;;;        (mapc (lambda (x y) (set x y)) (listq p q r) (list 100 200))は
;;;         変数 p と q を 100 と 200 に逐次セットし、(p q r)を返す 
;;;         (r への代入は起こらない)。
;;; ＠
;;; mapcan                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mapcan func &rest list1 list2 ... listN
;;; list1 list2 ... listN の各要素を引数として、関数 func を繰返し呼び出し、
;;; func を評価した結果をリスト形式で返す。
;;; list1 list2 ... listN に func を連続的に適用して得られた結果を連結
;;; (nconc) したリストが返される。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (mapcan 'index '(1 3 5 7) '(2 4 6 8)) -> (1 2 3 4 5 6 7 8)
;;;         (mapcan 'write (list 1 2 3 4)) ->
;;;           1
;;;           2
;;;           3
;;;           4
;;;          (1 2 3 4)
;;;         (mapcan 'cons (list 1 2 3 4) (list 5 6 7 8)) ->
;;;           (1 2 3 4 . 8)
;;; ＠
;;; mapcar                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mapcar func &rest list1 list2 ... listN
;;; list1 list2 ... listN の各要素を引数として、関数 func を繰返し呼び出し、
;;; 評価した結果をそのまま並べてリストにして返す。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (mapcar 'index '(1 3 5 7) '(2 4 6 8)) -> 
;;;         	((1 2) (3 4) (5 6) (7 8))
;;;         (mapcar 'write (list 1 2 3 4)) ->
;;;           1
;;;           2
;;;           3
;;;           4
;;;           (1 2 3 4)
;;;         (mapcar 'cons (list 1 2 3 4) (list 5 6 7 8)) ->
;;;           ((1 . 5) (2 . 6) (3 . 7) (4 . 8))
;;;         (mapcar (lambda (x) (x * x)) (list 1 2 3 4 5)) -> 
;;;           (1 4 9 16 25)
;;; ＠
;;; mapcatoms                              関数[#!exprdyn]
;;; 
;;; <説明>
;;;   形式 : mapcatoms func &opt package
;;; 関数 fn を package にある全てのシンボルに適用し、そのリターン値をリスト
;;; にして返す。
;;; 
;;; <例>
;;;         (mapcatoms #'(lambda (x) (print (list x)))) -> 
;;;         	(ch1)
;;;         	(ch2)
;;;         	(step)
;;;                 ...
;;; ＠
;;; mapcon                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mapcon func &rest list1 list2 ... listN
;;; list1 list2 ... listN に関数 cdr を順に適用してできるリストを引数として、
;;; 引数のどれかが空になるまで、関数 func を繰返し呼び出し、評価した結果を
;;; 連結 (nconc) してリストとして返す。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (mapcon 'write (list 1 2 3 4)) ->
;;;           (1 2 3 4)
;;;           (2 3 4)
;;;           (3 4)
;;;           (4)
;;;           (1 2 3 4 2 3 4 3 4 4)
;;;         (mapcon (lambda (x y) (write (list x y)))
;;;                 (listq p q r) (list 100 200)) ->
;;;           ((p q r) (100 200))
;;;           ((q r) (200))
;;;           ((p q r) (100 200) (q r) (200))
;;; ＠
;;; maphash                                関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : maphash func table
;;; 関数 func にハッシュ表 table の処理をさせる。
;;; 
;;; <例>
;;;         (!a (make-hash-table :size 10)) -> 
;;;                   {vector}45533(hash-table . 8)
;;;         (!(gethash 'color a) 1) -> 1
;;;         (!(gethash 'qwe a) 2) -> 2
;;;         (maphash #'(lambda (x y) (print (list x y))) a) -> 
;;;         		(qwe 2) (color ) nil
;;; ＠
;;; mapl                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mapl func &rest list1 list2 ... listN
;;; list1 list2 ... listN に関数 cdr を順に適用してできるリストを引数として、
;;; 引数のどれかが空になるまで、関数 func を繰返し呼び出し、評価し、list1
;;; を返す。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (mapl 'cons '(a b c) '(1 2 3)) -> (a b c)
;;;  	(mapl #'(lambda (x) (length x)) '(7 2 3 8)) -> (7 2 3 8)
;;;  	(mapl 'write (list 1 2 3 4)) ->
;;;           (1 2 3 4)
;;;           (2 3 4)
;;;           (3 4)
;;;           (4)
;;;           (1 2 3 4)
;;;         (mapl (lambda (x y) (write (cons x y)))
;;;                 (listq p q r) (list 100 200)) ->
;;;           ((p q r) (100 200))
;;;           ((q r) (200))
;;;           (p q r)
;;; ＠
;;; maplist                                関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : maplist func &rest list1 list2 ... listN
;;; list1 list2 ... listN に関数 cdr を順に適用してできるリストを引数として、
;;; 引数のどれかが空になるまで、関数 func を繰返し呼び出し、評価した結果を
;;; そのまま並べてリストとして返す。
;;; func は、list1 list2 ... listN の個数と同じ数の引数をとる。
;;; 
;;; <例>
;;;         (maplist 'write (list 1 2 3 4)) ->
;;;                 (1 2 3 4)
;;;                 (2 3 4)
;;;                 (3 4)
;;;                 (4)
;;;         	((1 2 3 4) (2 3 4) (3 4) (4))
;;;         (maplist 'union (list 1 2 3 4 5) (list 5 4 3 2 1)) ->
;;;           ((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 4 5) (1 5))
;;;         (maplist (lambda (x y) (write (cons x y)))
;;;                  (listq p q r) (list 100 200)) ->
;;;           ((p q r) 100 200)
;;;           ((q r) 200)
;;;           (((p q r) 100 200) ((q r) 200))
;;; ＠
;;; mask#                                  ロカティブオペレータ
;;; 
;;; <説明>
;;;   形式 : loc1 mask# loc2
;;; loc1 が loc2 でマスクされる。つまり、loc1 のビットは、loc2 の対応する
;;; ビットが 0 なら、その値は保持され、1 なら 0 になる。
;;; 
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- #5252) -> 2730
;;;         (q <- #7070) -> 3640
;;;         (s <- (p mask# q )) -> 130 (#202)
;;;         s -> 130
;;;         (r <- (q mask# p)) -> 1040 (#2020)
;;;         r -> 1040
;;; ＠
;;; mask-field                             関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : mask-field bytespec integer
;;; integer の bytespec で指定されたビット位置以外を 0 にし、結果を返す。
;;; 
;;; <例>
;;;         (mask-field (byte 1 2) 3) -> #0
;;;         (mask-field (byte 13 5) 100) -> #140
;;; ＠
;;; max                                    関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : max &rest number1 number2 ... numberN
;;; number1 number2 ... numberN の最大値を返す。
;;; 
;;; <例>
;;;         (max 1 3 2 -7) -> 3
;;;         (max -1 -2 -3 -4) -> -1
;;;         (max) -> 0
;;; ＠

;;; max2                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : max2 number1 number2
;;; number1 と number2 の最大値を返す。max 関数より実行速度が速い。

(defun max2 (number1 number2)
  (declare (fixnum number1 number2))
  (if (< number1 number2) 
      number2
      number1))
#|(time
 (do ((i 0 (1+ i))
      (j 1 (1+ j)))
     ((= 1000000 i))
   (max2 i j)))|#

;;; 
;;; <例>
;;;         (max2 -1 1) -> 1
;;;         (max2 10 10) -> 10
;;; ＠
;;; mem                                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mem pred item list
;;; list 中に item と条件 pred を満足する要素を検索し、その要素以降を
;;; リストとして返す。そのような要素がないときは nil を返す。
;;; 
;;; <例>
;;;         (mem 'equal 'c '(a b c d e)) = (c d e)
;;;         (mem 'equal '(a c) '((a x) (a c b) (a c) (c b) d)) = 
;;;         ((a c) (c b) d)
;;;         (mem 'equal 'c '(a b (c d) d c b a)) = (c b a)
;;;         (mem '< 4 (list 1 3 7 3 4 2)) -> (7 3 4 2)
;;;         (mem '< 10 (list 1 3 7 3 4 2)) -> nil
;;; ＠
;;; memass                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : memass pred item a-list 
;;; 連想リスト a-list の第 1 要素の値において、item と条件 pred を満足する
;;; ペアを検索し、その要素以降をリストとして返す。
;;; そのような要素がないときは nil を返す。
;;; (car (memass pred item a-list)) = (ass pred item a-list)
;;; 
;;; <例>
;;;         (memass '> 5 ((6 . six) (3 . three) (4 . four)))
;;;         -> ((3 . three) (4 . four))
;;; ＠
;;; member                                 関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : member item list &key :test :test-not :key
;;; list に、item と eql な要素が含まれていれば、最初の該当する要素以降の
;;; 要素で構成されるリストを返し、そのような要素がなければ nil を返す。
;;; memql と同じ。ただし memql ではキーワード引数の指定ができない。
;;; 
;;; <例>
;;;         (member '1 '(3 2 1)) -> (1)
;;;         (member '1 '(3 2 0)) -> ( )
;;;         (member '1 '(1 2 3)) -> (1 2 3)
;;; ＠
;;; member-if                              関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : member-if pred list &key :key
;;; list に、条件 pred を満足する要素が含まれていれば、最初の該当する要素
;;; 以降の要素で構成されるリストを返す。そのような要素がなければ nil を返す。
;;; 
;;; <例>
;;;         (memberp #'numberp '(a (1) 2 b)) -> (2 b)
;;;         (memberp #'listp '(a (1) 2 b)) -> ((1) 2 b)
;;;         (memberp #'listp '(a 2 b)) -> nil
;;; ＠
;;; member-if-not                          関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : member-if-not pred list &key :key
;;; list に、条件 pred を満足しない要素が含まれていれば、最初の該当する要素
;;; 以降の要素で構成されるリストを返す。満足しない要素がなければ nil を返す。
;;; 
;;; <例>
;;;         (member-if-not #'numberp '(0 a 1 b)) -> (a 1 b)
;;;         (member-if-not #'numberp '(0 1 2))   -> nil
;;; ＠
;;; memblk-size                            関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : memblk-size memblk
;;; メモリブロック memblk の大きさを返す。
;;; 
;;; <例>
;;;         (!aaa (get-memblk #!8b-memblk 20)) ->
;;;                        {memblk}480569(#!8b-memblk . {dnil}20)
;;;         (memblk-size aaa) -> 20
;;; ＠
;;; memblkp                                関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : memblkp memblk
;;; memblk がメモリブロックなら、評価値を返し、それ以外なら nil を返す。
;;; ELIS には以下のようにメモリブロックがある。
;;;   #!1b-memblk        #!2b-memblk        #!4b-memblk
;;;   #!8b-memblk        #!16b-memblk       #!32b-memblk
;;;   #!64b-memblk       #!prestk-memblk    #!id-hash-memblk
;;;   #!64bloc-memblk    #!strhead-memblk   #!locbit-memblk
;;;   #!cell-memblk      #!vector-memblk    #!id-memblk
;;;   #!str-memblk       #!bad-memblk       #!free-memblk
;;; 
;;; <例>
;;;         (!aa (get-memblk #!8b-memblk 16))
;;;      		-> {memblk}1387401(#!8b-memblk . {dnil}16)
;;;         (memblkp aa) -> {memblk}1387401(#!8b-memblk . {dnil}16)
;;; ＠
;;; memory-capacity                        関数[#!expr]
;;; 
;;; <説明>
;;;   システムが主記憶として何バイト取るかを調べ、その数値を返す。
;;; 
;;; <例>
;;;         (memory-capacity) -> 16384
;;;         システムが主記憶上に  16384 バイト持つことを意味する。
;;; ＠
;;; memq                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : memq item list
;;; item の値と eq なリスト list の要素を左から右に検索し、最初の要素を
;;; 発見したら、その要素以降の要素をリストとして返す。
;;; そのような要素がないときは nil を返す。
;;; (memq x y) = (mem eq x y)
;;; 
;;; <例>
;;;         (memq 3 '(1 2 3 4)) -> (3 4)
;;;         (memq 5 '(1 2 3 4)) -> nil
;;;         (memq 'm '(m e m q)) -> (m e m q)
;;;         (memq '(3 4) '((1 2) (3 4))) -> nil
;;; ＠
;;; memql                                  関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : memql item list
;;; item の値と eql なリスト list の要素を左から右に検索し、最初の要素を
;;; 発見したら、その要素以降の要素をリストとして返す。
;;; そのような要素がないときは nil を返す。
;;; (memql x y) = (mem eql x y)
;;; 
;;; <例>
;;;         (memql 'x '(a x b c)) -> (x b c)
;;;         (memql '1 '(2 3 4) ->  nil
;;; ＠
;;; memqu                                  関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : memqu item list
;;; item の値と equal な list の要素を左から右に検索し、最初の要素を発見
;;; したら、その要素以降の要素をリストとして返す。
;;; そのような要素がないときは nil を返す。
;;; (memqu x y) = (mem equal x y)
;;; 
;;; <例>
;;;         (memqu 'x '(a x b)) -> (x b)
;;;         (memqu 'x '(1 2) -> nil
;;; ＠
;;; merge                                  関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : merge result-type seq1 seq2 test &key :key
;;; 条件 test に従って、シーケンス seq1 と seq2 を併合し、型 type の
;;; 新たなシーケンスを生成する。
;;; 
;;; <例>
;;;         (!x '(1 3 4 6 7)) -> (1 3 4 6 7)
;;;         (!y '(2 5 8)) -> (2 5 8)
;;;         (merge 'list x y #'<) -> (1 2 3 4 5 6 7 8)
;;;         (merge 'string x y #'<) -> "12345678"
;;;         (merge 'string "BOY "boy" #'char<) -> "BOYboy"
;;;         (merge 'string "BOY" "boy" #'char-lessp) -> "BbOoYy"
;;; ＠
;;; merge-pathnames                        関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : merge-pathnames name &opt filler version
;;; パス名 name が full-pathname-object でないなら、filler を使って構文要素
;;; を name に与えることによって name から full-pathname-object を構成する。
;;; 構成された pathname-object を返す。
;;; version は pathname-object のバージョンナンバー。
;;; version の既定値は、:newest。
;;; filler の既定値は、*default-pathname-defaults* による。
;;; 
;;; <例>
;;;         (merge-pathnames "bs:<iyayo>dame.yo") -> {udo}170492pathname
;;; ＠
;;; mig                                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mig place
;;; 場所 place に格納された値を返す。
;;; 
;;; <例>
;;;         (bas:make-base-vector 10) ->
;;;         	 {vector}1831757(sys:base-vector . 10)
;;;         (!(mig 5) 123) -> 123
;;;         (mig 5) -> 123
;;;         (!xxx 890) -> 890
;;;         (mig (hidar xxx)) -> 890
;;;         (mig '(a b c)) -> a
;;; ＠
;;; min                                    関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : min &rest number1 number2 ... numberN
;;; number1 number2 ... numberN の最小値を返す。
;;; 
;;; <例>
;;;         (min 1 3 2 -7) -> -7
;;;         (min -1 -2 -3 -4) -> -4
;;;         (min) -> 0
;;; ＠
;;; min2                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : min2 number1 number2
;;; number1 と number2 の最小値を返す。min より実行速度が速い。
;;; 
;;; <例>
;;;         (min2 -1 0) -> -1
;;;         (min2 10 100 -> 10
;;; ＠
;;; minus                                  関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : minus number
;;; number の負数 (符号を反転した結果) を返す。
;;; (minus  x) = (neg x)
;;; 
;;; <例>
;;;         (minus 100) -> -100
;;;         (minus -100) -> 100
;;; ＠
;;; minusp                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : minusp number
;;; number が負の (0 より小さい) 場合、その値を返し、
;;; 負でなければ (0 又は正) nil を返す。
;;; 
;;; <例>
;;;         (minusp -543654) -> -543654
;;;         (minusp 123) -> nil
;;;         (minusp 0) -> nil
;;; ＠
;;; mismatch                               関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : mismatch seq1 seq2 &key :from-end :test :test-not :key 
;;;                                  :start1 :end1 :start2 :end2
;;; シーケンス seq1 の :start1 から :end1 までの要素と、seq2 の :start2
;;; から :end2 までの要素を照合し、要素の数と各要素の値が完全に一致すれば
;;; nil を返し、一致しなかった場合には最初に一致しなかった seq1 の最初の
;;; 要素のインデックスを返す。
;;; 
;;; <例>
;;;         (mismatch '(a b c) '(a b c d e)) -> 3
;;;         (mismatch '(a b c) '(a b c d e) :start1 2 :end1 3) -> 2
;;;         (mismatch '(a b c) '(a b c d e) :start1 2 :end1 3
;;;         		       	 :start2 2 :end2 4) -> 3
;;; ＠
;;; mod                                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : mod number1 number2
;;; number1 を、number2 の値で割ったときの剰余を返す。
;;; (mod number1 number2) = [number1 mod number2] = (number1 mod number2)
;;; 
;;; <例>
;;;         (mod 14 4) -> 2
;;; ＠
;;; mod#                                   ロカティブオペレータ
;;; 
;;; <説明>
;;;   形式 : loc1 mod# loc2
;;; loc1 について mod 操作を行う。loc2 は法。
;;; 
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- 100) -> 100
;;;         (q <- 256) -> 256
;;;         (s <- (p mod# 13 )) -> 9
;;;         (s <- (p mod# 11 )) -> 1
;;;         (s <- (p mod# 7 )) -> 2
;;;         (r <- (q mod# 8 )) -> 0
;;; ＠
;;; month-string                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : month-string number
;;; number に対応する月名 (上 3 桁) を文字列で返す。
;;; number が 1〜12 以外の時は、nil が返る。
;;; 
;;; <例>
;;;         (month-string 1) -> "Jan"
;;;         (month-string 2) -> "Feb"
;;;         (month-string 3) -> "Mar"
;;;         (month-string 4) -> "Apr"
;;;         (month-string 5) -> "May"
;;;         (month-string 6) -> "Jun"
;;;         (month-string 7) -> "Jul"
;;;         (month-string 8) -> "Aug"
;;;         (month-string 9) -> "Sep"
;;;         (month-string 10) -> "Oct"
;;;         (month-string 11) -> "Nov"
;;;         (month-string 12) -> "Dec"
;;; ＠
;;; more                                   関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : more &opt terno
;;; ターミナル番号が terno のターミナルに "more" モードを設定する。
;;; terno の既定値はこの関数が入力されたターミナル。
;;; "more" モードでのターミナルの出力は出力の総計が 1 画面を越える前に
;;; 中断される。
;;; "space" キーが押されると、次の 1 画面が表示される。
;;; "?" をたたくとコマンド表を表示する。
;;; ＠
;;; most-negative-double-float             定数
;;; 
;;; <説明>
;;;   double-float 演算において取り得る最も大きな負の値を格納した
;;; システム定数であり、このシステムの場合は -1.79769313486231f308。
;;; ＠
;;; most-negative-fixnum                   定数
;;; 
;;; <説明>
;;;   fixnum 演算において取り得る最も大きな負の値を格納した
;;; システム定数であり、このシステムの場合は -8388608。
;;; ＠
;;; most-negative-long-float               定数
;;; 
;;; <説明>
;;;   long-float 演算において取り得る最も大きな負の値を格納した
;;; システム定数であり、このシステムの場合は -1.79769313486231f308。
;;; ＠
;;; most-negative-short-float              定数
;;; 
;;; <説明>
;;;   short-float 演算において取り得る最も大きな負の値を格納した
;;; システム定数であり、このシステムの場合は -1.844667e19。
;;; ＠
;;; most-negative-single-float             定数
;;; 
;;; <説明>
;;;   single-float 演算において取り得る最も大きな負の値を格納した
;;; システム定数であり、このシステムの場合は -1.79769313486231f308。
;;; ＠
;;; most-positive-double-float             定数
;;; 
;;; <説明>
;;;   double-float 演算において取り得る最も大きな正の値を格納した
;;; システム定数であり、このシステムの場合は 1.79769313486231f308。
;;; ＠
;;; most-positive-fixnum                   定数
;;; 
;;; <説明>
;;;   fixnum 演算において取り得る、最も大きな正の値を格納した
;;; システム定数であり、このシステムの場合は 8388607。
;;; ＠
;;; most-positive-long-float               定数
;;; 
;;; <説明>
;;;   long-float 演算において取り得る最も大きな正の値を格納した
;;; システム定数であり、このシステムの場合は 1.79769313486231f308。
;;; ＠
;;; most-positive-short-float              定数
;;; 
;;; <説明>
;;;   short-float 演算において取り得る最も大きな正の値を格納した
;;; システム定数であり、このシステムの場合は 1.84467e19。
;;; ＠
;;; most-positive-single-float             定数
;;; 
;;; <説明>
;;;   single-float 演算において取り得る最も大きな正の値を格納した
;;; システム定数であり、このシステムの場合は 1.79769313486231f308。
;;; ＠
;;; multiple-value-bind                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : multiple-value-bind &rest var1 var2 ... 
;;;                                    val-form form1 form2 ... formN
;;; val-form の評価の結果として多値を返し、その I 番目の値に varI を束縛し、form1 form2 ... を逐次評価し、最後のフォーム formN の値を返す。
;;; 
;;; <例>
;;;         (multiple-value-bind (x)
;;;         	(/ 7 4) (list x)) -> (1)
;;;         (multiple-value-bind (x y)
;;;         	(/ 7 4) (list x y)) -> (1 3)
;;;         (multiple-value-bind (x y z)
;;;         	(/ 7 4) (list x y z)) -> (1 3 nil)
;;; ＠
;;; multiple-value-call                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : multiple-value-call func &rest form1 form2 ... formN
;;; form1 form2 ... formN を評価した値を引数として 関数 func を呼び出し、
;;; 得られた値を返す (form1 form2 ... formN の評価結果が順に第 1 引数、
;;; 第 2 引数、... となる)。
;;; 
;;; <例>
;;;         (!x '(a b c)) -> (a b c)
;;;         (!y '(p q r)) -> (p q r)
;;;         (multiple-value-call #'cons (car x) (car y)) -> (a . p)
;;;         (!x '(1 2 3)) -> (1 2 3)
;;;         (!y '(4 5 6)) -> (4 5 6)
;;;         (!z '(7 8 9)) -> (7 8 9)
;;;         (multiple-value-call #'* (car x) (car y) (car z)) -> 28
;;;         (multiple-value-call #'+ (/ 5 3) (/ 19 4)) -> 10
;;; ＠
;;; multiple-value-list                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : multiple-value-list form
;;; form を評価した結果の複数の値をリストにして返す。values-list の逆関数。
;;; 
;;; <例>
;;;         (multiple-value-list (/ 7 4)) -> (1 3)
;;;         (multiple-value-list 
;;;          	(read-from-string "abc def")) -> ("abc" " def")
;;; ＠
;;; multiple-value-prog1                   関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : multiple-value-prog1 form1 form2 ... formN
;;; form1 form2 ... formN を逐次評価し、form1 の評価値(複数の値となる)
;;; を返す。複数のリターン値を除いて、prog1 と同じ。
;;; 
;;; <例>
;;;         (multiple-value-prog1 (/ 7 4) (/ 7 3) (/ 7 2)) -> !(1 3)
;;; ＠
;;; multiple-value-setq                    関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : multiple-value-setq var1 var2 ... varN form
;;; form を評価し、var1 を最初のリターン値に、var2 を第 2 のリターン値に、
;;; というように複数のリターン値に変数を束縛する。
;;; var1 の値を返す。
;;; 
;;; <例>
;;;         (multiple-value-setq (a b) (/ 7 4)) -> 1
;;;         a -> 1
;;;         b -> 3
;;;         (multiple-value-setq (a b c) (/ 17 7)) -> 2
;;;         a -> 2
;;;         b -> 3
;;;         c -> nil
;;; ＠
;;; multiple-values-limit                  定数
;;; 
;;; <説明>
;;;   multiple-value-limit = 128
;;; 関数は multiple-values-limit の数以下の値を返さなければならない。
;;; ＠
