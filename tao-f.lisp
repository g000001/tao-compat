(in-package #:tao-compat)

;;; ＠
;;; fatjstringp                            関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : fatjstringp string
;;; string が fat 16 ビットキャラクタ (フォント情報を持つ 16 ビット
;;; キャラクタ) を含んでいるなら、string 中のその数を返し、それ以外なら
;;; nil を返す。16 ビットキャラクタは、JIS コードキャラクタ。
;;; 
;;; <例>
;;;         (!x (make-fatstring "あいうえお" 1)) -> "あいうえお"
;;;         (fatjstring x) -> 5   (反転)
;;;         (!y (make-fatstring "あいueo" 2)) -> "あいueo"
;;;         (fatjstring y) -> 2   (点滅)
;;; ＠
;;; fatstringp                             関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : fatstringp string
;;; string が fatstring (フォント情報をもつストリングのデータタイプ名) なら
;;; ば、評価値を返し、それ以外なら nilを返す。
;;; 
;;; <例>
;;;         (!x (make-fatstring "abc"  2)) -> "abc"
;;;         (fatstring x) -> "abc"  (点滅)
;;; ＠
;;; fboundp                                関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : fboundp symbol
;;; symbol が関数名なら関数オブジェクトを返し、そうでなければ nil を返す。
;;; 
;;; <例>
;;;         (fboundp 'car) -> {applobj}32594)#!subr-simple . 6)
;;;         (fboundp 'aaa) -> nil
;;;         (if (fboundp 'car) 'ok) -> ok
;;;         (if (fboundp 'aaa) 'ok) -> nil
;;; ＠
;;; fceiling                               関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fceiling number1 &opt number2
;;; number1 を number2 (既定値は 1) で割り、その結果以上の最小の整数を
;;; 浮動小数点形式で第 1 の値として返す。
;;; 第 2 の値として (- number1 (* 第 1 の値 number2)) を返す。
;;; 
;;; <例>
;;;         (fceiling 0) -> !(0.0f0.0)
;;;         (fceiling -2) -> !(-2.0f0.0)
;;;         (fceiling 2.35) -> !(3.0 -0.649993)
;;; ＠
;;; ffloor                                 関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : ffloor number1 &opt number2
;;; number1 を number2 (既定値は 1)で割り、その結果を超えない最大の整数を
;;; 浮動小数点形式で第 1 の値として返す。
;;; 第 2 の値として (- number1 (* 第 1 の値 number2)) を返す。
;;; 
;;; <例>
;;;         (ffloor -4.7) -> !(-5.0 0.299988)
;;;         (ffloor 3.5d0) -> エラー
;;;         (ffloor 0) -> !(0.0f0 0)
;;;         (ffloor 3.2) -> !(3.0 0.199997)
;;; ＠
;;; fgrep                                  関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fgrep pattern &rest file1 file2 ... fileN
;;; file1 file2 ... fileN において、キャラクタシーケンス pattern を検索し、
;;; 見つかれば、それをファイル名とともにプリントする。
;;; ファイルにはワイルドカード * が指定可能。ファイルが nil なら、 nil を
;;; 返す。grep の簡易版。
;;; 
;;; <例>
;;;         (fgrep "#!expr-simple" "bs:<imada>")
;;;         ディレクトリ bs:<imada> にあるすべてのファイルの
;;;         キャラクタシーケンス #!expr-simple を検索。
;;;         FILE bs:<imada>describeoperations.tao.1
;;;             2: {applobj}72115(#!expr-simple . 6)
;;;         FILE bs:<imada>screen.mem.2
;;;            33: start-ftp-server is function of #!expr-simple
;;;         FILE bs:<imada>screen.txt.2
;;;             9: start-ftp-server is function of #!expr-simple
;;;         FILE bs:<imada>screen.txt.4
;;;             9: start-ftp-server is function of #!expr-simple
;;;     ("bs:<imada>screen.txt.4"  "bs:<imada>screen.txt.2"  
;;;     "bs:<imada>screen.mem.2"  "bs:<imada>describeoperations.tao.1")
;;;     ファイル名とキャラクタシーケンスが存在する行を表示。
;;; ＠
;;; fifth                                  関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : fifth list
;;; list の 5 番目の要素の値を返す。
;;; 
;;; <例>
;;;         (fifth '(1 2 3 4 5 6 7 8 9 0)) -> 5
;;; ＠
;;; file-author                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-author file
;;; file の作成者名を文字列表現で返す。作者名を決定できなければ nil を返す。
;;; file は、ファイル名またはファイルに開かれたストリーム。
;;; 
;;; <例>
;;;         vdir "cs:<dire>manual.tao" ->
;;;         manual.tao.3         15486  hanako  0  4-Apr-87  20:10  hanako
;;;         (file-author "manual.tao") -> "hanako"
;;;         (!aa (open "manual.tao")) -> {udo}1181517file-stream
;;;         (file-author aa) -> "hanako"
;;; ＠
;;; file-conc                              関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-conc files new-name
;;; ファイルのリスト files 中の各ファイルをコピーし、それらを連結し、
;;; ファイル名 new-name の新しいファイルを作る。
;;; 
;;; <例>
;;;         (file-conc '("a" "b" "c") "d")
;;;         最初にファイル a , b , c がコピーされる。次にこのコピーは
;;;         それぞれ連結されて新しいファイル d になる。
;;; ＠
;;; file-length                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-length stream
;;; stream にオープンされたファイルの長さを非負整数で返す。
;;; 長さが決定できなければ nil を返す。
;;; 
;;; <例>
;;;         vdir "cs:<dire>abc.tao" ->
;;;         abc.tao.3            58695  elisko  0  4-Apr-87  20:10  eliko
;;;         (!aa (open "abc.tao")) -> {udo}58251file-stream
;;;         (file-length aa) -> 58695
;;; ＠
;;; file-namestring                        関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-namestring pathname
;;; パス名 pathname の要素のうち名前、型、バージョンを namestring で返す。
;;; 
;;; <例>
;;;         (file-namestring "Ti::bs:<anata>konata.sonata.5") -> 
;;;         	"konata.sonata.5"
;;; ＠
;;; file-position                          関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-position file-stream &opt position
;;; position をランダムアクセスファイル file-stream 中の現在位置として
;;; セットする。position が省略されると、 file-stream 中の現在の位置を表す
;;; 非負整数を返す。位置を決定できなければ nil を返す。ファイルの始まりの
;;; 位置はゼロ。position は、整数、あるいは、そのストリームの先頭を表す
;;; :start あるいは、そのストリームの最後を表す :end をとる。
;;; 位置の再設定に成功すれば t を返し、そうでなければ nil を返す。
;;; 
;;; <例>
;;;         abc.tao のファイルの内容が   1234567890..... とする。
;;;         (!aa (open "abc.tao")) -> {udo}67345file-stream
;;;         (file-position aa) -> 0
;;;         (file-position aa 6) -> t
;;;         (read-line aa) -> "7890....."
;;; ＠
;;; file-stream                            クラス
;;; 
;;; <説明>
;;;   インスタンスがファイルストリームであるクラス。
;;; 入力方向か出力方向のどちらか、あるいは両方向に動作し、ファイルから
;;; ファイルへのいかなるデータもこのストリームを利用できる。
;;; ファイルストリームはディスク入出力。
;;; ＠
;;; file-write-date                        関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : file-write-date file
;;; file が最後に書き込まれた時間をユニバーサルタイム書式で返す。時間を
;;; 決定できなければ nil を返す。
;;; file は、ファイル名、あるいはファイルにオープンされたストリーム。
;;; 
;;; <例>
;;;         (file-write-date "abc.tao") -> 2753483100
;;;         		(1987 年 04 月 03 日 15 時 25 分 00 秒)
;;; ＠
;;; fill                                   関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : fill seq item &key :start :end
;;; シーケンス seq の :start から :end までの要素を item に変更し、
;;; その結果を返す。
;;; 
;;; <例>
;;;         (!x (vector 'a 'b 'c 'd 'e)) ->
;;;                  {vector}1828126(common:simple-general-vector . 5)
;;;         (fill x 'z :start 1 :end 3) ->
;;;         	   {vector}1828126(common:simple-general-vector . 5)
;;;         (fill x 'p) -> 
;;;                   {vector}1828126(common:simple-general-vector . 5)
;;; ＠
;;; fill-pointer                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fill-pointer vector
;;; vector のフィルポインタの値を返す。フィルポインタを持っていない場合は、
;;; エラーを返す。
;;; 
;;; <例>
;;;         (!aa (make-array 5 :fill-pointer t))
;;;                          -> {vector}70287(common:array . 4)
;;;         (fill-pointer aa) -> 5
;;;         (fill-pointer #(1 2 3 4 5)) -> エラー
;;; ＠
;;; filstring                              関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : filstringp object
;;; object がフィルポインタ付きの文字列なら、その値を、そうでなければ nil
;;; を返す。
;;; ＠
;;; filter-copy                            関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : filter-copy tree
;;; tree のフィルタを通したコピーを作成し返す。
;;; フィルタを通すということは潜在ポインタ (comment, splvar, multivalue,
;;; 等) を除いてしまうということ。従って、これらの潜在ポインタはコピー
;;; されない。
;;; 
;;; <例>
;;;         (filter-copy '(a b ; this is a sample list
;;;                        c d))
;;;         -> (a b c d)
;;; ＠
;;; find                                   関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : find item seq &key :from-end :test :test-not :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、 item を検索し、最初の
;;; 値を返す (該当する要素が見つからなければ、nil を返す)。
;;; 
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (find 1 x) -> 1
;;;         (find 1 x :start 4) -> nil
;;;         (find 3 x :start 3 :end 5) -> 3
;;;         (find "a" "qweasd") -> "a"
;;; ＠
;;; find-all-symbols                       関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : find-all-symbols name
;;; システム内の全てのパッケージにおいて name (文字列またはシンボル) に
;;; 該当する全てのシンボルのリストを返す。
;;; 
;;; <例>
;;;         (find-all-symbols "car") -> (car)
;;;         (find-all-symbols "x") -> (dbg:x x sys:x zen:x bas:x)
;;; ＠
;;; find-if                                関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : find-if test seq &key :from-end :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、述語 test を満足する
;;; 要素を検索し、最初の要素の値を返す (該当する要素が見つからなければ、
;;; nil を返す)。
;;; 
;;; <例>
;;;         (find-if #'evenp '(1 2 3 4 5)) -> 2
;;;         (find-if #'string '("1" 2 "3" 4 "5")) ->"1"
;;;         (find-if #'string '("1" 2 "3" 4 "5") :start 3 :end 5) -> "5"
;;;         (find-if #'string '("1" 2 "3" 4 "5") :from-end t) -> "5"
;;; ＠
;;; find-if-not                            関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : find-if-not test seq &key :from-end :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、述語 test を満足しない
;;; 要素を検索し、最初の要素の値を返す (該当する要素が見つからなければ、
;;; nil を返す)。
;;; 
;;; <例>
;;;         (find-if-not #'evenp '(1 2 3 4 5)) -> 1
;;;         (find-if-not #'stingp '("1" 2 "3" 4 "5") -> 2
;;;         (find-if-not #'stringp '("1" 2 "3" 4 "5")
;;;                               :start 3 :end 5) -> 4
;;;         (find-if-not #'stringp '("1" 2 "3" 4 "5")
;;;         		      :from-end t) -> 4
;;; ＠
;;; find-package                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : find-package name
;;; name （文字列またはシンボル）をパッケージ名とするパッケージがシステム内
;;; に存在していれば、その名称を返し、なければ nil を返す。
;;; パッケージの検索では、大文字と小文字の差は識別する。
;;; 
;;; <例>
;;;         (find-package "bas") -> {vector}32228(package .12)
;;;         (find-package "suzuki") -> {vector}57222(package .12)
;;; ＠
;;; find-position-in-list                  関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : find-position-in-list item list
;;; list において item と eq な要素があれば、その最初の要素の位置を示す
;;; 番号を、なければ nil を返す。 要素の位置は 0 から数える。
;;; 
;;; <例>
;;;         (find-position-in-list 'a '(a b c d)) -> 0
;;;         (find-position-in-list 'c '(a b c d)) -> 2
;;; ＠
;;; find-position-in-list-equal            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : find-position-in-list-equal item list
;;; list において item と equal な要素があれば、その最初の要素の位置を示す
;;; 番号を、なければ nil を返す。 要素の位置は 0 から数える。
;;; 
;;; <例>
;;;         (find-position-in-list-equal 'c '(a b c d)) -> 2
;;;         (find-position-in-list-equal '(c d) '(a b (c d) e)) -> 2
;;; ＠
;;; find-symbol                            関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : find-symbol name &opt package
;;; package (既定値はカレントパッケージ) にリンクされたすべてのパッケージの
;;; 中に印字名 name を持つシンボルがあれば、name を第 1 の値、第 2 の値とし
;;; て次のどれかを返す。
;;;   :internal    name は package の内部シンボル
;;;   :external    name は package の外部シンボル
;;;   :inherited   name は package がユースしているパッケージの外部シンボル
;;; なければ nil を返す。 
;;; package がパッケージ "bas" なら、ユーザパッケージは捜さない。
;;; 新しくシンボルを作成したり、パッケージを変更したりしない点を除けば、
;;; intern と同じ。
;;; 
;;; <例>
;;;         (find-symbol 'car) -> !(car :inherited)
;;;         (find-symbol 'write) -> !(write :inherited)
;;; ＠
;;; find-symbol-local                      関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : find-symbol-local name &opt package
;;; package（既定値はカレントパッケージ）に、印字名 name を持つシンボルが
;;; あれば name を返し、それ以外なら nil を返す。
;;; package のサブパッケージや親パッケージは検索しない。
;;; 
;;; <例>
;;;         (find-symbol-local "car") -> nil
;;;         x -> エラー
;;;         (find-symbol-local "x") -> x
;;; ＠
;;; finish-output                          関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : finish-output &opt stream
;;; stream に結合されたバッファの内容を全て出力し、nil を返す。
;;; 既定値は、変数 *standard-output* の値。
;;; ＠
;;; first                                  関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : first list
;;; list の最初の要素の値を返す。
;;; 
;;; <例>
;;;         (first '(0 1 2 3)) -> 0
;;; ＠
;;; firstn                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : firstn n list
;;; list を元に、長さ n のリストを作成し返す。
;;; 作成するリストの長さが元のリストより小さいか等しい時、作成されるリスト
;;; の要素は list の最初から n 番目までの要素と同じになる。
;;; 作成するリストが元のリストより大きい時、追加した要素には nil を代入する。
;;; 
;;; <例>
;;;         (firstn 3 (list 1 2 3 4 5 6 7)) -> (1 2 3)
;;;         (firstn 0 (list 1 2 3)) -> nil
;;;         (firstn 5 (list 1 2 3)) -> (1 2 3 nil nil)
;;; ＠
;;; fix-all-dir                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fix-all-dir
;;; 全ディレクトリに対してメインメモリ中にあるファイル制御情報をディスク
;;; にコピーして 'ok' を返す。(全ディレクトリに対して fix-dir を行う)
;;; 
;;; <例>
;;;         (fix-all-dir) -> ok
;;; ＠
;;; fix-dir                                関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fix-dir &opt dir
;;; メインメモリ中にあるファイル制御情報をディスクにコピーして 'ok' を返す。
;;; dir の既定値はカレントディレクトリ。
;;; 
;;; <例>
;;;         (fix-dir) -> ok
;;;         (fix-dir "bs:<dir1>") -> ok
;;; ＠
;;; fixp                                   関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : fixp number
;;; number が整数 (shortnum または bignum) なら、その評価値を返し、
;;; それ以外なら nil を返す。
;;; (fixp x) = (integerp x)
;;; ＠

(defun flatsize (obj)
  "flatsize                               関数[#!subr]

<説明>
  形式 : flatsize object
フォーム (write-to-string object) の評価結果として作られる文字列の長さ
を返す。ただし flatsize は、文字列を作らない。
 (flatsize x) = (string-length (write-to-string x))

<例>
        (flatsize (list 1 2 3 4)) -> 9
        (flatsize nil) -> 3
        (flatsize (index 10 90)) -> 244 (実験では247になったが…)"
  (length (write-to-string obj)))

;;; ＠
;;; flet                                   関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : flet ((f-name1 (arg11 arg12 ...) body1)
;;;                (f-name2 (arg21 arg22 ...) body2)
;;;                ... )
;;;               form1 form2 ...
;;; ローカル関数を定義し、実行する。
;;; f-name1 f-name2 ... を関数名、arg11 arg12 ...、arg21 arg22 ...、...
;;; を引数、body1 body2 ...を関数本体とするローカル関数 (スコープ透過関数)
;;; を定義し、フォーム form1 form2 ... を逐次評価し、最後のフォームの評価
;;; 結果を返す。これらのフォームが全く省略されると nil を返す。
;;; 定義された関数は、関数 dye によって定義されたと同じように動く。しかし、
;;; それらのスコープは form1 form2 ... に制限される。ゆえに、ローカル変数
;;; arg11 arg12 arg21 arg22 ... は flet の外で値を取ることはできないし、
;;; 定義されたローカル関数の関数本体で、flet の外で定義されたグローバルな
;;; 関数を参照できる。
;;; 定義されたローカル関数は、それらの関数本体で用いることはできない。
;;; スコープを除いて、関数 labels と同じ。
;;; 
;;; <例>
;;;         (defun f00 (x) (1+ x)) -> f00
;;;         (flet ((f00 (x) (1- x))) (f00 10)) -> 9
;;;         (f00 10) -> 11
;;;         (flet ((f00 (x) (1- x))
;;;                (bar (x) (f00 x))) (bar 10)) -> 11
;;;         (flet ((f1 (x y) (x + y))
;;;                (g1 (a b) (a * b)))
;;;               ((f1 10 20) - (g1 3 5))) -> 15
;;;         (flet ((h1 (x y) (x + y + p)))
;;;                (let (p) (!p 30) (h1 10 20))) -> 60
;;;         (flet ((f1 (x y) (x + y))
;;;                (g1 (a b) (+ (a * b) (f1 10 20))))
;;;                (g1 3 5)) -> エラー
;;; ＠
;;; float                                  関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : float number1 &opt number2
;;; number1 (複素数でないオブジェクト) を、浮動小数点数 number2 と同一の型
;;; ( number2 が省略された場合は single-float 形式) の浮動小数点数に変換し、
;;; その結果を返す。
;;; 
;;; <例>
;;;         (float 0) -> 0.0f0
;;;         (float -2) -> 2.3000305175781f0
;;;         (float 3.56) -> 3.5599975859376f0
;;; ＠
;;; float-digits                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : float-digits number
;;; 浮動小数点数 number の内部表現で、仮数部を表現する b 進法の桁数を非負
;;; の整数として返す。 b は基数。
;;; 
;;; <例>
;;;         (float-digits 0.0f0) -> 53
;;;         (float-digits -2.3f0) -> 53
;;;         (float-digits 1.23f0) -> 53
;;; ＠
;;; float-locative-arrays 未インプリメント 関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : float-locative-arrays &rest array-spec
;;; 要素のデータ型が floating-point-locative (浮動小数点ロカティブ) である
;;; 配列を生成し、配列仕様 array-spec のリストを返す。
;;; array-spec は (var dimension) の形式。 var は生成される配列の名前、
;;; dimensions はその配列の次元を表す整数のリスト。
;;; array 参照。
;;; ＠
;;; float-locatives       未インプリメント 関数[#!exprdyn]
;;; 
;;; <説明>
;;;   形式 : float-locatives &rest var1 var2 ... varN
;;; N 個の 64 ビット浮動小数点ロカティブを生成し、それらを変数 var1 var2
;;; ... varN に代入し、リスト (var1 var2 ... varN) を返す。
;;; ＠
;;; float-precision                        関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : float-precision number
;;; 浮動小数点数 number の内部表現で、仮数部を表現する b 進法の桁数を
;;; 非負の整数として返す。 b は基数。
;;; number が正規化されていないか 0 の場合は、表現された桁数よりも精度は
;;; 悪くなる。正規化されている場合は、float-digits と同じ。
;;; 
;;; <例>
;;;         (float-precision 0.0f0) -> 0
;;;         (float-precision -2.3f0) -> 53
;;;         (float-precision 1.23f0) -> 53
;;; ＠
;;; float-radix                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : float-radix number
;;; 浮動小数点数 number の基数を整数形式で返す。
;;; 
;;; <例>
;;;         (float-radix 9.23f0) -> 2
;;;         (float-radix -9.23f0) -> 2
;;; ＠
;;; float-sign                             関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : float-sign number1 &opt number2
;;; 浮動小数点数 number1 と同じ符号で、number2 と同じ絶対値を持つ浮動小数点
;;; 数を生成して返す。
;;; 
;;; <例>
;;;         (float-sign -1.0f0) -> -1.0f0
;;;         (float-sign -4.0f0 6.5f0) -> -6.5f0
;;;         (float-sign 4.0f0 6.5f0) -> 6.5f0
;;; ＠
;;; floatp                                 関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : floatp object
;;; object が 64bit の浮動小数点数ならば、その評価値を返し、それ以外なら
;;; nil を返す。
;;; 
;;; <例>
;;;         (floatp 1.23) -> 1.23
;;;         (floatp 123) -> nil
;;;         (floatp (sqrt 2)) -> 1.4142135623731f0
;;; ＠
;;; floor                                  関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : floor number1 &opt number2
;;; number1 を number2 (既定値は 1) で割った結果の値を超えない最大の整数を
;;; 第 1 の値として、第 2 の値として (- number1 (* 第 1 の値 number2) を
;;; 返す。
;;; 
;;; <例>
;;;         (floor -1.24) -> !(-2 0.760002)
;;;         (floor -2.4) -> !(2 0.399993)
;;;         (floor 0.0) -> !(0 0.0)
;;; ＠
;;; fmakunbound                            関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fmakunbound symbol
;;; 関数定義 symbol を無効にし、symbol を返す。
;;; 
;;; <例>
;;;         (de f (x) (car x)) -> f
;;;         (applobj-of 'f) -> {applobj}66454(#!expr-simple . 6)
;;;         (fmakunbound 'f) -> f
;;;         (applobj-of 'f) -> nil
;;; ＠

(defmacro for (var list &body body)
  "for                                    関数[#!subr]

<説明>
  形式 : for var list form1 form2  ... 
form1 form2 ... を var を使って順に実行する。 var は list の各要素に
逐次束縛されたものである。 form1 form2 ... は list の長さと同じ回数評価
される。 nil を返す。

<例>
        (for i (index 60 80) (prins i)) -> <=>?@ABCDEFGHIJKLMNOP"
  `(dolist (,var ,list nil)
     ,@body))

#|
 (for i (index 60 80) (print i))
|#

;;;         				  nil
;;; ＠
;;; force-output                           関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : force-output &opt stream
;;; stream (既定値は *standard-output* の値) と結合されたバッファの内容を
;;; 全て出力することをシステムに要求し、その完了を待たずに nil を返す。
;;; ＠
;;; format                                 関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : format destination string &rest arg1 arg2 ... argN
;;; フォーマットに従った出力を生成する。
;;; destination で出力の送り先を指定する。この値が nil なら出力の結果がそ
;;; のまま文字列になって返される。t なら出力は変数 *standard-output* の値
;;; へ送られる。ストリームなら出力はそのストリームへ送られる。
;;; destination が nil の場合以外は、nil を返す。
;;; string で出力を制御するための文字列を指定する。この文字列に従って、
;;; arg1 arg2 ... argN がフォーマット出力される。
;;; string の中に 〜 で始まるフォーマット命令をいれることにより、どんな書式
;;; で出力されるかが指定される。
;;; フォーマット命令には以下のようなものがある。
;;; 
;;; 〜s  〜S  〜a  〜A
;;; arg1 arg2 ... argN を出力ストリーム上に書く。それらの引数は消費される。
;;; 〜s、または〜Sの場合は、:escape が nil 以外で、〜a、または 〜A の場合は、
;;; :escape が nil で出力が行われる。
;;; 
;;; 〜r  〜R  〜b  〜B  〜o  〜O  〜d  〜D  〜x  〜X
;;; 指定された基数で、arg1 arg2 ... argN を出力する。それらの引数は消費
;;; される。また、各引数は整数でなければならない。〜d、〜D は 10 進で、〜o、
;;; 〜O は 8 進で、〜b、〜B は 2 進で、〜x、〜X は 16 進。
;;; 
;;; <例>
;;;         (format nil "foo") -> "foo"
;;;         (!x 5) -> 5
;;;         (format nil "The answer is ‾D." x) -> "The answer is 5."
;;;         (format nil "The answer is ‾3D." x) -> "The answer is    5."
;;;         (!y "elephant") -> "elephant"
;;;         (format nil "Look at the ‾A!" y) -> "Look at the elephant!"
;;;         (format nil "Type ‾:C to ‾A."
;;;         	    (set-char-bit #¥D :control t)
;;;         	    "delete all your files.")
;;;         	    -> "Type ^D to delete all your files."
;;; ＠

(defmacro forn (&body binds-and-form)
  "forn                                   関数[#!macro]

<説明>
  形式 : forn (var1 list1) (var2 list2) ...  (varN listN) form 
変数 var1 var2  ... を使って form を評価し、その値を返す。
変数は list1 list2 ... の中の各要素に連続して束縛されたものである。"
  (let* ((binds (butlast binds-and-form))
	 (form (last binds-and-form))
	 (vars (mapcar #'car binds))
	 (do-binds (mapcar (lambda (x) `(,@x (cdr ,(car x)))) binds)))
    `(do ,do-binds 
	 ((some #'endp (list ,@vars)))
       (let ,(mapcar (lambda (x) `(,x (car ,x))) vars)
	 ,@form))))

;; まちがっていたので修正bind部は纏められず、最後は複数フォームを取らないのが、taoの〜n形式らしい。
;; 一番短いリストに合わせて終了しているが、それで良いのか。説明に書いてないので分からん。

#|
 (defmacro forn (binds &body body)
  "forn                                   関数[#!macro]

<説明>
  形式 : forn (var1 list1) (var2 list2) ...  (varN listN) form 
変数 var1 var2  ... を使って form を評価し、その値を返す。
変数は list1 list2 ... の中の各要素に連続して束縛されたものである。"
  (let* ((do-binds (mapcar (lambda (x) `(,(car x) ,(cadr x) (cdr ,(car x)))) binds))
	 (vars (mapcar #'car do-binds)))
    `(do ,do-binds 
	 ((some #'endp (list ,@vars)))
       (let ,(mapcar (lambda (x) `(,x (car ,x))) vars)
	 ,@body))))
|#

#|
 (forn ((i '(foo bar baz))
       (q (index 10 100))
       (j (reverse '(foo bar baz))))
  (print (list i j q)))
|#


;;; ＠
;;; fourth                                 関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : fourth list
;;; list の 4 番目の要素の値を返す。
;;; (fourth x) = (cadddr x) = (nth x 3) 
;;; 
;;; <例>
;;;         (fourth '(0 1 2 3 4 5)) -> 3
;;; ＠
;;; fresh-line                             関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fresh-line &opt stream
;;; stream がすでに行の先頭にあるかないかを判断出来ない場合は、stream に
;;; 改行を出力し、t を (そうでなければ nil を) 返す。
;;; stream が省略されると、変数 *standard-output* の値が使われる。
;;; crlf と同じ。terpri 参照。
;;; 
;;; <例>
;;;         ((prin1 'a) (fresh-line) (fresh-line) (prin1 'b) (fresh-line))
;;;         ->
;;;         a
;;;         改行
;;;         b
;;;         t 
;;; ＠
;;; fround                                 関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : fround number1 &opt number2
;;; number1 を number2 (既定値は 1)で割った結果の小数点以下を四捨五入した
;;; 値を浮動小数点形式で第 1 の値として返す。
;;; 第 2 の値として (- number1 (* 第 1 の値 number2)) を返す。
;;; 
;;; <例>
;;;         (fround 5.3) -> !(5.0 0.299988)
;;;         (fround 5.4) -> !(5.0 0.399988)
;;;         (fround 5.5) -> !(6.0 -0.50)
;;;         (fround 5.6) -> !(6.0 -0.399988)
;;; ＠
;;; ftruncate                              関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : ftruncate number1 &opt number2
;;; number1 を number2 (既定値は 1)で割った結果の小数点以下を切り捨てた値
;;; を浮動小数点形式で第 1 の値として返す。
;;; 第 2 の値として (- number1 (* 第 1 の値 number2)) を返す。
;;; 
;;; <例>
;;;         (ftruncate 5.1) -> !(5.0 0.100007)
;;;         (ftruncate 5.2) -> !(5.0 0.200012)
;;;         (ftruncate 5.3) -> !(5.0 0.299988)
;;;         (ftruncate 5.4) -> !(5.0 0.399993)
;;;         (ftruncate 5.5) -> !(5.0 0.50)
;;; ＠
;;; funcall                                関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : funcall func &rest arg1 arg2 ... argN
;;; arg1 arg2 ... argN を起動パラメータにして関数 func を起動する。
;;; (funcall fn arg1 arg2 ...) = (apply* fn arg1 arg2 ...)
;;; 
;;; <例>
;;;         (funcall '+ 1 2 3) -> 6
;;;         (apply 'funcall '(cons a b)) -> エラー (a と b が評価される)
;;; ＠
;;; common:funcall                         関数[#!macro]
;;; 
;;; <説明>
;;;   形式 : common:funcall func &rest arg1 arg2 ... argN
;;; arg1 arg2 ... argN を起動パラメータにして関数 fn を起動する。
;;; 
;;; <例>
;;;         (common:funcall '+ 1 2 3) -> 6
;;;         (apply 'common:funcall '(cons a b)) -> (a . b)
;;; ＠
;;; funcall-init                           関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : funcall-init &rest 'x
;;; Common Lisp ではオプショナル変数や補助変数などの初期値を関数 lambda, 
;;; defun, defmacro などの中に設定することができるが、TAO ではできないので、
;;; この初期化を seq と等しい funcall-init を用いていくつかの形に書き換える。
;;; 
;;; <例>
;;;         (defun foo (&optional (x 10)) (list x)) -> foo
;;;         definition foo -> (defun foo (&opt x)
;;;         			     (funcall-init
;;;         			      (ifundef x (sys:eval-uv x 10)))
;;;         			     (list x))
;;; ＠
;;; function                               関数[#!subr]
;;; 
;;; <説明>
;;;   形式 : function func
;;; func がシンボル ---> func を名前とする関数の定義を返す。
;;; func がラムダ式 ---> クロージャを生成して返す。
;;; (function 'func ...) = (#'func ...)
;;; 
;;; <例>
;;;         (funcall #'cons 1 2) -> (1 . 2)
;;;         [(funcall function cons 1 2)]
;;;         (defun foo (x) (1+ x) ) -> foo
;;;         (flet ((foo (x) (1- x)))
;;;               (funcall #'foo 10)) -> 9
;;;                (#'foo は局所関数 foo を呼び出す)
;;;         (flet ((foo (x) (1- x)))
;;;               (funcall 'foo 10)) -> 11
;;;                ('foo は大域関数 foo を呼び出す)
;;; ＠
;;; functionp                              関数[#!expr]
;;; 
;;; <説明>
;;;   形式 : functionp object
;;; object が関数オブジェクト (applobj) と結び付けられた識別子なら、
;;; その関数オブジェクト名を返し、それ以外なら nil を返す。
;;; 
;;; <例>
;;;         (functionp '+) -> {applobj}12143(#!subr . 6)
;;;         (functionp (lambda (x) (car x))) ->
;;;                         {applobj}19376(#!exprdyn-simple . 6)
;;;         (functionp 123) -> nil
;;; ＠
;;; fundamental                            関数[#!expr]
;;; 
;;; <説明>
;;;   ファンダメンタルストリームに *standard-input* と *standard-output* 
;;; を割り当てる。
;;; ＠
;;; fundamental-stream                     クラス
;;; 
;;; <説明>
;;;   インスタンスがファンダメンタルストリームであるクラス。
;;; ノーマルなストリームであるいかなるストリームも fundamental-stream の
;;; サブクラス。
;;; file-stream , pipe-stream 等は fundamental-stream のサブクラス。
;;; ＠
