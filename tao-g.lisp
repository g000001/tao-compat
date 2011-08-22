(in-package #:tao-internal)
(in-readtable :tao)
;;; ＠
;;; gc                                     関数[#!subr]
;;;
;;; <説明>
;;;   ガーベジコレクタを直ちに起動する。
;;; ＠
;;; gcd                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : gcd &rest integer1 integer2 ... integerN
;;; 引数の最大公約数を非負の整数形式で返す。
;;; 引数が 1 つの場合はその値の絶対値、何も指定されない場合は 0 を返す。
;;; 引数は shortnum の整数。
;;;
;;; <例>
;;;         (gcd 0 2 4 6) -> 2
;;;         (gcd -12 -4 0 2 4 6) -> 2
;;;         (gcd 3 4) -> 1
;;; ＠
;;; common:gcd                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:gcd &rest integer1 integer2 ... integerN
;;; 引数の最大公約数を非負の整数形式で返す。
;;; 引数が 1 つの場合はその値の絶対値、何も指定されない場合は 0 を返す。
;;; shortnum と bignum の両方を引数として指定可能。
;;; ＠
;;; gcdr                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : gcdr g-list
;;; 成長リスト g-list から、その第 1 要素を取り除いたリスト (cdr部) を返す。
;;; 返された cdr 部も成長リスト。
;;;
;;; <例>
;;;         grw が成長リスト (1 2 3 4 5) ならば
;;;         (!grw (gcdr grw)) -> (2 3 4 5)
;;;         (!grw (tcons grw 6)) -> (2 3 4 5 6)
;;;         (!grw (cdr grw)) -> (3 4 5 6)
;;;         (!grw (tcons grw 7)) -> (3 4 5 6 7)
;;;         grw はもはや成長リストではないから、この演算は前の例に較べると
;;;         少し遅い。
;;; ＠
;;; gensym                                 関数[#!closure]
;;;
;;; <説明>
;;;   形式 : gensym &opt object
;;; シンボルを新しく作り、先頭に #: をつけて返す（#: は新たに作られた
;;; シンボルがインターンされていないことを意味する）。
;;; 新たに作られたシンボルの印字名は接頭文字列と生成番号で構成される。
;;; object が整数なら生成番号をその値にセットする。
;;; object が文字列なら、それがそれ以降の接頭文字列となる。
;;;
;;; <例>
;;;         (gensym) -> #:g1
;;;         (gensym) -> #:g2
;;;         (gensym) -> #:g3
;;;         (gensym 'chomchom) -> #:chomchom4
;;;         (gensym) -> #:chomchom5
;;;         (gensym) -> #:chomchom6
;;;         (gensym 10) -> #:chomchom10
;;;         (gensym) -> #:chomchom11
;;;         (gensym "hema") -> #:hema12
;;; ＠
;;; gentemp                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : gentemp &opt prefix package
;;; 印字名が prefix で始まり、 package 内でユニークなシンボルを生成し、
;;; インターンする。
;;; 新たに作られたシンボルの印字名は接頭文字列と生成番号で構成される。
;;; prefix、package の既定値は、各々 t、カレントパッケージ名。
;;; 作成されたシンボルがインターンされる点を除けば、関数 gensym と同じ。
;;;
;;; <例>
;;;         (gentemp) -> t1000
;;;         (gentemp) -> t1001
;;;         (gentemp 'capi) -> capi1000
;;;         (gentemp) -> t1002
;;;         (gentemp) -> t1003
;;;         (gentemp 'capi) -> capi1001
;;;         (gentemp "capi") -> capi1002
;;;         (gentemp) -> t1004
;;; ＠
;;; get                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : get symbol ind &opt ret-val
;;; symbol の属性リスト中のインディケータに、 ind と一致するものがあれば、
;;; そのインディケータに対応する値 (属性値) を返す。なければ、 ret-val
;;; (既定値は nil)  を返す。 symbol は、属性リスト、識別子、交代リスト、
;;; 関数オブジェクト、クラスベクタ、パッケージのうちどれか。
;;; 関数 ! または関数 setf と組み合わせて、インディケータの属性値をセット
;;; するためにも使用する。
;;;
;;; <例>
;;;         (!(plist 'xxx) '(a 1 b 2 c (3 4) d nil)) ->
;;;         (a 1 b 2 c (3 4) d nil)
;;;         (get 'xxx 'a) -> 1
;;;         (get 'xxx 'c) -> (3 4)
;;;         (get 'xxx 'd) -> nil
;;;         (get 'xxx 'e) -> nil
;;;         (!(get 'ntt 'foo) 'bar) = (putprop 'ntt 'foo 'bar)
;;; ＠
;;; get-decoded-time                       関数[#!expr]
;;;
;;; <説明>
;;;   現在の時刻について、秒、分、時、日付、月、年、週日、夏時間かどうか
;;; (nil でなければ夏時間) とタイムゾーンの 9 つの値をデコーデッドタイム
;;; 形式で返す。
;;; タイムゾーンとは、グリニッチ標準時の西での時間数として表される整数。
;;; 例えば、マサチューセッツでは 5 で、カリフォルニアでは 8、日本では -9 。
;;;
;;; <例>
;;;         (get-decoded-time) -> 59
;;;         (multiple-value-list (get-decoded-time)) ->
;;;         			 (35 44 11 4 7 1985 4 nil-9)
;;;         (multiple-value-setq (sec min hou dat mon yea day dli tim)
;;;         	(get-decoded-time)) -> 23
;;;         sec -> 23,  min -> 45,  hou -> 11,  dat -> 4,  mon -> 7,
;;;         yea -> 1985, day -> 4,  dli -> nil,  tim -> -9
;;; ＠
;;; get-dispatch-macro-character           関数[#!expr]
;;;
;;; <説明>
;;;   形式: get-dispatch-macro-character disp-char sub-char &opt readtable
;;; 読み込み表 readtable にディスパッチ文字として登録されている文字
;;; disp-char の後の文字 sub-char に対するマクロ文字関数を返す。
;;; sub-char に関連付けられた関数がなければ nil を返す。
;;; disp-char が実際に指定された読み込み表中のディスパッチ文字でない場合
;;; にはエラーが警告される。
;;; set-dispatch-macro-charactor make-dispatch-macro-charactor
;;; set-macro-charactor 参照。
;;; ＠
;;; get-handler-for                        メッセージ
;;;
;;; <説明>
;;;   形式 : get-handler-for msg &opt type
;;; get-handler-for のレシーバは、メッセージ msg 、そして与えられている
;;; ならばメソッドタイプ type のメソッドのボディを返す。
;;; ＠
;;; get-internal-real-time                 関数[#!expr]
;;;
;;; <説明>
;;;   運転時間を、内部時間形式の単整数つまりインターナル・タイム・ユニット
;;; の数で返す。計測は、システムがスタートする時に始まる。
;;; 定数 internal-time-units-per-second 参照。
;;;
;;; <例>
;;;         (get-internal-real-time) -> 23550
;;; ＠
;;; get-internal-run-time                  関数[#!expr]
;;;
;;; <説明>
;;;   各ユーザの cpu 実行時間を、内部時間形式の単整数つまりインターナル・
;;; タイム・ユニットの数で返す。計測は、ユーザのログインにより始まる。
;;; 定数 internal-time-units-per-second 参照。
;;;
;;; <例>
;;;         (get-internal-run-time) -> 121
;;; ＠
;;; get-kanji-code                         関数[#!subr]
;;;
;;; <説明>
;;;   形式 : get-kanji-code jcharactor
;;; 2 バイト文字 jcharactor のキャラクタコードを返す。返されるコードの
;;; 7 ビット目と 15 ビット目は、0 である。
;;; ＠
;;; get-macro-character                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : get-macro-character char &opt readtable
;;; 文字 char が読み込み表 readtable にマクロ文字として登録されていれば、
;;; その入力マクロ定義を返す。第 2 の値として文字 char が区切り記号かどうか
;;; のフラグ（区切り記号なら nil、そうでなければ nil 以外）を返す。
;;; 文字 char がマクロ文字構文を持たないときは nil を返す。
;;; readtable の既定値は現在の読み込み表 (変数 *readtable* の値) 。
;;; set-macro-charactor 参照。
;;; ＠
;;; get-memblk                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : get-memblk memblk-type size
;;; 型 (メモリブロックにおける 1 語のビット数) memblk-type 、及び大きさ
;;; size のメモリブロックを確保し、それを返す。
;;; memblk-type で指定できる型は次のうちのいずれか。
;;;   #!1b-memblk   #!2b-memblk   #!4b-memblk   #!8b-memblk
;;;   #!16b-memblk  #!32b-memblk  #!64b-memblk
;;;
;;; <例>
;;;         (!a (get-memblk #!8b-memblk 16)) ->
;;;                    {memblk}480764(#!8b-memblk . {dnil}16)
;;;         a は新たに確保したメモリブロックを指すポインタ。
;;; ＠
;;; get-output-stream-string               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : get-output-stream-string stream
;;; 文字列出力ストリーム stream への出力を文字列として取り出す。同じ文字列
;;; 出力ストリームにこの関数が既に適用されていれば、最後の適用以降の出力が
;;; 取り出される。
;;; 文字列出力ストリームは関数 make-string-output-stream によって生成される。
;;; ＠
;;; get-properties                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : get-properties symbol list
;;; symbol の属性リスト中に、list の要素の 1 つと eq なインディケータがあれ
;;; ば、次の 3 つの値を返す。
;;; 1) list の要素の 1 つと 1 番最初に eq になったインディケータ
;;; 2) そのインディケータの対応する値 (属性値)
;;; 3) そしてこのインディケータが car 部となるような属性リストの tail
;;; 見つからなければ nil を返す。
;;; Common  Lisp では第 1 引数は属性リストでなくてはならないが、TAO では
;;; 識別子でもかまわない。
;;; リターン値を除いて getl 関数とほぼ同じ。
;;;
;;; <例>
;;;         (!(plist 'aaa) '(p 1 q 2 r 3)) -> (p 1 q 2 r 3)
;;;         (get-properties 'aaa '(r q)) -> !(q 2 (q 2 r 3))
;;;         (get-properties 'aaa '(x y)) -> !(nil nil nil)
;;; ＠
;;; get-setf-method                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : get-setf-method form
;;; 関数 setf が form に対してどのように働くのかを示す 5 つの値のリスト
;;; を返す。以下に 5 つの値を示す。
;;; 1) 関数 gensym または gentemp によって生成される一時的な変数のリスト。
;;;    これらの変数は form 内のアクセス関数 access-fn の引数に対応。
;;; 2) 上記の一時的な変数に対応する式のリスト
;;;    このリストの長さは上記のリストの長さと同じ。
;;;    一時的な変数は関数 setf の実行中、対応する式の値に束縛される。
;;; 3) form が供給している指定された場所に格納される変数のリスト。
;;; 4) 上記の格納変数の値を form が供給する場所にどう格納するかを示す式。
;;; 5) 新しい値 (格納変数の値) を格納する場所を供給する式。
;;;
;;; <例>
;;;         (get-setf-method 'x) -> (() () (#:g1) (setq x #:g1) x)
;;;         (get-setf-method '(car x))
;;;         	 -> ((#:g2) (x) *micro-code* (car #:g2))
;;; ＠
;;; get-setf-method-multiple-value         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : get-setf-method-multiple-value form
;;; 関数 setf が form に対してどのように働くのかを示す 5 つの値のリストを
;;; 返す。以下に 5 つの値を示す。
;;; 1) 関数 gensym または gentemp によって生成される一時的な変数のリスト。
;;;    これらの変数は form 内のアクセス関数 access-fn の引数に対応。
;;; 2) 上記の一時的な変数に対応する式のリスト
;;;    このリストの長さは上記のリストの長さと同じ。
;;;    一時的な変数は関数 setf の実行中、対応する式の値に束縛される。
;;; 3) form が供給している指定された場所に格納される変数のリスト。
;;; 4) 上記の格納変数の値を form が供給する場所にどう格納するかを示す式。
;;; 5) 新しい値 (格納変数の値) を格納する場所を供給する式。
;;; 格納変数の数をチェックしない点を除いて、関数 get-setf-method と同じ。
;;;
;;; <例>
;;;         (get-setf-method-multiple-value 'x)
;;;               -> (() () (#:g1) (setq x #:g1) x)
;;;         (get-setf-method-multiple-value '(car x))
;;;               -> ((#:g4) (x) (#:g5) *micro-code* (car #:g4))
;;; ＠
;;; get-sysmode                            関数[#!expr]
;;;
;;; <説明>
;;;   システムモードの現在の状態を示す。
;;; bas:sysmode sys:default-sysmode 参照。
;;;
;;; <例>
;;;         (get-sysmode) ->
;;;         (#4 :car-nil-error nil :cdr-nil-error nil :one-char-string nil
;;;             :common-lisp nil :negation-as-failure nil)
;;; ＠
;;; get-universal-time                     関数[#!expr]
;;;
;;; <説明>
;;;   現在の時刻を、ユニバーサルタイム形式で返す。
;;; ユニバーサルタイムは、グリニッチ標準時 1900 年 1 月 1 日の 0 時 0 分
;;; 0 秒からの秒数を表す。
;;;
;;; <例>
;;;         (get-universal-time) -> 2698255881
;;; ＠
;;; getcharn                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : getcharn string n
;;; string 中の n 番目の文字を shortnum と見なし、その値を返す。
;;; (最初の文字が 1 番目)。
;;;
;;; <例>
;;;         (getcharn "abcd" 1) -> 97
;;;         (getcharn 'Abcd 1) -> 65
;;;         (getcharn 'abcd 10) -> nil
;;; ＠
;;; getf                                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 : getf place ind &opt ret-val
;;; place (属性リスト又は識別子) 内に、 ind と eq なインディケータがあれば
;;; それに対応する値 (属性値) を、なければ ret-val (省略された場合は nil)
;;; を返す。 get 関数と全く同じであり、Common Lisp の getf 関数機能を包含し
;;; ている。Common Lisp では第 1 引数は属性リストでなくてはならないが、TAO
;;; では識別子でもよい。
;;;
;;; <例>
;;;         (!(plist 'xxx) '(q 2 r 3 s 4)) -> (q 2 r 3 s 4)
;;;         (getf 'xxx 'q) -> 2
;;;         (getf 'xxx 's) -> 4
;;;         (getf 'xxx 'u) -> nil
;;; ＠
;;; gethash                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : gethash key table &optn default
;;; ハッシュ表 table において key を有するエントリがあれば、エントリの内容
;;; と t を返す。なかった場合は default と nil ( default が指定されていな
;;; い場合は nil だけ) を返す。
;;;
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}1804146(hash-table . 8)
;;;         (!(gethash 'color a) 'brown) -> brown
;;;         (gethash 'color a) -> !(brown t)
;;; ＠
;;; getl                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : getl symbol list
;;; symbol の属性リスト中に、list の要素の 1 つと eq なインディケータが
;;; あれば、そのインディケータが先頭となる属性リストの後半部を返す。
;;; なければ nil を返す。
;;; symbol は次のうちのどれか ;  識別子、属性リスト、ベクタ、交代リスト、
;;; 関数オブジェクト、クラスベクタ、パッケージ
;;;
;;; <例>
;;;         aa の属性リストが  (a 1 b 2 c 3 d 4 e 5)  なら
;;;         (getl 'aa '(e c n)) -> (c 3 d 4 e 5)
;;;         (getl 'aa '(d)) -> (d 4 e 5)
;;;         (getl 'aa '(o w k)) -> nil
;;; ＠
;;; global-package                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : global-package package
;;; package から大域パッケージへのポインタを返す。
;;;
;;; <例>
;;;           (global-package "apropos") -> {vector}29724(package . 10)
;;; ＠
;;; go                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : go tag
;;; prog 本体内で、 tag へジャンプする。
;;;
;;; <例>
;;;         (de member (item list)
;;;             (prog (more)
;;;         	  (!more list)
;;;            	 loop
;;;         	  (cond ((null more)
;;;         		 (return nil))
;;;         		(eql item (car more))
;;;         		 (return more)))
;;;         	  (!!cdr !more)
;;;         	 (go loop)))
;;; ＠
;;; goal                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : goal &rest clause
;;; clause で指定した問題の解として、満足する局所変数の値がプリントされ、
;;; バックトラックが繰り返される。TAO のトップレベルで使う。
;;; ユーザが、プリントされた値の後にセミコロン (;) を入力すると、
;;; バックトラックが行なわれ、次の解としての値か no のどちらかが返る。no
;;; は、clause で指定した問題の解として満足する局所変数の値がないことを意味
;;; する。ユーザが、<cr> を入力すれば、yes か no だけが返る。yes は、
;;; clause で指定した問題の解として満足する局所変数の他の値があることを意味
;;; する。
;;;
;;; <例>
;;; (assert (concatenate (_a . _x) _y (_a _z)) (concatenate _x _y _z) )
;;; (assertz (concatenate () _x _x) )
;;; (goal (concatenate _x _y (1 2 3))) ->
;;; _y = nil
;;; _x = (1 2 3) ;
;;; [(;) は、そのフォームを失敗させバックトラックを行わせる]
;;; _y = (3)
;;; _x = (1 2) ;
;;; _y = (2 3)
;;; _x = (1) ;
;;; _y = (1 2 3)
;;; _x = () ;
;;; no
;;; _x = ...  と _y = ...  のペアが出力されるとセミコロン ; は、
;;; ユーザにより入力される。そして no はリターン値。
;;; ＠
;;; goal-all                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : goal-all &rest clause
;;; セミコロンの入力なしにバックトラックを自動的におこすこと以外は
;;; 関数 goal と同じ。つまり clause で指定した問題の解として満足する
;;; 局所変数の全ての値をプリントする。返される値は、that's-all 。
;;; TAO のトップレベルで使う。
;;;
;;; <例>
;;; (assert (concatenate (_a . _x) _y (_a _z)) (concatenate _x _y _z) )
;;; (assertz (concatenate () _x _x) )
;;; (goal-all (concatenate _x _y (1 2 3))) ->
;;; _y = nil
;;; _x = (1 2 3)
;;; _y = (3)
;;; _x = (1 2)
;;; _y = (2 3)
;;; _x = (1)
;;; _y = (1 2 3)
;;; _x = ()
;;;         that's-all
;;; ＠
;;; goal-all-list                          関数[#macro]
;;;
;;; <説明>
;;;   形式 : goal-all-list arg &rest x
;;; goal-all と同じように働くが、 x 節内にある論理変数 arg に対する結合され
;;; た値のリストを返す。
;;;
;;; <例>
;;;         (asserta (concat () _x _x)) -> concat
;;;         (asserta (concat (_a . _x) _y (_a . _z)) (concat _x _y _z))
;;;         						-> concat
;;;         (goal-all-list _x (concat _x _y (1 2))) -> (() (1) (1 2))
;;; ＠
;;; graphic-char-p                         関数[#!subr]
;;;
;;; <説明>
;;;   形式 : graphic-char-p char
;;; 文字 char が、印字文字であればその印字文字を、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (graphic-char-p #¥a) -> "a"
;;;         (graphic-char-p #¥space) -> #¥space
;;;         (graphic-char-p #¥Backspce) -> nil
;;;         (graphic-char-p #¥Tab) -> nil
;;;         (graphic-char-p #¥Rubout) -> nil
;;;         (graphic-char-p #¥Linefeed) -> nil
;;;         (graphic-char-p #¥Return) -> nil
;;;         (graphic-char-p #¥Page) -> nil
;;; ＠
;;; greaterp                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : greaterp &rest number1 number2 ... numberN
;;; number1 number2 ... numberN (複素数も可) を左から右に順に比較し、完全に
;;; 単純減少(等しいものがあってもいけない) している場合は、t を返し、
;;; そうでなければ nil を返す。
;;;
;;; <例>
;;;         (greaterp 10 9) -> t
;;;         (greaterp 9 10) -> nil
;;;         (greaterp 10 9 8 7 6) -> t
;;;         (greaterp 10 8 8 7 6) -> nil
;;;         (greaterp) -> t
;;;         (greaterp #c(4 5) #c( 3 7)) -> エラー

(defun tao:greaterp (&rest numbers)
  (if numbers
      (apply #'> numbers)
      t))

;(greaterp 3 2)

;;; ＠
;;; grep                                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 : grep pattern file &opt out start end
;;; file において、キャラクタシーケンス pattern を検索し、見つかったら、
;;; ファイル out に、pattern を含む行を書き込む。
;;; file はファイル名又はファイル名のリストで、ワイルドカード * が指定可能。
;;; start と end は、file において、捜し始めの行と終える行。省略されると、
;;; それぞれ、file の最初の行と最終行となる。
;;; out が省略されるか nil なら変数 *standard-output* の値に書き込まれる。
;;; pattern には、 *, %, + 等のような正規表現を用いることができる。
;;; pattern を含むファイルのファイル名を返す。
;;; ＠

#|(grep "\\?" "/tmp/foo.txt" "/tmp/zot.txt" 1 4)|#
#|(grep "変数" "./tao*")|#

(defun tao:grep (pattern files &optional out start end)
  (flet ((grepout (pattern files out start end)
	   (mapcar (lambda (f)
		     (grep* pattern f out start end))
		   (directory files))))
    (if out
	(with-open-file (out out
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :overwrite)
	  (grepout pattern files out start end))
	(grepout pattern files nil start end))))

(defun grep* (pattern file out start end)
  (flet ((p (line out)
	   (cond ((cl-ppcre:scan pattern line)
		  (princ line out)
		  (terpri out)
		  line))))
    (with-open-file (in file)
      (do ((out (if out out t))
	   (line (read-line in nil 'eof) (read-line in nil 'eof))
	   (cnt 1 (1+ cnt))
	   find?)
	  ((eq 'eof line) (and find? file))
	(and (cond ((and start end)
                    (and (<= start cnt end) (p line out)))
                   ((and start (null end))
                    (and (<= start cnt) (p line out)))
                   ((and (null start) end)
                    (and (<= cnt end) (p line out)))
                   ('T (p line out)))
	     (setq find? t))))))

(defstruct (gl (:print-function print-gl))
  data next last)

(defun print-gl (gl stream depth)
  (declare (ignore depth))
  (format stream "#<GL ~A>" (gl->list gl)))

(defun gl->list (lst)
  (if (gl-p lst)
      (cons (gl-data lst) (gl->list (gl-next lst)))
      lst))

(defun gl-cons (x lst)
  (let ((elt (make-gl :data x :next lst)))
    (if (gl-p lst)
	(setf (gl-last elt) (gl-last lst))
	(setf (gl-last elt) (car (last lst))))
    elt))

#|(defun gl-put-last (x lst)
  (let ((elt (make-gl :data (car lst) :next (cdr lst) :last x)))
    (if (gl-p lst)
	(setf (gl-last lst) (gl-last elt))
	(setf (gl-next elt) (append (cdr lst) (list x))))
    elt))|#


;(gl-put-last 'x '(foo bar baz))

(defun gl-list (&rest args)
  (reduce #'gl-cons args
	  :from-end t :initial-value nil))

;(gl-list 'x 'y 'z 'a 'b 'c)

;(gl-cons 'y (gl-cons 'x '(foo bar baz)))

#|(let ((l '(foo bar baz)))
  (make-gl :data 1 :next l :last (car (last l))))|#

#|(gl-cons 1 ())|#


#|(values
 (time (do ((i 0 (1+ i))
	    (result () (gl-put-last (random 10) result)))
	   ((= 10000 i) )))
 (time (do ((i 0 (1+ i))
	    (result () (gl-cons result (random 10))))
	   ((= 10000 i) ))))|#

#|(gl-cons (random 10) ())|#

#|(gl-put-last 1 ())|#

;;; growlistp                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : growlistp list
;;; list が成長リストなら t を返し、それ以外なら nil を返す。
;;; 成長リストは、その最初の要素 (リストの car 部) と同様に最後の要素にも
;;; ポインタを持つ。それゆえ、最後の要素として新しい値がリストに付け加え
;;; られた時、成長リストでは、普通のリストより速く実行される。
;;; 普通のリストは、最初の要素にだけポインタを持つ。
;;;
;;; <例>
;;;         (!aa (tcons nil '(1 2 3))) -> ((1 2 3))
;;;         (growlistp aa) -> t
;;; ＠
;;; sys:gt                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sys:gt object1 object2
;;; object1 の評価値が、object2 (31 ビットのデータ) の評価値より大きければ
;;; t 、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (de foo (x y) (sys:gt x y)) -> foo
;;;         (foo a b) -> t
;;;         (foo 1 2) -> nil
;;;         (foo "a" "b") -> nil
;;; ＠

