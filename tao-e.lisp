(in-package #:tao-internal)
(in-readtable :tao)

(defmacro tao:exit-progi (&optional val progi-id)
  "exit-progi                             関数[#!subr]

<説明>
  形式 : exit-progi &opt val progi-id
progi-id を持つ progi 節から強制的に脱出させる。
val が指定されると val を返し、省略されると {undef}0 を返す。
progi-id が省略された時は、最も内側にある progi 節から脱出させる。

<例>
        (progi qwe 1 2 (exit-progi 3 qwe) 4 5) -> 3
        (progi 1 2  (exit-progi 3) 4 5) -> 3"
  `(return-from ,progi-id ,val))

;;; ＠
;;; ecase                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : ecase keyform
;;;                (keylist1 form11 form12 ...)
;;;                (keylist2 form21 form22 ...)
;;;                (keylist3 form31 form32 ...)
;;;                ...)
;;; フォーム keyform を評価し、リスト keylist の要素と eql な最初の節を選択
;;; し、その節の form を順に評価し、最後の form によって返されるものを返す。
;;; 満足されるような節がなければ、エラーを警告する。このエラーから継続する
;;; ことは、許されない。otherwise と t は特別扱いされない。
;;;
;;; <例>
;;;         (defun test (x)
;;;         	(ecase x (a 10)
;;;         		 (b 20))) -> test
;;;         (test 'a) -> 10
;;;         (test 'b) -> 20
;;;         (test 'c) -> (not-implemented-yet ecase (x ((a 10) (b 20))))
;;; ＠
;;; echo-stream                            クラス
;;;
;;; <説明>
;;;   インスタンスがエコーストリームであるクラス。
;;; 入出力両方向に動作し、データは入力ストリームからとられ出力ストリームに
;;; 送られる。さらに入力ストリームからとられたデータは自動的に出力
;;; ストリームに送られる。
;;; ＠
;;; ed                                     関数[#!macro]
;;;
;;; <説明>
;;;   形式 : ed &opt arg
;;; エディタを呼び出す。
;;; (ed) あるいは (ed nil)   単にエディタにはいり、エディタにいた最後のとき
;;;                          と同じ状況になる。
;;; (ed pathname)      ファイル pathname の内容を編集。
;;; (ed symbol)        関数 symbol のテキストを編集。
;;; ＠
;;; eighth                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : eighth list
;;; list の 8 番目の要素の値を返す。
;;;
;;; <例>
;;;         (eighth '(1 2 3 4 5 6 7 8 9 0)) -> 8
;;; ＠
;;; elapse-time                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : elapse-time form
;;; form が実行されている間の経過時間を返す。
;;; ＠
;;; elt                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : elt seq n
;;; シーケンス seq の n 番目の要素を返す。
;;; n は、seq の長さ以内の 0 又は正の整数でなければならない。
;;; ベクタのフィルポインタの値を見ることができる。
;;;
;;; <例>
;;;         (elt '(a b c d e) 2) -> c
;;;         (!x  '(a b c d e)) -> (a b c d e)
;;;         (setf (elt x 4) 1) -> (a b c d 1)
;;;         x -> (a b c d 1)
;;; ＠
;;; encode-universal-time                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : encode-universal-time second minute hour date month year
;;;         	               &opt time-zone
;;; それぞれ second,  minute,  hour,  date,  month,  year, で指定された
;;; 秒、分、時、日、月、年で表されるデコーデッドタイム形式の時刻を、
;;; ユニバーサルタイム形式の時刻に変換する。
;;; 返される値は、変換されたユニバーサルタイム形式の時刻。
;;; time-zone の既定値は、夏時間調整を行った現在のタイムゾーン。
;;; time-zone を指定すると、夏時間調整は実行されない。
;;;
;;; <例>
;;;         (encode-universal-time 0 37 10 3 7 1985) -> 2698256220
;;;         (encode-universal-time 0 0 -9 1 1 1900) -> 0
;;; ＠
;;; endp                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : endp list
;;; リストの終りを検出するために使用される。list がリストの終わりならば
;;; t 、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (endp '(1 2 3)) -> nil
;;;         (endp '()) -> t
;;;         (de list-reverse (list)
;;;             (do ((x list (cdr x))
;;;                  (y '() (cons (car x) y)))
;;;         	 ((endp x) y))) -> list-reverse
;;; ＠
;;; enough-namestring                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : enough-namestring pathname &opt defaults
;;; pathname を識別するのに十分な省略された namestring を返す。
;;; namestring でファイル指定をおこなうと省略された要素は defaults の値から
;;; とられる（既定値は変数 *default-pathname-defaults* の値）。
;;;
;;; <例>
;;;         (namestring *default-pathname-defaults*)
;;;         		-> "Ho::cs:<dire>foo.tao"
;;;         (enough-namestring "abc.tao") -> "abc"
;;;         (enough-namestring "tao.oat") -> "tao.oat"
;;; ＠
;;; environment                            関数[#!macro]
;;;
;;; <説明>
;;;   形式 : environment &rest var
;;; ローカル変数 var があるならば、その値を保存し、ないときには、それが実行
;;; される場所から見られる全てのローカル変数の値を保存。
;;;
;;; <例>
;;;         (let (x y z)
;;;               (!x 1) (!y 2) (!z 3)
;;;               (!a (environment))
;;;               (!b (environment x y z))
;;;               (!c (environment x y)) -> {applobj}40202(#!closure . 8)
;;;         (x + y + z) -> エラー       (z は束縛されていない変数)
;;;         (a '(x + y + z)) -> 6
;;;         (b '(x + y + z)) -> 6
;;;         (c '(x + y + z)) -> エラー  (z は束縛されていない変数)
;;;         (c '(x + y)) -> 3
;;; ＠
;;; eq                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : eq object1 object2
;;; 2 つの引数の値が以下の条件を満足すれば t 、そうでなければ nil を返す。
;;;     (1) 両方とも nil
;;;     (2) 等値の shortnum
;;;     (3) 同一の symbol
;;;     (4) 同じコード化整数 (codnum)
;;;     (5) 同じ文字 (a character string)
;;;     (6) 等値の shortfloat (24 ビットで示された floating-point number)
;;; 2 つの引数のアドレスがメモリー上、同位置にあるとき、t を返す。
;;; 同じ値をプリントする引数が eqable でなければ、nil を返す。
;;; 2 引数が全く同一のデータを示すときは t を返す。
;;; eql equal 参照。
;;;
;;; <例>
;;;         (eq nil nil) -> t ;nil's
;;;         (eq -345678 -345678) -> t ;shortnums
;;;         (eq #12 #12) -> t ;shortnums
;;;         (eq 12345678 12345678) -> nil ;bignums
;;;         (eq 12.3 12.3) -> t ;floating numbers
;;;         (eq 'abc 'abc) -> t ;identical symbols
;;;         (eq #!expr #!expr) -> t ;codnums
;;;         (eq "a" "a") -> t ;characters
;;;         (eq "a" "A") -> nil ;different internal character codes
;;;         (eq '(1 2) '(1 2)) -> nil ;not atoms but lists
;;;         (eq "abc" "abc") -> nil ;not characters but strings
;;;         (eq x x) -> t ;whatever x's value is
;;; ＠
;;; eql                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : eql object1 object2
;;; 2 つの引数の値が以下の条件を満足した場合は、nil 以外の値を返す (これ
;;; 以外の場合は nil を返す)。
;;;     (1) eq である。
;;;     (2) 同じタイプの数値で同じ値。
;;;     (3) 同じ文字列。
;;;
;;; <例>
;;;         (eql 'abc 'abc) -> t
;;;         (eql 3 3) -> t
;;;         (eql 123456789 123456789) -> 123456789
;;;         (eql 3.0 3.0) -> 3.0
;;;         (eql "tao" "tao") -> t
;;;         (eql #¥a #¥a) -> t
;;;         (eql #¥A #¥a) -> nil
;;;         (eql '(a . b) '(a . b)) -> nil
;;; ＠
;;; equal                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : equal object1 object2
;;; 2 つの引数の値が構造的に同型ならば t を返す。
;;;
;;; <例>
;;;         (equal (list 1 2) '(1 2)) -> t
;;;         (equal 1234567890 1234567890) -> t
;;;         (equal "string-uneqable" (sconc "string-" "uneqable")) -> t
;;;         (equal (vcons 'qwer 3) (vcons 'qwer 3)) -> t
;;; ＠
;;; equalp                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : equalp object1 object2
;;; 2 つの引数の値が以下の条件を満足すれば t 、それ以外なら nil を返す。
;;;   (1) 文字型: コード属性が同じ
;;;             (大・小文字の差異、ビット・フォント属性は無視)。
;;;   (2) 数型: 同一の数値 (型は一致しなくてよい)。
;;;   (3) リスト: 同一の型で、各要素がこの関数の条件を満足。
;;;   (4) 配列: 等次元数で、対応する要素がこの関数の条件を満足。
;;;
;;; <例>
;;;         (equalp 'a 'b) -> nil
;;;         (equalp 'a 'a) -> t
;;;         (equalp 3 3.0) -> t
;;;         (equalp #c(3 -4.0) #c(3 -4)) -> t
;;;         (equalp '(a . b) '(a . b)) -> t
;;;         (progn (setq x '(a . b)) (equalp x x)) -> t
;;;         (equalp "Foo" "foo") -> t
;;; ＠


(defun tao:error (string &optional arg1 arg2)
  "error                                  関数[#!expr]

<説明>
  形式 : error string &opt arg1 arg2
現在の標準エラーを報告する。そのエラーは出力ストリーム *error-output*
に出力される。 string はエラー名を示す文字列。
arg1 は最初の補助情報であり、大抵の場合、エラーが発生した内部関数を示す。
arg2 は 2 番目の補助情報であり、大抵の場合、エラーを引き起こした誤った
引数を示す。"
  (cl:error "~:[~;~:*~A at ~]~:[~;~:*~A: ~]~A" arg2 arg1 string))


;;; ＠
;;; sys:error-in-error                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sys:error-in-error string arg1 arg2
;;; エラーが、set-error-function によって定義されたエラー処理ルーチンで発生
;;; した場合、この関数がシステムによって自動的に起動される。
;;; 3 つの引数は、親のエラー処理ルーチンからそれぞれの値を継承する。
;;; string はエラーを示す文字列。
;;; arg1 は最初の補助情報であり、大抵の場合、エラーが発生した内部関数を示す。
;;; arg2 は 2 番目の補助情報であり、大抵の場合、エラーを引き起こした誤った
;;; 引数を示す。
;;; ユーザは決してこの関数を使ってはならない。
;;; ＠
;;; etypecase                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : etypecase keyform
;;;                    (type1 form11 form12 ...)
;;;                    (type2 form21 form22 ...)
;;;                    (type3 form31 form32 ...)
;;;                    ... )
;;; フォーム keyform を評価し、その型が一致するtype の最初の節を選択し、
;;; その節の form を順に評価し、最後の form によって返されるものを返す。
;;; 満足するような節がなければ、エラーを警告する。このエラーから継続する
;;; ことは、許されない。type は、型指定子であり、評価されない。
;;; type として otherwise は 指定できない。typecase 参照。
;;;
;;; <例>
;;;         (defun test (x)
;;;         	(etypecase (list 'list) (integer 'integer))) -> test
;;;         (test '(a b c)) -> (not-implemented-yet etypecase
;;;         			((list 'list) (integer 'integer)))
;;; ＠
;;; eval                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : eval object
;;; object を評価し、結果を返す。
;;; シンボルの値を求める場合、symbol-value のほうが効率的。
;;;
;;; <例>
;;;         (eval nil) -> nil
;;;         (eval 123) -> 123
;;;         (eval 'x)  -> 現在の x の値を返す
;;;         (eval '(cons 'abc 'def)) -> (abc . def)
;;;         (eval ''a) -> 'a
;;; ＠
;;; eval-in-upper-env                      関数[#!subr]
;;;
;;; <説明>
;;;   形式 : eval-in-upper-env object n
;;; この関数自身のある場所のレベルよりも n レベル下ったところでの object の
;;; 値を返す。
;;;
;;; <例>
;;;         (!y 123)
;;;         (de fn (x y)
;;;             (!x 'y)
;;;             (!y 456)
;;;             (eval x)
;;;             (eval-in-upper-env x 1)
;;; ＠
;;; eval-inside-yourself                   メッセージ
;;;
;;; <説明>
;;;   形式 : (receiver eval-inside-yourself form)
;;; このメッセージの receiver と同じ環境で、form を実行する。
;;; form では、receiver のクラスがもつクラス変数、インスタンス変数、
;;; メソッドなどを使うことができる。
;;; ＠
;;; eval-when                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : eval-when situation form
;;; form が評価される状況を situation で決定する。
;;; (1) situation = load
;;;   s 式 (eval-when...) のコンパイルされたコードが動く時に、評価される。
;;; (2) situation = eval
;;;   s 式 (eval-when...) がロードされるか、またはインタプリタで評価される
;;;   時に評価される。
;;; (3) situation = compile
;;;   コンパイラが s 式 (eval-when...) を処理しようとする時に評価される。
;;;
;;; <例>
;;;         (seq (f-1)
;;;              (eval-when (load) f-2)
;;;              (eval-when (eval) f-3)
;;;              (eval-when (compile) f-4)
;;;              (eval-when (compile load) f-5)
;;;              (eval-when (compile eval) f-6
;;;              (eval-when (compile load eval) f-7)
;;;         s 式 (seq ... ) がロードされると f-1 f-3 f-6 f-7 が評価される。
;;;         s 式 (seq ... ) がコンパイルされると、コンパイルが始まる前に
;;;         f-4  f-5 f-6 f-7 が評価される。
;;;         次に、seq を含む f-1 f-2 f-5 f-7 のオブジェクトコードが
;;;         コンパイラにより生成される。
;;; ＠
;;; evalhook                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : evalhook form evalhookfn applyhookfn &optional env
;;; デバッグを助けるためのフック機能を容易に使えるように用意された関数。
;;; 変数 *evalhook* と変数 *applyhook* を各々 evalhookfn と applyhookfn
;;; に束縛して form を評価して、その結果を返す。
;;; ＠
;;; evalp                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : evalp object
;;; object のデータタイプが eval# (オブジェクトの先頭がコンマ) ならば評価値
;;; を返す。
;;;
;;; <例>
;;;         (evalp (cadr `(a ,b c))) -> ,b
;;;         (evalp ',a) -> ,a
;;; ＠
;;; evenp                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : evenp integer
;;; integer の評価値が偶数であれば評価値を返し、奇数なら nil を返す。
;;;
;;; <例>
;;;         (evenp 2) -> 2
;;;         (evenp 3) -> nil
;;; ＠
;;; every                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : every pred seq1 &rest seq2 ... seqN
;;; シーケンス seq1 seq2 ... seqN の各要素に述語 pred を順番に適用し、nil
;;; になった場合には、ただちに nil を返し、最後まで nil にならなかった場合
;;; には t を返す。pred が 1 つ以上の引数をとる場合、pred がとる引数と同じ
;;; 数のシーケンスがなければならない。
;;;
;;; <例>
;;;         (every #'oddp '(1 3 5)) -> t
;;;         (every #'oddp '(1 2 3)) -> nil
;;; ＠
;;; exit                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : exit &opt object
;;; 最も内側にある関数 (lambdaを含む) から強制的に脱出し、object の値を返す。
;;; object の既定値は nil。
;;;
;;; <例>
;;;         (loop (&aux aa)
;;;               (:init (!aa '(1 2 3 4 5 6 7 8 9)))
;;;               (:until (null aa) 'owari)
;;;               (if (not (numberp (car aa)))
;;;         	  (exit 'not-number-occurs))
;;;               (!!cdr !aa))
;;; ＠

(defmacro tao:exit-for (&optional val exit-id)
  "exit-for                               関数[#!subr]

<説明>
  形式 : exit-for &opt val exit-id
exit-id を持つ for 節（関数 named-for が構成）から強制的に脱出させる。
val が指定されると val を返し、省略されると {undef}0 を返す。
exit-id が省略された時は、最も内側にある for 節から脱出させる。

<例>
        (named-for abc x (index 1 100)
                   (!y (+ (x ** 3) (x ** 2) x 1))
                   ((cond ((y <= 50) (write y))
        		       (t (exit-for 'end abc))))
        	-> 4
        	   15
        	   40
        	   end"
  `(return-from ,exit-id ,val))

;;; ＠
;;; exit-image                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : exit-image &opt val
;;; 最も内側にある image 節から強制的に脱出させる。
;;; val が指定されると val を返し、省略されると {undef}0 を返す。
;;;
;;; <例>
;;;         (image x (index 1 100)
;;;          	(!y (+ (x ** 3) (x ** 2) x 1))
;;;         		((cond ((y <= 50) (write y))
;;;         		       (t (exit-image 'end))))
;;;         	-> 4
;;;         	   15
;;;         	   40
;;;         	   end
;;; ＠

(defmacro tao:exit-loop (&optional val tag)
  "exit-loop                              関数[#!subr]

<説明>
  形式 : exit-loop &opt val exit-id
exit-id を持つ loop から強制的に脱出させる。
val が指定されると val を返し、省略されると {undef}0 を返す。
exit-id が省略された時は、最も内側にある loop から脱出させる。

<例>
        (loop abc (&aux x y)
         	 (:init (!x 0))
        		 (!x (1+ x))
        	         (!y (+ (x ** 3) (x ** 2) x 1))
        		 ((cond ((y <= 50) (write y))
        		        (t (exit-loop 'end abc))))
        	-> 4
        	   15
        	   40
        	   end"
  `(return-from ,tag ,val))

(defun tao:exploden (char-list)
  "exploden                               関数[#!expr]

<説明>
  形式 : exploden char-list
文字リスト char-list の文字対応のコード値を十進数 (ASCIIコード
JISコード) のリスト形式で返す。

<例>
        (exploden \"abc\") -> (97 98 99)    (ASCII)
        (exploden 'abc) -> (97 98 99))    (ASCII)
        (exploden '123) → (49 50 51)     (ASCII)
        (exploden \"あいう\") -> (42146 42148 42150)  (JIS)"
  (let ((str (string-or-symbol->string char-list)))
    (map 'list #'char-code (coerce str 'list))))

;;; tao:export                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : tao:export &rest symbol1 symbol2 ... symbolN
;;; カレントパッケージの symbol1 symbol2 ... symbolN を外部シンボルに変える。
;;;
;;; <例>
;;;         (tao:export a b c d) -> ok
;;; ＠
;;; export                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : export symbols &opt package
;;; symbols を package (既定値はカレントパッケージ) の外部シンボルとして
;;; 登録し、t を返す。symbols は 1 個のシンボルまたはシンボルを並べたリスト。
;;;
;;; <例>
;;;         (export '(edit ledit)) -> t
;;;         (export 'tool) -> t
;;;         (de a (x)
;;;            (print x)) -> a
;;;         (export 'x) -> t
;;; ＠

(defmacro tao:expr (var-list &body body)
  "expr                                   関数[#!subr]

<説明>
  形式 : expr var-list body
var-list を引数リストとする expr 型関数 (スコープ限定関数) を生成。
body は定義本体。 de と expr の関係は dye と lambda の関係と同じ。

<例>
        (expr (x y) (+ (foo x) (bar y)))
        ((expr (x y) (+ x y)) 2 3) -> 5"
  ;; evalはnull lexical environmentにするために利用
  (eval `(lambda ,var-list ,@body)))

;;; expt                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : expt number1 number2
;;; number1 を number2 でべき乗した結果を返す。
;;; number1 が有理数であり、かつ number2 が整数であるなら、結果は有理数型
;;; となる。 この条件を満足しない場合は、浮動小数点形式の近似値を返す。
;;;
;;; <例>
;;;         (expt 0 0) -> 1
;;;         (expt 1 0) ->1
;;;         (expt 2 3) -> 8
;;;         (expt 2.3 2) -> 5.29
;;;         (expt -3 4) -> 81
;;;         (expt -3 3) -> -27
;;; ＠
;;; expunge-files                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : expunge-files &opt dir
;;; ディレクトリ dir 中のファイルのうち、関数 delete-file によりすでに
;;; 削除されたファイルを物理的に削除する。
;;; この関数により削除されたファイルの復活はできない。
;;;
;;; <例>
;;;         (delete-file "ts.tao") -> ("bs:<dire>ts.tao.2")
;;;         (delete-file "tss.tao") -> ("bs:<dire>tss.tao.5")
;;;         (all-delete-files) ->
;;;    	("bs:<dire>tss.tao.5" "bs:<dire>ts.tao.2")
;;;         (expunge-files) -> (2 files 221 byte are expunged)
;;; ＠
