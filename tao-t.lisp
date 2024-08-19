(tao:common-lisp)


(in-package #:tao-internal)


(defconstant tao:t t)


(define
 "t"
 (subr nil)
 :documentation
 "一番新しいエラー発生時のバックトレースのリストを返す。異なるエラーが
生じない限りは、同じ値を返す。つまり、カレントプロセスのインスタンス
変数 :backtrace から情報を受け取る。"
 :example
 "a -> Error at top-exec: unbound-variable a nil
        (t) -> ((unbound-variabl a nil)
        	bas:as-shadow vanilla-error backtrace-stopper)")


(define "t" (class T))


(define
 "tagbody"
 (macro (&rest tags-and-forms)
     (let ((value (gensym "value")))
       `(let (,value)
          (declare (dynamic-extent ,value))
          (tagbody ,@(butlast tags-and-forms)
                   (setq ,value (multiple-value-list ,@(last tags-and-forms))))
          (values-list ,value))))
 :documentation
 "形式 : tagbody &rest body
body がリストならそれをフォームとみなし、リスト以外ならタグとみなす。
そしてフォームだけを評価していき、最後のフォームの評価が終わると、その
値を返す。しかし、評価の途中で go によってあるタグへのジャンプが起こる
と、そのタグのすぐ後のフォームから評価を再開する。"
 :example
 "(!true nil) -> nil
        (tagbody
                (when true (go tag))
        	(prins 'a)
              tag
         	(prins 'b)
        	(prins 'c)) -> abcc")


(define
 "common:tagbody"
 (macro (&rest tags-and-forms)
     `(tagbody ,@tags-and-forms))
 :documentation
 "形式 : common:tagbody &rest body
body がリストならそれをフォームとみなし、リスト以外ならタグとみなす。
そして,フォームだけを評価していき、最後のフォームの評価が終わると nil
を返す。しかし、評価の途中で go によってあるタグへのジャンプが起こると、
そのタグのすぐ後のフォームから評価を再開する。"
 :example
 "(!true '(a b)) -> (a b)
        (common:tagbody
                (when true (go tag))
        	(prins 'a)
              tag
         	(prins 'b)
        	(prins 'c)) -> bcnil")


(define
 "tao.sys:tagep"
 (subr nil)
 :documentation
 "形式 : sys:tagep arg
arg の拡張タグビットがオンならば t を返し、それ以外なら nil を返す。"
 :example
 "(!aa (sys:set-tage \"a\")) -> \"a\"
        (sys:tagep aa) -> t")


(define
 "tailp"
 #'tailp
 :documentation
 "形式 : tailp sublist list
list に対して cdr を連続して適用し、そのうちの 1 つが sublist と eq に
なった時、t を返す。"
 :example
 "x = (1 2 3)とする
        (tailp x x) -> t
        (tailp (cddr x) x) -> t
        (tailp nil x) -> t
        (tailp (list 2 3) x) -> nil (equalであるが eqでない)")


(define
 "talk"
 (expr nil)
 :documentation
 "形式 : talk line-num
ターミナル line-num へメッセージを送る。talking-to ルーチンを呼ぶ。
ユーザは別のユーザと端末を通して会話可能。line-name で相手のユーザの
ログインネームまたはターミナルナンバーを指定する。相手が reply を使って
応答してくれば、端末を通しての会話が可能。終了は、ctrl + z を入力。"
 :example
 "talk 2
        以下入力した内容がターミナル No.2 へ出力される。")


(define
 "tan"
 #'tan
 :documentation
 "形式 : tan number
number (単位:ラジアン) に対する正接値 (tangent) を返す。"
 :example
 "(tan pi) -> -0.0f0
        (tan [pi / 4]) -> 1.0f0")


(define
 "tanh"
 #'tanh
 :documentation
 "形式 : tanh number
number (単位:ラジアン) に対する双曲的正接値を返す。"
 :example
 "(tanh 1.0f0) -> 0.761594155955764f0
        (tanh 0.50f) -> 0.46211715726001f0")


(define
 "tao.sys:tao-package"
 (constant (load-time-value (find-package 'tao)))
 :documentation
 "パッケージ \"tao\" へのポインタ。\"tao\" は、パッケージ \"univ\" の
サブパッケージ。\"tao\" には、パッケージ \"bas\" に同じ名前の TAO 関数が
登録されている。"
 :example
 "sys:tao-package -> {vector}32256(package . 12)")


;;"tao-standard-readtable" -> readtable.lisp


(define
 "tao-standard-sysmode"
 (variable nil)
 :documentation
 "この変数の値は、TAO の標準システムモードを指定する。すなわち
システムモードを指定するキーワード引数を以下のようにセットする。
    :car-nil-error -------- t
    :cdr-nil-error -------- nil
    :one-char-string ------ nil
    :common-lisp ---------- nil
    :negation-as-failure --- nil"
 :example
 "tao-standard-sysmode -> 2052")


(define
 "tconc"
 (subr (list1 list2)
   (etypecase list1
     (list (nconc list1 list2))
     (gl (typecase list2
           (LIST
            (setf (cdr (cdr (gl.cons list1)))
                  list2)
            (setf (cdr (gl.cons list1))
                  (last list2))
            list1)
           (GL
            (setf (cdr (cdr (gl.cons list1)))
                  (car (gl.cons list2)))
            (setf (cdr (gl.cons list1))
                  (last (car (gl.cons list2))))
            (setf (cdr (gl.cons list1))
                  (last (cdr (gl.cons list2))))
            list1)))))
 :documentation
 "形式 : tconc list1 list2
list1 と list2 をこの順でコピーしないで連結する。list1 の最後のセルは、
list2 に結合される。ただし list1 が成長リストである場合、nconc より実行
が速くなる。"
 :example
 "(tconc '(a) '(b))  ->  (a b)
        (tconc '(a) ())    ->  (a)")


(define
 "tcons"
 (subr (list new-value)
   (typecase list
     (LIST
      (gl-snoc new-value (list->gl list)))
     (GL
      (gl-snoc new-value list ))))
 :documentation
 "形式 : tcons list new-value
new-value を、成長リスト list の最後の要素として挿入し、その結果を返す。
list が成長リストでない場合は、list を成長リストとする。list がどのよう
な長さでも実行時間は一定。"
 :example
 "通常のリストを成長リストにしたり、また成長リストを通常のリストにし
たりする方法には以下のような方法がある。

        (!g (tcons nil 1)) -> (1)   ここで  g=(1)  成長リスト
        (tcons g 2) -> (1 2)   ここで  g = (1 2)  成長リスト
        (tcons g 3) -> (1 2 3)  ここで  g = (1 2 3)  成長リスト
        (!g (peelinv g)) -> (1 2 3)  ここで  g = (1 2 3)  通常のリスト
        (!g (tcons (list 1 2 3 4 5) 100)) -> 
        	(1 2 3 4 5 100)  成長リスト")


(define
 "tdir"
 (expr nil)
 :documentation
 "形式 : tdir &opt pathname output-stream 
プリントする順序を除いて、vdir と同じ。pathname での指定にマッチする
全てのファイルについての情報が、それらのファイルが最後に修正された
(新たな作成も含む) 時間順に、プリントされる。"
 :example
 "(!bb (open \"bon.bon\" :direction :output))
        			-> {udo}1314792file-stream
        (tdir \"bs:<dire>\" bb) -> (tdir \"<yukari>\")
        (close bb) -> ok
        ディレクトリ <dire> のファイル群が \"bon.bon\" に書き込まれる。")


(define
 "tek4404-terminal"
 (class T)
 :documentation
 "インスタンスが tek4404 ターミナルのクラス。"
 :example "")


(define
 "tenth"
 #'tenth
 :documentation
 "形式 : tenth list
list の 10 番目の要素の値を返す。"
 :example
 "(tenth '(0 1 2 3 4 5 6 7 8 9 10 11))  ->  9")


(define
 "terminal"
 (expr nil)
 :documentation
 "形式 : terminal &opt type &rest mode
ターミナルタイプが type 、ターミナルモードが mode であるターミナル
ストリームを作成し、*standard-input* 、*standard-output* に代入する。
type は vt100,vt200,cit101e,cit600,cit6001,tek4404,pc98k,dcu のどれかを
指定する。type の既定値は cit101e 。mode は :more か :no-more、:dumb か
:no-dumb、:screen か :no-screen、wrap か :no-wrap を指定する。
mode の既定値は :no-dumb、:no-screen、:no-wrap、:more 。"
 :example
 "")


(define
 "terminal-mode"
 (expr nil)
 :documentation
 "ターミナルのモードを返す。ターミナルがターミナルストリームに結びつい
ていないときは、nil を返す。"
 :example
 "(terminal-mode) -> 
                  (more 24 wrap nil dumb () screen nil local-echo nil)")


'(define
  "terminal-ports"
  (constant nil)
  :documentation
  "ターミナル番号のリスト。例えば ELIS では (0 1 2 3 4 5 6 7) 。"
  :example
  "")


(define
 "terminal-stream"
 (class stream)
 :documentation
 "インスタンスがターミナルストリームであるクラス。"
 :example
 "")


(define
 "terminal-stream-p"
 (expr (stream)
   (typep stream 'tao:terminal-stream))
 :documentation
 "形式 : terminal-stream-p stream
stream がターミナルストリームのとき stream を返し、そうでなければ、nil
を返す。"
 :example
 "")


(define
 "terminal-type"
 (expr nil)
 :documentation
 "*standard-input* と *standard-output* のターミナルタイプを返す。
ターミナルがターミナルストリームに結びついているとき、*standard-input*
と *standard-output* のクラス名称を返す。
terminal-type は、vt100, vt200, cit101e, cit600, cit6001, tek4404, 
pc98k, dcu のどれかが返る。terminal でターミナルタイプが設定されていな
ければ、ファンダメンタルストリームを返す。"
 :example
 "")


(define
 "terpri"
 #'terpri
 :documentation
 "形式 : terpri &opt stream
stream に改行文字と復帰文字を出力し、t を返す。stream が省略されると、
*standard-output* が使われる。terpri は、crlf と同じ。"
 :example
 "")


(define
 "the"
 (macro (value-type form)
     `(cl:the ,value-type ,form))
 :documentation
 "形式 : the type form
form を評価し、その結果がデータ型 type と一致した場合は評価結果を返す
(一致しなかった場合はエラー)。"
 :example
 "(the string(copy-seq x))      この結果は文字列
        (the integer(+ x 3))          + の結果は整数
        (+ (the integer x) 3)         x の値は整数")


(define
 "third"
 #'third
 :documentation
 "形式 : third list
list の 3 番目の要素の値を返す。"
 :example
 "(third '(0 1 2 3 4 5 6 7 8 9 10 11))  ->  2")


(define
 "throw"
 (macro (catch-tag values-form)
     `(cl:throw ,catch-tag ,values-form))
 :documentation
 "形式 : throw tag val
最も内側にある catch, catcher, catcher-case から強制的に脱出させる。
tag を評価し、その値と eq なキャッチャーを最後に設定した catch 式の実行
を終了し、val をその catch 式の値とする。val が空白のとき、未定義値 
{undef}0 を tag に与える。"
 :example
 "(catch 'a (throw 'a 3)) -> 3
        y = 2  a = (b c)
        (catch a (seq (!x 1) (throw a y) (!z 3))) -> 2
        x = 1  y = 2  z = unbound
        (catch a (seq (!x 1) (throw '(b c) y) (!z 3))) はエラー。")


(define
 "throwablep"
 (subr nil)
 :documentation
 "形式 : throwablep tag
tag が catch, catcher, catcher-case のいずれかにあるキャッチタグとして
使われた場合は t を返し、それ以外なら nil を返す。"
 :example
 "")


(define
 "time"
 (cl-macro time)
 :documentation
 "形式 : time internal-time
内部時間形式の時間 internal-time を、時間, 分, 秒, と unit (20 ミリ秒)
のリストとして表示する。
internal-time の既定値は、get-internal-real-time の返す値。
unit については、定数 internal-time-units-per-second を参照。"
 :example
 "(time) -> (1 55 44 44)
        (time (get-internal-run-time)) -> (0 3 21 54)")


(define
 "times"
 #'*
 :documentation
 "形式 : times &rest number1 number2 ... numberN
number1 number2 ... numberN の積を返す。"
 :example
 "(times) -> 1
        (times 2 3 4) -> 24")


(define
 "togap"
 (subr (expr)
   (and (eq 'toga (and (consp expr) (car expr)))))
 :documentation
 "形式 : togap arg
arg がトガ ^ のついた式なら t を返し、そうでなければ nil を返す。"
 :example
 "(togap '^(x y `z)) -> t")


(define
 "trace"
 (cl-macro trace)
 :documentation
 "形式 : trace func
func を変数 *traced-fns* の値とする。func は、トレースされるべき関数と
なる。func が呼ばれると、tracer が自動的に呼ばれ、func の実行についての
種々の情報をプリントする。trace は、種々のサブ関数を指定できる。
:entry 、:entry-do 、:break は、トレースされる関数が適用される前に実行
されることを指定する。 :exit 、:exit-do 、:exit-break は、トレースされ
た関数を出る直前に実行されることを指定する。 :both 、:both-do 、
:both-break は、両方で実行されることを指定する。"
 :example
 "(de my-car (x) (car x)) -> my-car
(trace my-car) -> (my-car)
(my-car '(p q r)  ->
1 <IN> (my-car [x = (p q r)])
1 <OUT> (my-car p)
p
トレース結果についての説明
1 行目の左端の 1 はトレースのレベルを示す。<IN> は、その後にくるものが、
トレースされる関数 my-car が適用される前の値であることを表す。
[x = (p q r)] は、my-car の引数 x の値が (p q r) であることを表す。
2 行目の左端の 1 もトレースのレベルを示す。<OUT> は、その後にくるものが、
トレースされた関数を出る直前の値であることを表す。p は、トレースの一番
上のレベルでの my-car の評価結果である。

(de my-car (x) (car x)) -> my-car
(trace (my-car :entry (numberp (car x)) :exit (stringp (car x))))
        -> my-car
(my-car '(10 20 30))
1 <IN> (my-car [x = (10 20 30)])
10
(my-car '(\"10\" \"20\" \"30\"))
1 <OUT> (my-car \"10\")
10
トレースされる関数が適用される前に、フォーム (numberp (car x)) が評価さ
れる。評価結果が nil でなければ、<IN> を含む行がプリントされ、 nil なら、
その行はプリントされない。さらに、トレースされた関数をでる直前に、フォ
ーム (stringp (car x)) が評価される。評価結果が nil でなければ、<OUT> 
を含む行がプリントされ、nil であれば、その行はプリントされない。")


(define
 "tree-equal"
 #'tree-equal
 :documentation
 "形式 : tree-equal object1 object2 :test :test-not
object1 と object2 を比較し、同一の葉をもった準同形の木であれば t を
返す。"
 :example
 "(tree-equal '(2 3) (cdr '(1 2 3))) -> t")


(define
 "trim"
 (macro (var list &rest forms)
     `(cl:loop :for ,var :in ,list
               :when (cl:progn ,@forms) :collect ,var))
 :documentation
 "形式 : trim &rest var list form1 form2 ... formN
まず、list の第 1 要素を var として form1 form2 ... formN を順に評価
する。次に、list の第 2 要素を var として form1 form2 ... formN を順に
評価する。以下、list の最後の要素までこれを繰り返す。そして、最後の
フォーム formN の評価結果の値をすべて調べ、それが non-nil であった時の、
最新の var の値を並べてリストとして返す。"
 :example
 "(trim i '(1 2 3 4 5 6 7) (oddp i)) -> (1 3 5 7)
        (trim i '(1 2 3 4 5) (!!1+ !i) (evenp i)) -> (2 4 6)
        (trim i (index 1 10) (oddp i) (evenp i)) -> (2 4 6 8 10)")


(define
 "truename"
 #'truename
 :documentation
 "形式 : truename pathname
ファイル pathname のフル・パス名を返す。"
 :example
 "(truename \"cs:<dire>\") -> \"Ho::cs:<dire>foo.tao\"
        (truename \"test.tao\") -> \"Ho::cs:<dire>test.tao\"")


(define
 "truncate"
 #'truncate
 :documentation
 "形式 : truncate number1 &opt number2
number1 を number2 の値で割り算した結果の小数点以下を切り捨てた値を返す。
(number2 が指定されない場合、number1 の値)。"
 :example
 "(truncate 2.6) -> !(2 0.600007)
        (truncate 2.4) -> !(2 0.399993)
        (truncate 0) -> !(0.0)
        (truncate -0.3) -> !(0 -0.30)")


(define
 "two-way-stream"
 (class two-way-stream)
 :documentation
 "インスタンスが双方向ストリームであるクラス。
入出力両方向に動作し、データは、入力ストリームからとられ、出力
ストリームに送られる。"
 :example
 "")


(define
 "tyi"
 (subr nil)
 :documentation
 "形式 : tyi stream
stream から 1 バイトを読み、それに対応する ASCII コードを返す。stream 
が省略されると、*standard-input* の値 (通常コンソールターミナル) が指定
されたものと見なす。stream に読まれる文字がなくなれば、0 を返す。"
 :example
 "(!aa (open \"asd.tao\")) -> {udo}1256651file-stream
        (tyi aa) -> 113  (\"q\")
        (tyi aa) -> 119  (\"w\")
        (tyi aa) -> 0    (:eof)")


(define
 "tyi-no-hang"
 (subr nil)
 :documentation
 "形式 : tyi-no-hang stream
stream のバッファに文字があれば、1 文字読み、対応する ASCII コードに
変換して返す。stream のバッファに 1 文字もなければ、0 (ゼロ) を返す。
stream が省略されると *standard-input* の値 (通常コンソールターミナル)
が指定されたものと見なす。"
 :example
 "(tyi-no-hang) -> 0
        ファイル asd.tao にストリング qw が書かれている
        (!aa (open \"asd.tao\")) -> {udo}1172432file-stream
        (tyi-no-hang aa) -> 113    (\"q\")
        (tyi-no-hang aa) -> 119    (\"w\")
        (tyi-no-hang aa) -> 0")


(define
 "tyi-with-timeout"
 (subr nil)
 :documentation
 "形式 : tyi-with-timeout time-units stream
stream から 1 文字を読み、その文字を返す。stream のバッファに 1 文字も
なければ、time-units (1 time-unit は 20 ミリ秒) 時間、バッファへの入力
を待ち、time-units 時間を過ぎてもバッファに入力された文字がなければ、
nil を返す。time-units を省略した場合、tyi と同じ。stream が省略される
と、*standard-input* の値 (通常コンソールターミナル) が指定されたものと
見なす。"
 :example
 "")


(define
 "tyib"
 (subr nil)
 :documentation
 "形式 : tyib &opt stream
stream から、1 ユニットの入力ストリームを読み、それを返す。stream が
省略されると、*standard-input* の値 (通常コンソールターミナル) が指定
されたものと見なす。stream でコンソールターミナルを指定した場合は、
1 バイトが 1 ユニットになるので tyi を使うのと同じ。現在は tyi と
まったく同じ。"
 :example
 "(!aa (open \"asd.tao\")) -> {udo}1256651file-stream
        (tyib aa) -> 113  (\"q\")
        (tyib aa) -> 119  (\"w\")
        (tyib aa) -> 0    (:eof)")


(define
 "tyo"
 (subr nil)
 :documentation
 "形式 : tyo code &opt stream
stream に、code を ASCII コードに対応させた文字を出力し、code を返す。
stream が省略されると、*standard-output* が使われる。code は、数でなけ
ればならない。"
 :example
 "(tyo 113) -> q113
        (tyo 119) -> w119
        (tyo 101) -> e101")


(define
 "tyo-with-timeout"
 (subr nil)
 :documentation
 "形式 : tyo-with-timeout ticks code &opt stream
ticks (ticks が 1 なら 20 ミリ秒) が過ぎ去っても、まだ code が、stream
に出力されていなければ、nil を返し、出力されていれば、code に対応した
文字を返す。stream の既定値は、*standard-output* の値。ターミナルもしく
はポートのハングアップをチェックするのに利用できる。"
 :example
 "(tyo-with-timeout 10 97) -> a97
        (!aa (open \"eli.tao\" :direction :output)) -> 
        	{udo}1275469file-stream
        (tyo-with-timeout 100 97 aa) -> 97
        (tyo-with-timeout 100 98 aa) -> 98
        (tyo-with-timeout 100 99 aa) -> 99
        (close aa) -> ok
        ファイル eli.tao に abc が書き込まれた")


(define
 "tyob"
 (subr nil)
 :documentation
 "形式 : tyob code &opt stream
stream に、code を ASCII コードに対応させた文字を出力し、code を返す。
code は、数でなければならない。つまり、tyo とほぼ同様の働きをする。
違いは、stream に 1 ユニットだけが出力されること。stream の省略される
と、*standard-output* が使われる。stream でコンソールターミナルを指定す
れば、tyob は、tyo と同じ。(tyo は 1 バイトとして、1 ユニットとり、
1 ユニットは、コンソールターミナルでは、1 バイト)"
 :example
 "(!aa (open \"eli.tao\" :direction :output)) -> 
        	{udo}1275469file-stream
        (tyob 97 aa) -> 97
        (tyob 98 aa) -> 98
        (tyob 99 aa) -> 99
        (close aa) -> ok
        (tyob 97) -> a97
        (tyob 98) -> b98
        (tyob 99) -> c99")


(define
 "type"
 (subr (file &optional (stream *standard-input*))
   (with-open-file (in file :if-does-not-exist :create)
     (loop :for c := (read-char in in)
           :until (eq c in)
           :do (princ c stream))))
 :documentation
 "形式 : type file &opt stream
file の内容を stream にプリントする。
stream が省略されると *standard-input* の値 (通常コンソールターミナル)
が指定されたものと見なす。"
 :example
 "")


(define
 "type-of"
 #'type-of
 :documentation
 "形式 : type-of object
object が属するタイプの型指定子を返す。"
 :example
 "(type-of 3) -> fixnum
        (type-of \"moji\") -> simple-string
        (type-of 123.4) -> simple-float
        (type-of '(a b c)) -> cons")


(define
 "typecase"
 (cl-macro typecase)
 :documentation
 "形式 : typecase key-form
            (type1 form11 form12 ...)
            (type2 form21 form22 ...)
            ...
key-form を評価した値が型指定子 type1 type2 ... のどれかにマッチするか
を調べる。見つかれば、その後にくるフォームを順に実行し、最後のフォーム
の評価結果を返す。見つからなけば nil を返す。"
 :example
 "(typecase an-object
               (string ... )
               ((array t) ... )
               ((array bit) ... )
               (array ... )
               ((or list number) ... )
               (t ... ))")


(define
 "typep"
 #'typep
 :documentation
 "形式 : typep object &opt type
object のデータタイプが型 type と一致しているなら object を、それ以外
なら nil を返す。type の指定がない場合、object のタイプを示すシンボルを
返す。"
 :example
 "(typep () 'null) -> t
        (typep 'abc 'symbol) -> nil
        (typep \"a\" 'atom) -> t
        (typep '(1 2 3) 'list) -> (1 2 3)
        (typep 123 'number) -> 123
        (typep \"a\" 'character) -> \"a\"
        (typep \"string\" 'string) -> \"string\"
        (typep 123 'string) -> nil")


;;; *EOF*
