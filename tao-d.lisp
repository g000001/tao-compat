(in-package #:tao-internal)
(in-readtable :tao)

;; dashift                                関数[#!subr]
;;
;; <説明>
;;   形式 : dashift object1 object2
;; object2 を評価した結果（数）だけ object1 の内容を左に (負なら右に)
;; ビットシフトし、その結果を返す (元のオブジェクトは破壊される)。
;; サインビットは破壊されない。左へのシフトで空白になったビット部分
;; には 0 をつめる。右へのシフトで空白になったビット部分にはサインビット
;; の値をつめる。ashift の破壊版。

;; sys:data-type                          関数[#!subr]
;;
;; <説明>
;;   形式 : sys:data-type object
;; object のデータタイプを返す。
;;
;; <例>
;;         (sys:data-type "qwe") -> sys:str
;;         (sys:data-type 23) -> sys:shortnum
;;         (sys:data-type '(1 2 3)) -> sys:cell

(defun tao:day-of-week-string (number)
  #.(string '#:|day-of-week-string                     関数[#!expr]

<説明>
  形式 : day-of-week-string number
number に対応する曜日名を文字列で返す。number が 0～6 以外の時は、nil
を返す。

<例>
        (day-of-week-string 0) -> "Sun"
        (day-of-week-string 1) -> "Mon"
        (day-of-week-string 2) -> "Tue"|)
  (case number
    (0 "Sun")
    (1 "Mon")
    (2 "Tue")
    (3 "Wed")
    (4 "Thu")
    (5 "Fri")
    (6 "Sut")
    (otherwise nil)))

;; dbit-off                               関数[#!subr]
;;
;; <説明>
;;   形式 : dbit-off bit-array &rest position1 position2 ... positionN
;; bit-array (locative データ型か、64 ビット以内で表現できる数値) の
;; ビット位置 position1 position2 ... positionN のビットをクリアし、
;; その結果を返す。bit-array は破壊される。bit-off の破壊版。
;;
;; <例>
;;         (!a #177) -> #177
;;         (bit-off a 3 5) -> #127
;;         a -> #127
;;         (dbit-off a 2) -> #123
;;         a -> #123

;; dbit-on                                関数[#!subr]
;;
;; <説明>
;;   形式 : dbit-on bit-array &rest position1 position2 ... positionN
;; bit-array (locative データ型か、64 ビット以内で表現できる数値)の
;; ビット位置 position1 position2 ... positionN のビットをセツトし、
;; その結果を返す。bit-array の内容は破壊される。bit-on の破壊版。
;;
;; <例>
;;         (!x #123) -> #123
;;         (bit-on x 2) -> #127
;;         x -> #127
;;         (dbit-on x 3 5) -> #177
;;         x -> #177

;; dbp                                    関数[#!expr]
;;
;; <説明>
;;   形式 : dpb newbyte bytespec integer
;; integer の bytespec が指定するバイトを newbyte の bytespec が指定
;; するバイトで置き換え、その結果を返す。
;;
;; <例>
;;         (dbp 1 (byte 1 2) 3) -> #7
;;         (dbp 3 (byte 4 6) 10) -> #312

;; dcu-terminal                           クラス
;;
;; <説明>
;;   インスタンスが dcu ターミナルであるクラス。

;; deは、define exprの略
;; defunとの差異が不明 拡張されたlambdaリストが取れるのが、
;; defunかもしれない。
(defmacro tao:de (fn var-list &body body)
  "de                                     関数[#!expr]

<説明>
  形式 : de 'fn 'var-list &rest 'body
fn が関数名、var-list が引数リストである expr 型関数、すなわち
スコープ限定型関数を body で定義。

<例>
        (de fact (n)
          (cond ((n = 0) 1)
                (t (n * (fact (n - 1)))) )) -> fact
        fact は、階乗の計算をする関数。
        (de cell-count (x)
          (cond ((consp x)
                 (+ 1 (cell-count (car x)) (cell-count (cdr x))))
                (t 0) ))  ->  cell-count"
  ;; evalは、null lexical environmentにするために利用
  `(eval
    '(defun ,fn ,(substitute '&optional '&opt var-list)
      ,@body)))

;; debug                                  関数[#!expr]
;;
;; <説明>
;;   形式 : debug &opt prompt init-str
;; デバッガを呼ぶ。デバッガが走っている間、トップレベルと同じようにして
;; ユーザはスタックの情報（デバッガ呼びだしまで走っていた関数の状態につい
;; ての情報）を得ることができ、あらゆる関数を実行させることができる。
;; :ok または :exit をフォームとともにタイプすると、デバッガを終了でき、
;; デバッガはフォームの評価結果の値を返す。
;; プロンプトのシンボル > の数は、デバッガがネストされている深さを表わす。
;; 例えば、debug>>>> は、現在走っているデバッガが 4 重にネストされている
;; ことを示す。デバッガのプロンプトで、種々のコマンドを実行できる。
;; それらは、 :bt, :btf, :btff, :fn, :def, :body, :form, :varp,
;; :specialp, :ok, :exit 等。それぞれの機能を以下に示す。
;; :bt, :btf, :btff ---- バックトレースの情報が得られる。:bt, :btf, :btff
;;                       の順により詳細な情報となる。
;; :fn ----------------- デバッガを呼んだ関数の名前が得られる。
;; :def ---------------- デバッガを呼んだ関数の定義が得られる。
;; :body --------------- デバッガを呼んだ関数の本体が得られる。
;; :form --------------- デバッガを呼んでいるフォームがプリントされる。
;; :varp --------------- :varp var-name とすると var-name が変数かどうか
;;                       チェックされる。
;; :specialp ----------- :specialp var-name とすると var-name がスペシャル
;;                       変数かどうかチェックされる。
;; :debug-on ----------- エラーが起こるたびにデバッガが呼ばれる。
;;                       このデバッガ呼びだしのプロンプトは error> 。
;; :debug-off ---------- エラーが起こってもデバッガは呼ばれない。

(defmacro tao:dec (var &optional (val 1))
  "dec                                    関数[#!macro]

<説明>
  形式 : dec var &opt val
var の値から val の値を引き、その結果を返す。 val の既定値は 1 。
\(!!- !x n) と同じ。

<例>
        (!x 10)
        (dec x) -> 9
        x -> 9
        (dec x -2) -> 11
        x -> 11
        (dec 3 2) -> エラー
        (dec 3) -> エラー"
  `(setq ,var (- ,var ,val)))

;; decf                                   関数[#!macro]
;;
;; <説明>
;;   形式 : decf place &opt delta
;; place の値から delta の値を引き、その結果を返すとともに、 place に
;; 代入する。  delta の既定値は 1 。機能は dec と同じ。
;;
;; <例>
;;         (!n 0)
;;         (decf n 3) -> -3  そして  n=-3
;;         (decf n -5) -> 2  そして  n=2
;;         (decf n)   -> 1   そして  n=1

;; declare                                関数[#!subr]
;;
;; <説明>
;;   形式 : declare &rest 'decl-spec
;; プログラム内の特定の部分だけに動的束縛を発生させる。
;; decl-spec で指定される宣言子は以下のようなものがある。
;; (special 変数1 ... 変数n)   変数1～変数n のスペシャル宣言
;; (type  データ型  変数1 ... 変数n)    変数1～変数n の型宣言
;;     これは (データ型  変数1  変数n) と簡略化できる
;; (ftype  データ型  関数名1 ... 関数名n)
;;     関数名1～関数名n が名前の関数型宣言
;;     データ型は次の形式
;;     (function (第1引数の型 ... 第m引数の型) 返す値の型)
;;     または
;;     (function  関数名 (第1引数の型 ... 第m引数の型)
;;                       (values  第1返値の型 ... 第n返値の型))
;; (function  関数名 (第1引数の型 ... 第m引数の型)
;;           第1返値の型 ... 第n返値の型)
;;     関数名が名前の関数の引数と返す値の型宣言
;; (ignore  変数1 ... 変数n)
;;     変数1～変数n が決して参照されないことを宣言
;; (inline  関数名1 ... 関数名n)
;;     関数名1～関数名n が名前の関数を、可能なら、コンパイル時に
;;     インライン展開することを宣言
;; (notinline  関数名1 ... 関数名n)
;;     関数名1～関数名n が名前の関数をコンパイル時にインライン展開しては
;;     ならないことを宣言
;; (optimize 指定1 ... 指定n)    コンパイル時の最適化指定
;;     指定i は次の形式
;;     (speed  レベル)    実行効率の最適化レベルの指定
;;     (space  レベル)    メモリ効率の最適化レベルの指定
;;     (safety  レベル)    実行時エラーチェックのレベル指定
;;     (compilation-speed  レベル)    コンパイル時間の最適化レベルの指定
;;     レベルは 0 ～ 3 の整数
;; (declaration  記号1 ... 記号n)
;;     記号i で始まるリストを宣言子として使用することを宣言
;;
;; <例>
;;         x を以下のように定義すると、この定義の中に限って
;;         x がスペシャル変数になる。
;;         (de foo (x)
;;           	(declare (special x))
;;                 (h (1+ x)))
;;         従って、関数 h には大域変数の x の値に 1 を加えた値が送られる。
;;         (de h (x)
;;             (cons (symbol-value 'x) x))
;;         (foo 1) -> (1 . 2)

;; decnum                                 関数[#!expr]
;;
;; <説明>
;;   形式 : decnum number
;; number を 10 進数に変換し、それを返す。
;;
;; <例>
;;         (decnum #x10) -> 16
;;         (decnum #o10) -> 8

;; decode-float                           関数[#!expr]
;;
;; <説明>
;;   形式 : decode-float number
;; 浮動小数点数 number の値に対応する、以下の 3 つの値を返す。
;; (1) 小数部を表す新しい浮動小数点数
;; (2) 指数部を表す整数
;; (3) 符号を表す同一形式の浮動小数点数
;;
;; <例>
;;         (decode-float 3.278) -> !(0.8195 2 1.0)
;;         (decode-float 3.0) -> !(0.75 2 1.0)
;;         (decode-float -0.239) -> !(0.956001 -2 -1.0)

(defclsynonym tao:decode-universal-time
    "decode-universal-time                  関数[#!expr]

<説明>
  形式 : decode-universal-time universal-time &opt time-zone
ユニバーサルタイム形式の時刻 universal-time を、デコーデッドタイム
形式の時刻に変更する。
リターン値は、秒、分、時、日、月、年、週日、夏時間かどうか、タイム
ゾーンの 9 つの値。time-zone の既定値は、現在のタイムゾーン。

<例>
        (decode-universal-time (get-universal-time)) -> 21
        (multiple-value-list (decode-universal-time
        			(get-universal-time))) ->
        (15 38 12 4 7 1985 4 nil -9)
        (decode-universal-time  0) -> !(0 0 -9 1 1 1900 1 nil -9)")

;; defclass                               関数[#!macro]
;;
;; <説明>
;;   形式 : defclass 'class-name 'class-vars 'inst-vars
;;                    &opt 'supers &rest 'options
;; クラスベクタと呼ばれるベクタを作成し、class-name で指定された識別子
;; の属性リストに格納する。
;; class-name が、この作成されたクラスの名前を表し、作成したクラスの名前を
;; 返す。
;; class-vars はクラス変数を宣言する部分であり、その要素が cv のよう
;; な識別子または (cv cvini) のようなリストであるリストとして宣言する。
;; class-vars の要素が例えば (cv cvini) というリストである場合、car 部で
;; ある cv はクラス変数を宣言し、cdr 部である cvini はクラス変数cv の初期
;; 値となる。この cvini 式はそのクラスが定義される時に各々一回評価される。
;; inst-vars はインスタンス変数を宣言する部分で、宣言の仕方はクラス変数
;; の場合と同様である。 inst-vars においても、 class-vars と同様に、インス
;; タンス変数の初期値設定ができる。
;; supers は、作成されるクラスのスーパクラス名である識別子から成るリストで
;; ある。 supers の既定値は nil 。
;; options で種々のオプションを指定する。もし、そのオプションの 1 つ
;; :no-vanilla-class が options として与えられなければ、vanilla-class が作
;; 成されるクラスのスーパクラスになる。
;;
;; クラスはスーパクラスやサブクラスで示されるように階層構成をとる。
;; クラスの作成はこの階層構成の上にあるクラスから順に行なう方が望ましい。
;; ただし、関連のあるクラスがすべて作成された後に、それらのクラスの
;; インスタンスの最初の 1 つが関数 make-instance によって作られる場合には、
;; この関数の適用の順序は階層構成の順である必要はない。
;;
;; クラスの構造を示すクラスベクタはそのジェネレーションによって管理される。
;; つまり、あるクラスが同じクラス名によって再定義された場合、このクラスの
;; バージョンナンバーが 1 増して、このクラスのジェネレーションが変わって
;; いることを示す。さらにこのクラスのサブクラスのバージョンナンバーも
;; すべて最新のものになる。
;;
;; クラスは、そのすべてのスーパクラスの全インスタンス変数と全メソッドを
;; 継承するが、クラス変数は継承しない。
;;
;; <例>
;;         (defclass a1 () ((b11 1) (b12 2)) () :gettable) -> a1
;;         (defclass a2 () ((b21 3) (b22 4)) (a1) :gettable) -> a2
;;         (defclass a3 () ((b31 5) (b32 6)) (a2) :gettable) -> a3
;;         (!aa (make-instance 'a3)) -> {udo}40766a3
;;         [aa b11] -> 1
;;         [aa b22] -> 4
;; ＠
;; defclass-method                        関数[#!expr]
;;
;; <説明>
;;   形式 : defclass-method 'class-message-pair 'args &rest 'body
;; class-message-pair は (class-name class-message-pattern) の形式。
;; クラス class-name に、クラスメッセージ名 class-message-pattern で呼び
;; 出されるクラスメソッドを定義し、そのクラスメッセージ名を返す。
;; クラスメッセージは、関数 send-class-message によって、class-name へ
;; 送られる。
;; クラスメソッドのボディ body の中では、関数 cvar によってクラス変数に
;; アクセス可能。
;;
;; <例>
;;         (defclass abc ((a 1)) ()) -> abc
;;         (defclass-method (abc init) (x) (!(cvar a) x)) -> init
;;         (send-class-message abc init 0) -> 0

(defclsynonym tao:defconstant
    "defconstant                            関数[#!expr]

<説明>
  形式 : defconstant symbol init-value &opt doc
symbol をグローバル定数として宣言し、 init-value (省略時は nil) を
初期値とする。 doc はドキュメンテーション文字列。
symbol に代入することはできない。
<例>
        (!aaa 0) -> 0
        (defconstant aaa 1) -> aaa
        aaa -> 1
        (defconstant aaa (- 5 1)) -> aaa
        aaa -> 4
        (!aaa 0) -> エラー")


(defsynonym tao:defglobal #+sbcl sb-ext:defglobal
  #.(string '#:|defglobal                              関数[#!expr]

<説明>
  形式 : defglobal symbol &opt init-val doc
symbol をグローバル変数として宣言し、init-val (省略時は nil)を初期値と
する。doc はドキュメンテーション文字列。

<例>
        (defglobal aa) -> aa
        aa -> nil
        (defglobal bb 10) -> bb
        bb -> 10
        (defglobal cc "fight") -> cc
        cc -> "fight"|))

(defmacro tao:define (symbol applobj)
  "define                                 関数[#!expr]

<説明>
  形式 : define symbol applobj
symbol を関数オブジェクト applobj に結び付ける。

<例>
        (define fn (lambda (x y) (list x y)))
                 = (dye fn (x y) (list x y))
        (define fn (expr (x y) (list x y)))
                 = (de fn (x y) (list x y))
        (define aa (array 10)) -> aa"
  `(progn
     (setf (symbol-function ',symbol) #'values)
     (setf (symbol-function ',symbol)) ,applobj))

;; define-modify-macro                    関数[#!macro]
;;
;; <説明>
;;   形式 : define-modify-macro &rest name lambda-list f-name doc
;; 指定された方法で変数の値を変更する関数を生成したいとき、使用。
;; de や defun 等の関数定義関数によって生成できず、マクロを用いること
;; によってのみ定義される。
;; (define-modify-macro name (arg1 arg2 ...) func doc)
;;     = (defmacro name (x arg1 arg2 ...)
;;           doc
;;         	  (setf ,x (func ,arg1 ,arg2 ...)))
;; doc は省略可。
;;
;; <例>
;;         (define-modify-macro (counter (up-by) +) -> counter
;;         (!count 0) -> 0
;;         (counter count 2) -> 1
;;         count -> 2
;;         (counter count 3) -> 5
;;         count -> 5
;;         以下のように defmacro 関数を使って同様な関数が定義できる。
;;         (defmacro calc (n delta) ｀(setf ,n (+ ,n ,delta))) -> calc
;;         (calc count 4) -> 9
;;         (calc count -3) -> 6

;; define-setf-method                     関数[#!macro]
;;
;; <説明>
;;   形式 : define-setf-method &rest x
;; 関数 setf が指定された関数式に対してどう働くかを定義する。
;; 次の 2 つのどちらかの方法で用いる。
;; (1)
;; (defsetf access-fn update-fn)
;; access-fn は setf が新しい値を格納する場所を供給する関数。
;; update-fn は 場所内に新しい値を格納する関数。
;; つまり、(setf (access-fn argument) new-value) は (access-fn argument)
;; が供給する場所に新しい値を格納する。
;; (2)
;; (define-setf-method access-fn (arg1 arg2 ... ) forms)
;; (access-fn arg1 arg2 ... ) は新しい値が格納される場所を供給する。
;; 一時的な変数である var は place に格納されるべき新しい値を持つ。
;; form は (access-fn arg1 arg2 ... ) によって指定された場所に新しい値を
;; 格納するのに使用。
;; 両方法共に access-fn は関数、またはマクロの名前でなくてはならない。
;; get-setf-method、defsetf を参照。
;;
;; (define-setf-method access-fn (arg1 arg2 ... ) forms)
;; は以下の点を除いて関数 defsetf と同じ。
;; forms が評価されている間は arg1 arg2 ... は一時的な変数に束縛されない。
;; access-fn は関数とマクロどちらの名前も必要としない。
;; 返される値は 5 つの値のリストである。

;; definition                             関数[#!expr]
;;
;; <説明>
;;   形式 : definition symbol
;; symbol の種類に従って以下の情報を返す。
;;
;;         +------------------+------------------------------------+
;;         | symbol の種類    |         返却値                     |
;;         +------------------+------------------------------------+
;;         | 値をもつ変数     |   (!symbol 'value)                 |
;;         +------------------+------------------------------------+
;;         |                  |  (de symbol ...)                   |
;;         |                  |  (defun symbol ...)                |
;;         |  関数            |  (defrel symbol ...)               |
;;         |                  |  (defmacro symbol ... )            |
;;         |                  |  (defsubst symbol ... )            |
;;         +------------------+------------------------------------+
;;         |  クロージャ      |  (!symbol (closure ... ))          |
;;         +------------------+------------------------------------+
;;         |  クラス          |  (defclass symbol ...)             |
;;         +------------------+------------------------------------+
;;         |  配列            | the same information as array-info |
;;         +------------------+------------------------------------+
;;
;; <例>
;;         (de add1 (x)
;;             (1+ x) ) -> add1
;;         (definition add1) -> (de add1 (x)
;;                                  (1+ x) )
;;         ((definition *print-base*) -> (!*print-base* 10.)

;; deflogic-method                        関数[#!macro]
;;
;; <説明>
;;   形式 : deflogic-method 'method-spec 'arg-pattern-list &rest 'forms
;; 論理メソッドを定義する。defmethod と同じように働く。defmethod を参照。
;; method-spec = (class-name method-type selector)
;; arg-pattern-list はヘッド部分に対応する。 forms はボディ部分に対応する。
;; forms には (&aux x y z) のような補助変数宣言がくることがある。論理メソ
;; ッドでは self を使用可能。
;;
;; <例>
;;         (defclass aclass () ()) -> aclass
;;         (deflogic-method (aclass amethod) (_x _y) (cons _x _y))
;;         	-> amethod
;;         (deflogic-method (aclass amethod) (_x _y _z) [_x + _y + _z])
;;         	-> amethod
;;         (deflogic-method (aclass amethod) ((_p . _)) _p) -> amethod
;;         (!x (make-instance 'aclass)) -> {udo}76445aclass
;;         [x amethod p (q r s)] -> (p q r s)
;;         [x amethod 1 2 3] -> 6
;;         [x amethod (aa bb cc)] -> aa
;;         [x amethod aa bb cc dd] -> nil

(defclsynonym tao:defmacro
    "defmacro                               関数[#!expr]

<説明>
  形式 : defmacro 'fn 'var-list &rest 'body
fn が名前、var-list が引数リストのマクロ関数を body で定義する。

<例>
        (defmacro first (x) (list 'car x)) -> first")

;; defmethod                              関数[#!macro]
;;
;; <説明>
;;   形式 : defmethod 'method-spec 'arg-list &rest 'forms
;; method-spec は (class-name message-pattern) または、
;;                (class-name method-type message-patern) という形式。
;; method-type は :before, :primary, :after, :or, :and, :list,
;; :inverse-list, :nconc, :progn のいずれかで、メソッド結合の時に使われる。
;;
;; クラス class-name に、メッセージ名 message-pattern で呼び出される
;; メソッドを定義し、そのメッセージ名を返す。 message-pattern は、
;; メッセージを送るときに使われ、メソッド名とも呼ばれる。
;;
;; メソッドには id-method と list-method の 2 種類がある。
;; id-method を定義するとき、この id-method で使われる引数は、 arg-list の
;; 部分にリストにして書く。このリスト中にはキーワード &aux を書ける。
;;
;; forms でメソッドのボディが指定される。
;; メソッドが呼び出されると、このボディ中の式が一つずつ評価される。
;; list-method が定義されると、arg-list は、forms の一部分とみなされる。
;;
;; list-massage は id-message と同じ役割を果たすことができる。
;; しかし list-message は id-message と違って message-pattern をユニフィケ
;; ーションのパターンとして使うことができる。しかし list-message では送ら
;; れるメッセージ名と一致するメソッド名をもつメソッドが起動されるが、
;; list-method では送られるメッセージ名とユニファイできるメソッド名をもつ
;; メソッドが起動される。それゆえ list-message の message-pattern は論理
;; 変数をその一部分として含むこともある。
;;
;; メソッドのボディ (forms) の中では、クラス変数は関数 cvar を使うことによ
;; りアクセスされる。
;; スーパクラスのメソッドは、関数 super によって実行される。
;; クラスはメソッド結合で指定された方法で、全てのスーパクラスの、全ての
;; メソッドを継承する。
;;
;; <例>
;;         (defclass a1 () (b1) () :gettable :settable) -> a1
;;         (defclass a2 () (b2) (a1) :gettable :settable) -> a2
;;         (defclass a3 () (b3) (a2) :gettable :settable) -> a3
;;         (defmethod (a1 mult) () (!!* !b1 b2)) -> mult
;;         (defmethod (a3 (which is larger))
;;            (cond ([b1 > b2] 'b1) (t 'b2)) ) -> (which is larger)
;;         (!cc (make-instance 'a3 b1 10 b2 20)) -> {udo}44994a3
;;         [cc b1] -> 10
;;         [cc b2] -> 20
;;         [cc mult] -> 200
;;         [cc b1] -> 200
;;         [cc b2] -> 20
;;         [cc (which is larger)] -> b1
;;         (goal (== _x ,(cc (which is _y))))
;;       	_y = larger
;;    	     _x = b1 ;
;;       	no

(defclsynonym tao:defparameter
  #.(string '#:|defparameter                           関数[#!expr]

<説明>
  形式 : defparameter symbol init-value &opt doc
symbol をグローバル変数として宣言し、init-value を初期値とする。
関数 defvar では初期値が与えられる前に値が代入されていたら初期値は無視
されるが、この関数では初期値が優先 (代入)。doc はドキュメンテーション
文字列。

<例>
        (!aa 0) -> 0
        (defparameter aa 1) -> aa
        aa -> 1
        (defparameter aa (- 4 1)) -> aa
        aa -> 3|))

;; defprop                                関数[#!macro]
;;
;; <説明>
;;   形式 : defprop p-list val ind
;; 属性リスト p-list において ind と eq な最初のインディケータに対応し
;; た属性値を val に置き換え ind を返す。 eq なインディケータがない場合
;; は、 p-list に ind と val のペアをコンスし ind を返す。
;; p-list は破壊される。
;; 引数が評価されないこと以外は putprop と同じ。
;; (defprop foo bar next-to) = (putprop 'foo 'bar 'next-to)
;; 連想リストのかわりに属性リストを扱う点を除いて、putalist と同じ。
;;
;; <例>
;;         (!(plist 'xxx) '(q 2 r 3 s 4)) -> (q 2 r 3 s 4)  そして
;;         xxx の属性リストは、 (q 2 r 3 s 4)
;;         (!yyy (plist 'xxx)) -> (q 2 r 3 s 4)  そして
;;         (defprop xxx 1 p) -> 1  そして
;;         (plist 'xxx) -> (p 1 q 2 r 3 s 4)  しかし
;;         yyy = (q 2 r 3 s 4)
;;         (defprop xxx 5 s) -> 5
;;         (plist 'xxx) -> (p 1 q 2 r 3 s 5)  そして
;;         ここで  yyy = (q 2 r 3 s 5)

;; defrel                                 関数[#!expr]
;;
;; <説明>
;;   形式 : defrel p ((A1") (P11 B11") (P12 B12") ... (P1m B1m"))
;;          	  ((A2") (P21 B21") (P22 B22") ... (P2m B2m"))
;;          	  ...
;;          	  ((An") (Pn1 Bn1") (Pn2 Bn2") ... (Pnm Bnm"))
;; 主ファンクタが p である n 個のホーン節を定義する。
;; assert を n 回実行するのと同じ。
;; Pij は主ファンクタ、Aij" と Bij" はそれぞれ Aij と Bij から得たもの。
;; DEC10-Prolog では次のように記述される。
;; p(A1") :- P11(B11"), P12(B12"), ... (P1m(B1m").
;; p(A2") :- P21(B21"), P22(B22"), ... (P2m(B2m").
;;    ....
;; p(An") :- Pn1(B1n"), Pn2(Bn2"), ... (Pnm(Bnm").
;;
;; <例>
;;          (defrel concat ((() _x _x))
;;                        ((( _a . _x) _y ( _a . _z)) (concat _x _y _z)))
;;           -> concat
;;          (goal (concat _x _y (1 2))) ->
;;          _x = ()
;;          _y = (1 2);
;;          _x = (1)
;;          _y = (2);
;;          _x = (1 2)
;;          _y = ();
;;          _x = ()
;;          no
;;
;;          (defrel p ((A1") (P11 B11") (P12 B12")...(P1m B1m"))
;;                    ((A2") (P21 B21") (P22 B22")...(P2m B2m"))
;;                    ....
;;                    ((An") (Pn1 Bn1") (Pn2 Bn2")...(Pnm Bnm")))
;;          =
;;          (define p (hclauses (&+ (A1") (P11 B11") (P12 B12")
;;                                     ...(P1m B1m"))
;;                              (&+ (A2") (P21 B21") (P22 B22")
;;                                     ...(P2m B2m"))
;;                       ....
;;                              (&+ (An") (Pn1 Bn1") (Pn2 Bn2")
;;                                     ...(Pnm Bnm")))

(defclsynonym tao:defsetf
    "defsetf                                関数[#!macro]

<説明>
  形式 : defsetf &rest x
関数 setf が指定された関数式に対してどう働くかを定義する。
次の 2 つのどちらかの方法で用いる。
\(1)
\(defsetf access-fn update-fn)
access-fn は setf が新しい値を格納する場所を供給する関数。
update-fn は 場所内に新しい値を格納する関数。
つまり、(setf (access-fn argument) new-value)
は (access-fn argument) が供給する場所に新しい値を格納する。
\(2)
\(defsetf access-fn (arg1 arg2 ... ) (var) form)
\(access-fn arg1 arg2 ... ) は新しい値が格納される場所を供給する。
一時的な変数である var は place に格納されるべき新しい値を持つ。
formは (access-fn arg1 arg2 ... ) によって指定された場所に新しい値を
格納するのに使用。
両方法共に access-fn は関数、またはマクロの名前でなくてはならない。

<例>
        (defsetf symbol-value-set) ->
                                   {applobj}33390(#!subr-simpl . 6)
        (defsetf substring (str a &opt z) (new-var)
        	｀(seq (replace ,str ,new-var :string1 ,a :end1 ,z)
        	 ,new-var))
        	-> ((#:g1 #:g2 #:g3) (#:g4)
        	      (replace #:g1 #:g4 :star1 #:g2 :end1 #:g3) #:g4)
        上記の 2 番目の例の返される値について get-setf-method を参照。")

;; defstruct                              関数[#!macro]
;;
;; <説明>
;;   形式 : defstruct 'name-opts &rest 'slots
;; レコード構造のデータ型を定義する関数。一般的には、次の様に呼び出す。
;; (defstruct (symbol option-1 option-2 ... )
;;             doc-string
;;             slot-description-1 slot-description-2 ... )
;; symbol が構造の全具体例を構成する新しいデータ型の名前となる。
;; symbol がリターン値。
;; slot-description-1 slot-description-2 ... は、それぞれ次のような形を
;; している。
;; (slot-name default-init
;;         slot-option-name-1 slot-option-value-1
;;         slot-option-name-2 slot-option-value-2
;;         ...)
;; slot-name は、シンボル。
;; default-init は、構造が作られる度に評価される形式で、その値はスロット
;; の初期値として用いられる。
;; 構造を定義するフォームの評価は以下のことを含んでいる。
;; ・レコードの実体をアクセスするためのアクセス関数が、それぞれのスロット
;;   に対して定義される。
;; ・レコードの実体を生成するコンストラクタ関数が定義される。
;; ・name は、関数 typep で受け入れ可能なものとなる。
;; ・#s 構文が、構造の具体例を読むために用いることができる。
;; ・オブジェクトが与えられた時に、その与えられたもののコピーである新しい
;;   オブジェクトを生成するコピー関数が定義される。
;; ・関数 setf を用いて、それぞれのスロットの実体を変更することができる。
;; ・name により name-p という 1 引数の関数が定義される。
;;
;; <例>
;;         (defstruct date day month year) -> date
;;         コンストラクタ関数 make-date 、
;;         アクセス関数 date-day, date-month, date-year が用意される。
;;         (setq today (make-date :day 1 :month 6 :year 1987)
;;         		-> #S(date day 1 monyh 6 year 1987)
;;         (date-day today) -> 1
;;         (date-month today) -> 6
;;         (setf (date-day today) 3) -> 3
;;         (date-day today) -> 3
;;         (typep today date) -> t
;;         (typep tommorow date) -> nil

(defclsynonym tao:deftype
    "deftype                                関数[#!expr]

<説明>
  形式 : deftype symbol lambda-list &rest body
body で新しい型指定子を定義し、それを symbol に結び付ける。
lambda-list は新しいデータタイプが必要とする引数のリスト。
マクロと同様に動作する。lambda-list 中のオプショナル変数の初期値は * 。

<例>
        (deftype modulus (n) `(integer 0 (,n))) -> modulus
        (deftype str-cha () '(or character string)) -> str-cha
        str-cha は文字またはストリングを表す新しいデータタイプ。
        (square-matrix number 10) は (array number 10 10) と、型指定子
        に関して同等。")

(defclsynonym tao:defun
    "defun                                  関数[#!expr]

<説明>
  形式 : defun 'fn 'var-list &rest 'body
fn を名前、var-list を引数リストとする大域関数を body で定義する。
var-list にはキーワード引数 &optional, &rest, &key, &aux を含むことが
できる。

<例>
        (defun fact (n)
               (cond ((n = 0) 1)
                     (t (n * (fact (n - 1)))) )) -> fact
        (defun tatoeba (x y &optional p q &aux m)
                   (!m (index (!!or !p 0) (!!or !q 9)))
                   (image i m (+ x (y / i)))) -> tatoeba")

(defclsynonym tao:defvar
    "defvar                                 関数[#!macro]

<説明>
  形式 : defvar symbol &opt init-value doc
symbol をスペシャル変数として定義し、init-value (省略時 nil)を初期値と
するが、グローバル値を持っているならは init-value を評価せずグローバル
値をも再代入しない。doc はドキュメンテーション文字列。

<例>
        (defvar aa 10) -> aa
        aa -> 10
        (defglobal bb 12) -> bb
        bb -> 12
        (defvar bb 100) -> bb
        bb -> 12")

;; del                                    関数[#!subr]
;;
;; <説明>
;;   形式 : del pred item list &opt n
;; list の要素を順に item とともに述語 pred に適用し、その述語を満足する
;; 要素を n 個取り除く。 pred は引数を 2 つとる関数で、第 1 引数は item 、
;; 第 2 引数は list の各要素。n が、省略されたり pred を満足する要素の個数
;; 以上の数の場合、あるいは負の場合は、pred を満足するすべての要素を
;; 取り除く。list は破壊される。rem の破壊版。
;;
;; <例>
;;         x を (1 2 3 4 5 6 7) とする
;;         (del #'< 4 x) -> (1 2 3 4)
;;         (del #'< 4 x 2) -> ( 1 2 3 4 7)

;; del-alist                              関数[#!subr]
;;
;; <説明>
;;   形式 : del-a-list item a-list
;; 連想リスト a-list の要素対のうち、第 1 要素が item と eq な要素対を
;; a-list から破壊的に削除し、その結果を返す。そのような要素がなければ
;; a-list を返す。代入形式で使用する。
;;
;; <例>
;;         (!x '((a . 1) (b . 2) (c . 3))) -> ((a . 1) (b . 2) (c . 3))
;;         (del-alist 'a x) -> ((b . 2) (c . 3))
;;         x -> ((a . 1) (b . 2) (c . 3)).
;;         (del-alist 'b x) -> ((a . 1) (c . 3))
;;         x -> ((a . 1) (c . 3))
;;         (!x (del-alist 'p x)) -> ((a . 1) (c . 3))

;; del-if                                 関数[#!macro]
;;
;; <説明>
;;   形式 : del-if pred list
;; list から、述語 pred を満足する要素をすべて削除し、その結果を返す
;; ( list は破壊される)。 pred は引数を 1 つしかとらない。すなわち、
;; list の各要素。
;; rem-if の破壊版。
;;
;; <例>
;;         x = (1 a 2 b 3 c 4 d 5)
;;         (del-if #'integerp x) -> (a b c d)

;; del-if-not                             関数[#!macro]
;;
;; <説明>
;;   形式 : del-if-not pred list
;; list から、述語 pred を満足しない要素をすべて削除し、その結果のリスト
;; を返す ( list は破壊される)。 pred は、引数を 1 つしかとらない。
;; rem-if-not の破壊版。

;; delete                                 関数[#!macro]
;;
;; <説明>
;;   形式 : delete item sequence
;;          &key :from-end :test :test-not :start :end :count :key
;; sequence の :start から :end までの範囲で item を :count 個削除し、その
;; 結果を返す。  関数 remove の破壊版。
;;
;; <例>
;;         (delete 4 '(1 2 4 1 3 4 5)) -> (1 2 1 3 5)
;;         (delete 4 '(1 2 4 1 3 4 5) :count 2) -> (1 2 1 3 5)
;;         (delete 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) ->
;;                                                  (1 2 4 1 3 5)
;;         (delete 3 '(1 2 4 1 3 4 5) :test #'>) -> (4 3 4 5)

;; delete-dir                             関数[#!expr]
;;
;; <説明>
;;   形式 : delete-dir dir
;; ディレクトリ dir を削除する。
;;
;; <例>
;;         (delete-dir "cs:<work>")

;; delete-duplicates                      関数[#!macro]
;;
;; <説明>
;;   形式 : delete-duplicates seq &key :from-end :test :test-not
;;                                     :start :end :key
;; シーケンス seq の :start から :end までの範囲内の要素のうち重複する要素
;; を前方から取り除き (from-end が nil 以外なら後方)、 最後の要素
;; (from-end が nil 以外なら前の 1 つ) を残し、その結果を返す。
;;
;; <例>
;;         (!x '(a b c b d d e)) -> (a b c b d d e)
;;         (delete-duplicates x) -> (a c b d e)
;;         X -> (a c b d e)

;; delete-file                            関数[#!expr]
;;
;; <説明>
;;   形式 : delete-file file
;; ファイルまたはファイル群 file を削除する。file は、文字列、パス名の
;; udo 、ストリームの udo のいずれか。
;; 削除に成功すれば nil でない値を返し、削除操作に成功しなければエラーを
;; 警告する。最新バージョンファイルのみ削除する。
;;
;; <例>
;;         (delete-file "qwe.tao") -> ("cs:<dire>qwe.tao.1")
;;         (delete-file "foo.*.*")は
;;         カレントディレクトリ中の foo という名のファイルを全て削除する。
;;         (delete-file "*.abc")は
;;         カレントディレクトリ中の拡張子 (type) が "abc" のファイルを
;;         全て削除する。

;; delete-if                              関数[#!macro]
;;
;; <説明>
;;   形式 : delete-if test seq &key :from-end :start :end :count :key
;; シーケンス seq の :start から :end までの範囲内で、条件 test を満足
;; する要素を :count 個削除し、その結果を返す。remove-if の破壊版。
;;
;; <例>
;;         (delete-if #'odpp '(1 2 4 1 3 4 5)) -> (2 4 4)
;;         (delete-if #'odpp '(1 2 4 1 3 4 5) : count 1 : from-end t)
;;                            	-> (1 2 4 1 3 5)

;; delete-if-not                          関数[#!macro]
;;
;; <説明>
;;   形式 : delete-if-not test seq &key :from-end :start :end :count :key
;; シーケンス seq の :start から :end までの範囲内で、条件 test を満足
;; しない要素を :count 個削除し、その結果を返す。
;; remove-if-not の破壊版。
;;
;; <例>
;;         (delete-if-not #'oddp '(1 2 4 1 3 4 5)) -> (1 1 3 5)
;;         (delete-if-not #'oddp '(1 2 4 1 3 4 5) :count 1 :from-end t)
;;                                 -> (1 2 4 1 3 5)

;; delq                                   関数[#!subr]
;;
;; <説明>
;;   形式 : delq item list &opt n
;; list から item と eq な要素を n 個削除し、その結果を返す。
;; (delq x y z) = (del eq x y z)
;;
;; <例>
;;         (delq 'a '(b a c (a b) d a e)) -> (b c (a b) d e)
;;         (delq '(1 2) '(b (1 2) c ((1 2) b) d (1 2) e)) ->
;;         			   (b (1 2) c ((1 2) b) d (1 2) e)
;;         {(eq '(1 2) '(1 2)) -> nil}

;; delql                                  関数[#!subr]
;;
;; <説明>
;;   形式 : delql item list &opt n
;; list から item と eql な要素を n 個削除し、その結果を返す。
;; (delql x y z) = (del eql x y z)
;;
;; <例>
;;         (delql "ab" '(ab "ab" c "de" "ab")) -> (ab c "de")

;; delqu                                  関数[#!subr]
;;
;; <説明>
;;   形式 : delqu item list &opt n
;; list から item と equal な要素を n 個削除し、その結果を返す。
;; (delqu x y z) = (del equal x y z)
;;
;; <例>
;;         (delqu '(1 2) '(b (1 2) c ((1 2) b) d (1 2) e))
;;         	-> (b c ((1 2) b) d e)

;; denominator                            関数[#!expr]
;;
;; <説明>
;;   形式 : denominator number
;; number (整数又は分数) をとり、その分母を正の整数で返す。
;; 整数の分母は 1 。
;;
;; <例>
;;         (denominator 3) -> 1
;;         (denominator -1/3) -> 3
;;         (denominator 1/3) -> 3
;;         (denominator 8/2) -> 1

;; deposit-field                          関数[#!expr]
;;
;; <説明>
;;   形式 : deposit-field newbyte bytespec integer
;; integer の bytespec で指定されたバイトを newbyte の bytespec で指定
;; されたバイトで置き換え、その結果を返す。
;;
;; <例>
;;         (deposit-field 1 (byte 1 2) 3) -> #3
;;         (deposit-field 2 (byte 3 5) 10) -> #12

;; deref                                  関数[#!subr]
;;
;; <説明>
;;   形式 : deref locbit
;; locbit で指定された 64-bit-word にある値を 8 進数で返す。
;;
;; <例>
;;         (!x (locbit (get-memblk #!8b-memblk 20))) ->
;;                       {locbit}({memblk}480569(#!8b-memblk . {dnil}20))
;;         (!(deref x) 768) -> 768
;;         (deref x) -> #0
;;         (!(deref x) 123) -> 123
;;         (deref x) -> #173
;;         (> 200 (deref x)) -> #173
;;         (deref x) は、@x と略記することができるが、現在使用できない。
;;         (!@x #123) -> #123
;;         @x -> #123
;;         (!aaa @x) -> #123

;; describe                               メッセージ
;;
;; <説明>
;;   メッセージとして使われた describe は、関数として使われた describe
;; と同じ。
;; [obj describe] = (describe obj)

;; describe                               関数[#!expr]
;;
;; <説明>
;;   形式 : describe object
;; object に関する参考になる情報をプリントする。
;;
;; <例>
;;         (defclass aaa ()
;;          ((a '(white . black)) (b "abcdefg") (c 12345)) ) -> aaa
;;         (!aa (make-instance 'aaa)) -> {udo}59617aaa
;;         (describe aa) -> {udo}59617aaa, an object of class aaa
;;         		 (version 0),
;;                               has instance variable values:
;;                                   a:    (white . black)
;;                                   b:    "abcdefg"
;;                                   c:    12345
;;                                 nil

;; describe-operations                    メッセージ
;;
;; <説明>
;;   このメッセージのレシーバに送ることのできるメッセージに対応した
;; メソッド名とボディがプリントされ、レシーバそれ自身が返される。
;;
;; <例>
;;         (defclass tempo () () ()) -> tempo
;;         (!temp (make-instance 'tempo)) -> {udo}38091tempo
;;         [temp get-handler-for 'get-handler-for]
;;            -> Getting inherited (and combined) method
;;         	 (get-handler-for)
;;               (defmethod (temp get-handler-for)
;;                          (operation &opt type)
;;                          (bas:method-body (bas:get-class-vect self)
;;                           operation
;;                          (ifundef type ':primary))
;;         [temp operation-handle-p 'operation-handle-p]
;;            -> {applobj}38112(#!expr-simple . 6)
;;         [temp describe-operations] とすると、以下のようにプリントする:
;;         Id methods are:
;;            Message pattern `describe-operations' with no argument
;;          whose body is: (bas:print-methods-of-obj self)
;;            Message pattern `which-operations' with no argument
;;          whose body is: (which-operations self)
;;            Message pattern `operation-handle-p'
;;                             with one argument operation
;;          whose body is: (bins (bas:id-msg-slot
;;                                     (bas:get-class-vect self))
;;                                                     operation)
;;            Message pattern `get-handler-for'
;;                             with 3 arguments: (operation &opt type)
;;          whose body is: (bas:method-body (bas:get-class-vect self)
;;                                                     operation
;;                          (ifundef type ':primary))
;;            Message pattern `describe' with no argument
;;          whose body is: (describe self)
;;            -> {udo}38091temp

;; dev-dir-namestring                     関数[#!expr]
;;
;; <説明>
;;   形式 : dev-dir-namestring pathname
;; pathname のデバイスとディレクトリをストリングで返す。
;;
;; <例>
;;         (dev-dir-namestring "To::bs:<anata>konata.sonota") ->
;;         	"bs:<anata>"
;;         (dev-dir-namestring (merge-pathname "bs:<kore>are.sore")) ->
;;         	"bs:<kore>"

;; diff                                   関数[#!expr]
;;
;; <説明>
;;   形式 : diff file1 file2
;; ファイル file1 と file2 を比較し、それらの間の違いを表示する。
;;
;; <例>
;;         (diff "foo.lsp.1" "foo.lsp.2")
;;         ->  foo.lsp.1 at 10
;;                  (print "ver 1.0")
;;                  (do-something)
;;         	foo.lsp.2 at 10
;;                  (print "ver 2.0")
;;                  (do-something)
;;                  ----------
;;               ok

;; digit-char                             関数[#!subr]
;;
;; <説明>
;;   形式 : digit-char integer &opt radix font-attr
;; フォント情報 font-attr を持ち、基数 radix 進法における integer を表す
;; 文字データを返す。そのような文字が存在しなければ、nil を返す。radix の
;; 既定値は 10。
;;
;; <例>
;;         (digit-char 7) -> "7"
;;         (digit-char 10) -> エラー
;;         (digit-char 12 16) -> "C"
;;         (char= "c" (digit-char 12 16)) -> nil
;;         (char-equal "c" (digit-char 12 16)) -> "C"
;;         (digit-char 6 2) -> エラー
;;         (digit-char 1 2) -> "1"

;; digit-char-p       未インプリメント    関数[#!subr]
;;
;; <説明>
;;   形式 : digit-char-p char &opt radix
;; 文字 char が、基数 radix 進法の数字を表す文字データならその数を返し、
;; そうでなければ、nil を返す。 radix の既定値は 10。
;;
;; <例>
;;         (digit-char-p #\7) -> 7
;;         (digit-char-p #\a) -> nil
;;         (digit-char-p #\b 16) -> 11

;; directory                              関数[#!expr]
;;
;; <説明>
;;   形式 : directory &opt pathname flag
;; pathname （デバイス、ディレクトリ）にマッチする全てのファイルのリスト
;; を返す。 pathname の既定値はカレントディレクトリ。
;; flag の値が nil (既定値) なら、返されるファイル名に各々バージョン番号
;; がつけられるが、 nil でなければバージョン番号は省略される。
;; 関数 all-files と同じような働きをする。
;;
;; <例>
;;         vdir "bs:<dire>"
;;           bs:<dire>
;;         asd.tao.1 .....................
;;         qwe.tao.2 .....................
;;           2 files 152 bytes.
;;         (vdir "bs:<dire>")
;;         (directory "bs:<dire>")
;;         	-> ("bs:<dire>asd.tao.1" "bs:<dire>qwe.tao.2")
;;         (directory "bs:<dire>" t)
;;         	-> ("bs:<dire>asd.tao" "bs:<dire>qwe.tao")

;; directory-namestring                   関数[#!expr]
;;
;; <説明>
;;   形式 : directory-namestring pathname
;; 文字列イメージの pathname によって指定されたファイルのディレクトリ名
;; を返す。pathname-directory 参照。
;;
;; <例>
;;         (directory-namestring "Ti::bs:<anata>konata.sonata.5") ->
;;         	"<anata>"

;; dired                                  関数[#!expr]
;;
;; <説明>
;;   形式 : dired &opt pathname
;; ディレクトリエディタを呼び出す関数。
;; pathname で、エディトするディレクトリまたはファイル群を指定する。
;;
;; <例>
;;         dired *.tao  (ファイル type が tao のものだけを抽出する)
;;            asd.tao.10       24-Apr-87  11:59     0  dire      698
;;            kkk.tao.1	     6-Mar-87   9:03     0  dire     1056
;;            taot.tao.1	    18-Feb-87  13:03     0  dire     2345
;;            qwe.tao.3	     3-Apr-87  15:45     0  dire      856
;;         [ Dired  (?:help) ]    Ho::bs:<dire>*.tao
;;         関数 dired に入った後、? をタイプすると、
;;         以下のようにコマンドリストが得られる。
;;         q:quit, r:reenter, d/u:(un)delete, n:next p:previous, e:edit,
;;         v:view, l:load, s:dump, x:undump, k:kill<-dired-list,
;;         f:file->dired-list, b:backup(knotfork)
;;           q: dired の終了。
;;           r: 画面の再表示。
;;           d: ファイルの削除。
;;           u: 削除したファイルの復活。
;;           n: 次のファイルを選択。
;;           p: 前のファイルを選択。
;;           e: ファイルをエディト (更新可)。
;;           v: ファイルをエディト (更新不可・参照のみ)。
;;           l: ファイルをロード。
;;           s: ファイルをフロッピーにダンプ。
;;           x: フロッピーへのダンプをキャンセル。
;;           k: dired-list を消去。
;;           f: dired-list を作成。
;;           b: 他のディレクトリへコピー。

(defclsynonym tao:disassemble
    "disassemble                            関数[#!expr]

<説明>
  形式 : disassemble symbol
symbol で指定されたコンパイルコード (アセンブリコード) を表示する。
symbol は、関数オブジェクト、ラムダ式、あるいは関数定義を持つシンボルの
いずれか。対象となる関数がコンパイルされた関数でなければ、それがまず
コンパイルされる。コンパイルされたコードは、次に \"逆アセンブル\" され、
そして、記号形式で印字表現される。")

;; dlogand                                関数[#!subr]
;;
;; <説明>
;;   形式 : dlogand x &rest y
;; 引数のビット対応の論理積を求め、その結果を返す。
;; logand の破壊版。
;;
;; <例>
;;         (dlogand #10 #34) -> #10 (shortnum)
;;         (dlogand #22 #95) -> #0
;;         (!a '#123) -> #123
;;         (!b '#456) -> #456
;;         (dlogand a b) -> #2
;;         a -> #123
;;         b -> #456

;; dlogior                                関数[#!subr]
;;
;; <説明>
;;   形式 : dlogior x &rest y
;; 引数のビット対応の論理和を求め、その結果を返す。
;; logior の破壊版。
;;
;; <例>
;;         (dlogior #10 #2 #230) -> #232
;;         (dlogior #117 #23) -> #137
;;         (!a '#123) -> #123
;;         (!b '#456) -> #456
;;         (dlogior a b) -> #577
;;         a -> #123
;;         b -> #456

;; dlognot                                関数[#!subr]
;;
;; <説明>
;;   形式 : dlognot arg
;; arg のビット対応の補数を求め、その結果を返す。
;; arg の内容は破壊される。lognot の破壊版。
;;
;; <例>
;;         (dlognot #777) -> #77777000
;;         (!x 0) -> 0
;;         (dlognot x) -> #77777777
;;         x -> #77777777
;;         (!a '#123) -> #123
;;         (dlognot a) -> #77777654
;;         a -> #123

;; dlogxor                                関数[#!subr]
;;
;; <説明>
;;   形式 : dlogxor bit-array1 &rest bit-array2
;; ビット配列 bit-array1 と bit-array2 の排他的論理和を求め、その結果を
;; 返す。引数の内容は破壊される。logxor の破壊版。
;;
;; <例>
;;         (dlogxor #1234 #567) -> #1753
;;         (!x #12) -> #12
;;         (!y #56) -> #56
;;         (dlogxor x y) -> #44
;;         (!a '#123) -> #123
;;         (!b '#456) -> #456
;;         (dlogxor a b) -> #575
;;         a -> #123
;;         b -> #456

;; dlsh                                   関数[#!subr]
;;
;; <説明>
;;   形式 : dlsh shortnum1 shortnum2
;; shortnum1 の内容を、shortnum2 の評価結果に従ってビットシフトし、
;; その結果を返す。shortnum1 の内容は破壊される。
;; shortnum2 の値が正なら左へ、負なら右へその値の絶対値だけビットシフトし、
;; その結果を返す。
;; (サインビットは保存され、空いたビット位置には、0 が詰めらる)。
;; lsh の破壊版。

;; dnil                                   クラス
;;
;; <説明>
;;   インスタンスとして "nil" だけをもつクラス。

(defclsynonym tao:do
    "do                                     関数[#!macro]

<説明>
  形式 : do ((var1 init1 step1)
       	     (var2 init2 step2)
             ... )
     	    (termination-test exit-form1 exit-form2 ...)
    	    form1 form2 ...)
init1 init2 ... を順に評価し、var1 var2 ... をそれらの評価結果に
それぞれ並列的に束縛し、termination-test を評価し、その結果が nil 以外
になるまで、form1 form2 ... を繰り返し評価する。評価を１回終える度ごと
に、関数式 step1 step2 ... を評価し、var1 var2 ... をそれらの評価結果に
並列的に束縛する（ stepi が指定されていなければ対応する vari への束縛は
行わない）。termination-test の評価結果が nil 以外になると、
exit-form1 exit-form2 ... を順に評価して、その最後のフォームの評価結果
を返す。それらのフォームが全く省略された時は nil を返す。

<例>
        (do ((i 1 (+ i 1)) (j 5 (- j 1)))
            ((i > 5) (!x (+ i j)))
          (!y i)
          (!z j))
        -> 6.
         x = 0, なら
        (do ((i (!x 1) (i + 2)) (j (!y (x + 4)) (1+ j)))
            ((i > j) (!z (- j i)))
          (!z (+ j i)))
        -> -1.")

(defclsynonym tao:do*
    "do*                                    関数[#!macro]

<説明>
  形式 : do* ((var1 init1 step1)
       	      (var2 init2 step2)
        	... )
     	     (termination-test exit-form1 exit-form2 ...)
    	     form1 form2 ...)
init1 の評価結果に var1 を束縛し、init2 の評価結果に var2 を束縛し、
.... と逐次的に束縛していき、 termination-test を評価し、その結果が
nil 以外になるまで、 form1 form2 ... を繰り返し評価する。評価を 1 回
終える度ごとに、step1 の評価結果に var1 を束縛し、step2 の評価結果に
var2 を束縛し、... と逐次的に束縛する (stepi が指定されていれば対応する
vari への束縛はおこなわない)。 termination-test の評価結果が nil 以外
になると、 exit-form1 exit-form2 ... を順に評価して、その最後のフォーム
の評価結果を返す。それらのフォームが全く省略された時は nil を返す。

<例>
        (do* ((i 1 (+ i 1)) (j 5 (- j 1)))
             ((i > 5) (!x (+ i j)))
             (!y i)
             (!z j))
             -> 6.
        x = 0, ならば
        (do* ((i (!x 1) (+1 i)) (j (!y (x + 4)) (+ i j)))
             ((j > 15) (!z (i + j)))
             (!z (- j i)))
             -> 24
        繰り返し中は i と j は次のように順に代入される。
        i = 1, 2, 3, 4, 5,  j = 5, 7, 10, 14, 19.
        最後のペア i = 5, j = 19 に関して termination-test で
        t が成立し、(!z (- j i)) は実行されない。")

;; do*-named                              関数[#!macro]
;;
;; <説明>
;;   形式 : do*-named name var-decl exit-cond &rest body
;; 内側の do* の実行中に外側にある do* から戻りたい場合に役立つ。
;; どの do* から戻りたいのか name を指定できる。リターンは return-from
;; を使用。do*-named と do-named の関係は do* と do の関係と同じ。

(defclsynonym tao:do-all-symbols
    "do-all-symbols                         関数[#!macro]

<説明>
  形式 : do-all-symbols vr &rest body
do-symbols と同じような操作を行なうが、システム内に登録されている
全てのシンボルに対して 1 度だけ本体が実行される点が異なる。
パッケージに登録されているシンボルに対して一定の操作を行なう。
最初にパッケージ式 (値がパッケージであるもの) を評価し、次に指定された
パッケージの全シンボルにバインドされ評価を行ない、最後に値が評価され
do-all-symbols 式の値となる。

<例>
        (do-all-symbols (i) (print i))
        	 -> typep$gen-vector-pred
        	    typep$gen-array-pred
        		  ...")

(defclsynonym tao:do-external-symbols
    "do-external-symbols                    関数[#!macro]

<説明>
  形式 : do-external-symbols vpr &rest body
do-symbols と同じような操作を行なうが、指定されたパッケージの外部
シンボルのみバインドされる点が異なる。

<例>
        (do-external-symbols (i sys:package) (print i))
        	 -> debug-on
        	    ansi$lf
                    ...")

(defmacro tao:do-forever (&body body)
  "do-forever                             関数[#!macro]

<説明>
  形式 : do-forever &rest body
body の中のどこかで return を実行するまで body の中の式の実行を繰り
返す。 body の中に return がなければ、繰り返しをいつまでも続ける。

<例>
        (do-forever
        (!aa (read))
          (if (equal aa 'ok) (retrun nil) (prins  \"input ok? : \")))
        \"ok\" と入力しないかぎり、do-foreverから抜け出ることはできない。"
  (let ((=> (gensym)))
    `(block nil
       (tagbody
          ,=>
          ,@body
          (go ,=>)))))

;; do-named                               関数[#!macro]
;;
;; <説明>
;;   形式 : do-named name var-decl exit-cond &rest body
;; 内側の do の実行中に外側にある do から戻りたい場合に役立つ。
;; どの do から戻りたいのか name を指定できる。
;; リターンは return-from を使用。
;;
;; <例>
;;         (do-named first-do
;;                ((i 1 (1+ i)))
;;                ((i > 5))
;;                (!x ((a * x) + b))
;;                (do-named second-do
;;                       ((j 1 (1+ j)))
;;                       ((j > 4))
;;                       (!x ((c * x) + d))
;;                       (cond ((x = 10)
;;                              (return-from first-do (cons i j)))))
;;             (!x (a + b)))
;; ＠
;; do-symbols                             関数[#!macro]
;;
;; <説明>
;;   形式 : do-symbols vpr &rest body
;; パッケージに登録されているシンボルに対して一定の操作を行なう。
;; 最初にパッケージ式 (値がパッケージであるもの) を評価し、
;; 次に指定されたパッケージの全シンボルにバインドされ評価を行ない、
;; 最後に値が評価され do-symbols 式の値となる。
;;
;; <例>
;;         (do-symbols (i (current-packge)) (print i))
;;         	 -> i-buffer-class
;;                     z
;;         		...

(defclsynonym tao:documentation
    #.(string '#:|documentation                          関数[#!expr]

<説明>
  形式 : documentation symbol type
symbol の、型 type についてのドキュメント（文書文字列）を返す。
そのようなドキュメントが存在しなければ nil を返す。
type と対応する構文要素は以下のようになる。
            型            構文要素
          function        defun
          function        defmacro
          variable        defvar
          variable        defparameter
          variable        defconstant
          structure       defstruct
          setf            defsetf
          type            deftype

<例>
        (defun vehicle (x)
           "equivalent to car"
           (car x)) -> vehicle
        (documentation 'vehicle 'function) -> "equivalent to car"
        (defvar *x* 3 "only for example") -> *x*
        (documentation '*x* 'variable) -> "only for example"|))

(defclsynonym tao:dolist
    "dolist                                 関数[#!macro]

<説明>
  形式 : dolist (var val-form [res-form]) &rest body
最初に val-form を評価する。結果はリストでないといけない。次に、その
リストの各要素を順に var に束縛し body を評価する。
res-form があれば、val-list の中の最後の要素に束縛された var を使って
res-form を評価する。
そして res-form があれば res-form の値を返し、それ以外なら nil を返す。

<例>
        (dolist (x '(a b c d)) (prin1 x) (princ \" \")) -> abcdnil
        (de list-reverse (list)
            (let ((y nil))
        	 (dolist (x list y)
        	     (!!cons x !y))))
        (list-reverse '(a b c)) -> (c b a)")

(defclsynonym tao:dotimes
    "dotimes                                関数[#!macro]

<説明>
  形式 : dotimes (var val-form [res-form]) &rest body
最初に val-form を評価し、整数 n を生成する。次に 0, 1, 2, ... n-1 に
束縛した var を使って、順に body を評価する（n が 0 か負なら評価はし
ない）。
res-form があれば、n に束縛された var を使って res-form を評価する。
そして res-form があればその値を返し、なければ nil を返す。

<例>
        (let ((y nil))
             (dotimes (x 10 y)
        	  (!!cons x !y))) -> (9 8 7 6 5 4 3 2 1 0)")

(defconstant tao:double-float-epsilon cl:double-float-epsilon
  "double-float-epsilon                   定数

<説明>
  システムで処理し得る最小の正の double-float が格納されている
システム定数であり、本システムでは、1.11022302462516f-16。")

(defconstant tao:double-float-negative-epsilon cl:double-float-negative-epsilon
  "double-float-negative-epsilon          定数

<説明>
  システムで処理し得る最小の負の double-float が格納されている
システム定数であり、本システムでは、5.55111512312578f-17。")

(defclsynonym tao:dribble
    "dribble            未インプリメント    関数[#!expr]

<説明>
  形式 : dribble &opt pathname
引数が指定されると、変数 *standard-input* と *standard-output* を再束縛
し、ファイル pathname へ入出力の会話のレコードを送る。
引数を省略すると、ドリブルファイルに対する入出力の記録を停止し、
close する。")

;; dsys                                   関数[#!expr]
;;
;; <説明>
;;   形式 : dsys &opt sec flag
;; flag の値が nil (既定値) なら、 sec (既定値は 5) 秒ごとに、システムの
;; 現在の状態がプリントされる。flag の値が mem ならメモリアロケーション、
;; メモリ使用の状態がプリントされる。
;; flag の値が io なら入出力デバイスの状態がプリントされる。
;; リターンキーで、画面が更新される。それ以外のキーで終了。 ctrl-\ により
;; 出ていくことはできない。
;;
;; <例>
;;         以下のフォーマットで情報が表示される。
;;                                                    24-Dec-85 10:11:12
;; job  ter  process     parent    job-name          status          load
;; ----------------------------------------------------------------------
;;   *    *  *           *         *                 *               61.8
;;   7    6  okuno       *         z                 running         18.1
;;   6    0  kamio       *         top-level         input-wait       0.0
;;   3    1  osato       *         z                 running         43.6
;;   1    4  not-login   *         login             input-wait       0.0
;;   0    0  monitor     *         interrupt-charac  mail-wait        0.0
;;
;;       (dsys 2 mem)   (2 秒毎に画面自動更新)
;; memblk-type  total-count  free-count  used-count     memory-load(%)
;; --------------------------------------------------------------------
;; 64bloc           11760       11677          83               0.7
;; strhead         125000       84060       40940              32.7
;; locbit            1000        1000           0               0.0
;; cell           1100000       27379     1072621              97.5
;; vector          250000      153342       96658              38.6
;; id               88000       77332       10668              12.1
;; str            1600000     1120819      479181              29.9
;; others          144752       96427       48325              33.3
;; dsys-end

;; dumb                                   関数[#!expr]
;;
;; <説明>
;;   形式 : dumb &opt terno
;; ターミナル番号 terno のターミナルを dumb モードにする。terno の
;; 既定値はこの関数を入力したターミナルである。

;; dumb-terminal                          関数[#!expr]
;;
;; <説明>
;;   形式 : dumb-terminal &opt terminal-type &rest mode
;; dumb モードのターミナルストリームをやめ *standard-input* 及び
;; *standard-output* に代入する。terminal-type の既定値は cit101e 。
;; mode の既定値は :more :no-wrap :no-screen 。

;; dump-to-floppy                         関数[#!expr]
;;
;; <説明>
;;   形式 : dump-to-floppy dir dev &opt query all
;; フロッピーディスク dev に  ディレクトリ dir にあるファイルをダンプする。
;; 随意引数 query と all の値により以下のようになる。
;; query  =  t    ------  ユーザは、ファイルをダンプするべきどうか
;;         	       ディスプレイ上で全ファイルをチェックできる。
;;        =  nil  ------  ユーザへの照会は、行われない。(既定値)
;; all    =  t    ------  全ファイルがダンプされる。
;;        =  nil  ------  最新バージョンのファイルだけダンプされる。
;;                        (既定値)
;;
;; <例>
;;         (dump-to-floppy "bs:<gonbe>" "dy1:")

;; コンパイラが自動でスペシャルにするのを期待するという手抜き
;; 本格的にはCodewalkerを利用しないと駄目
(defmacro tao:dye (fn var-list &body body)
  "dye                                    関数[#!expr]

<説明>
  形式 : dye 'fn 'var-list &rest 'body
fn を名前、var-list を引数リストとする exprdyn 型関数 (スコープ透過
関数) を body で定義する。

<例>
        (dye some-routine ()
             (!state (transition state))
             (cond ((state = 4) (!d-flag t))
                   ((state = 10) (!d-flag nil)) )
             (!result (fn state result)) )  ->  some-routine"
  `(defun ,fn ,var-list
     (declare (special ,@var-list))
     ,@body))

