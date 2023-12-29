(tao:common-lisp)
(in-package #:tao-internal)

;;; ＠
;;; labels                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : labels ((f-name1 (arg11 arg12 ... ) body1)
;;;                  (f-name2 (arg21 arg22 ... ) body2)
;;;                  ...
;;;        	 	form1 form2 ...
;;; ローカル変数を定義し、それを実行する。
;;; f-name1 f-name2 ... を関数名、arg11 arg12 ...、arg21 arg22 ... を引数、
;;; body1 body2 ... を関数本体とするローカル関数 (スコープ透過関数)を定義し、
;;; form1 form2 ... を逐次評価し、最後のフォームの評価結果を返す。これらの
;;; フォームが全く省略されると nil を返す。
;;; 定義された関数は、関数 dye によって定義されたと同じ。しかし、それらの
;;; スコープは labels 式内に制限される。ゆえに、ローカル変数 arg11 arg12
;;; arg21 arg22 ... は labels の外で値を取ることはできないし、定義された
;;; ローカル関数の関数本体で、labels の外で定義されたグローバルな関数を参照
;;; できる。定義されたローカル関数は、それらの関数本体で用いることができる。
;;; スコープを除いて、関数 flet と同じ。
;;;
;;; <例>
;;;         (defun f00 (x) (1+ x)) -> f00
;;;         (f00 10) -> 11
;;;         (labels ((f00 (x) (1- x))
;;;          	 (bar (x) (f00 x))) (bar 10)) -> 9
;;;         (labels ((f1 (x y) (x + y))
;;;         	 (g1 (a b) (+ (a * b) (f1 10 20))))
;;;          	 (g1 3 5)) -> 45
;;;         (labels ((f2 (x y) (x + y + p)
;;;         	 (g2 (p) (p * (f2 10 20))))
;;;         	 (g2 100)) -> 13000
;;; ＠
;;; lambda                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : lambda var-list body
;;; var-list を引数リストとするラムダ関数 (スコープ透過関数) をつくる。
;;; この式が評価されたときに関数のオブジェクトを生成。
;;; 動的なエクステントを持つ。
;;;
;;; <例>
;;;         (lambda (x) (x * x))
;;; ＠
;;; common:lambda                          関数[#!subr]
;;;
;;; <説明>
;;;   形式 : common:lambda var-list body
;;; var-list を引数リストとするラムダ関数 (スコープ透過関数) をつくる。
;;; この式が評価されたときに関数のオブジェクトを生成。
;;; 静的なエクステントを持つ。
;;;
;;; <例>
;;;         (common:lambda (x) (* x x) 3) -> 9
;;; ＠
;;; lambda-list-keywords                   定数
;;;
;;; <説明>
;;;   lambda-list-keywords = (&optional &optn &opt :opt &rest &aux
;;;         		  :aux &key &allow-other-keys &whole &body
;;;         		  &environment)
;;; ＠
;;; lambda-parameters-limit                定数
;;;
;;; <説明>
;;;   lambda-parameters-limit = 128

(setf (documentation 'tao:lappend 'function)
      "lappend                                関数[#!subr]

 <説明>
   形式 : lappend x y z
 ロジカルな (prolog 型) append 関数。
 下式と同じ。
 (assertz (lappend (_a . _x) _y (_a . _z)) (lappend _x _y _z))
 (assertz (lappend () _x _x))

 <例>
         (lappend (1) (2) (1 2)) -> t
")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (tao:defrel tao:lappend
    ((     (_a . _x) _y (_a . _z))
     (tao:lappend _x _y _z))
    ((     () _x _x))))

;;; #### CL ####
;;; last                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : last list
;;; list の最後のセルを返す。
;;;
;;; <例>
;;;         (last '(o w a r i)) -> (i)
;;;         (last nil) -> nil

;;; #### CL ####
;;; lcm                                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : lcm integer &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN の最小公倍数を返す。
;;;
;;; <例>
;;;         (lcm 14 35) -> 70
;;;         (lcm  0  5) ->  0
;;;         (lcm 1 2 3 4 5 6) -> 60
;;; ＠
;;; ldb                                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : ldb bytespec integer
;;; integer を 2 の補数で表し、そのバイト指定子 bytespec で指定されたビット
;;; 列を返す。
;;;
;;; <例>
;;;         (ldb (byte 1 2) 3) -> #0
;;;         (ldb (byte 13 5) 100) -> #3
;;; ＠
;;; ldb-test                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : ldb-test bytespec integer
;;; integer を 2 の補数で表し、そのバイト指定子 bytespec で指定されたビット
;;; 列の内、どれかのビットが 1 の場合、t を返す。
;;; すべてのビットが 0 なら nil を返す。
;;; <例>
;;;         (ldb-test (byte 1 2) 3) -> nil
;;;         (ldb-test (byte 13 5) 100) -> t
;;; ＠

;;; #### CL
;;; ldiff                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : ldiff list1 list2
;;; list2 が list1 のサブリストと eq ならば、list1 から list2 を除いた
;;; リストを返す。そうでなければ、list1 のコピーを返す。
;;;
;;; <例>
;;;         x = (a b c d e f)  とすると
;;;         (ldiff x (cdddr x)) -> (a b c) で
;;;         x = (a b c d e f)
;;;         しかし
;;;         (ldiff x '(d e f)) -> (a b c d e f)
;;; ＠

(defun tao:leap-year-p (int)
  "leap-year-p                            関数[#!expr]

<説明>
  形式 : leap-year-p integer
integer がうるう年ならば 0 を返し、そうでなければ nil を返す。"
  (cond ((zerop (mod int 400)) t)
	((zerop (mod int 100)) nil)
	((zerop (mod int 4)) t)
	('T nil)))

;;; ＠
;;; least-negative-double-float            定数
;;;
;;; <説明>
;;;   double-float 演算において取り得る最も小さい負の値が格納されている
;;; システム定数であり、このシステムの場合は -4.94065645841247f-324。
;;; ＠
;;; least-negative-long-float              定数
;;;
;;; <説明>
;;;   long-float 演算において取り得る最も小さい負の値が格納されている
;;; システム定数であり、このシステムの場合は -4.94065645841247f-324。
;;; ＠
;;; least-negative-short-float             定数
;;;
;;; <説明>
;;;   short-float 演算において取り得る最も小さい負の値が格納されている
;;; システム定数であり、このシステムの場合は -1.0842021724855s-19。
;;; ＠
;;; least-negative-single-float            定数
;;;
;;; <説明>
;;;   single-float 演算において取り得る最も小さい負の値を格納した
;;; システム定数であり、このシステムの場合は -4.94065645841247f-324。
;;; ＠
;;; least-positive-double-float            定数
;;;
;;; <説明>
;;;   double-float 演算において取り得る最も小さい正の値を格納した
;;; システム定数であり、このシステムの場合は 4.94065645841247f-324。
;;; ＠
;;; least-positive-long-float              定数
;;;
;;; <説明>
;;;   long-float 演算において取り得る最も小さい正の値を格納した
;;; システム定数であり、このシステムの場合は 4.94065645841247f-324。
;;; ＠
;;; least-positive-short-float             定数
;;;
;;; <説明>
;;;   short-float 演算において取り得る最も小さい正の値を格納した
;;; システム定数であり、このシステムの場合は 1.0842e-19。
;;; ＠
;;; least-positive-single-float            定数
;;;
;;; <説明>
;;;   single-float 演算において取り得る最も小さい正の値を格納した
;;; システム定数であり、このシステムの場合は 4.94065645841247f-324。
;;; ＠
;;; common:length                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:length seq
;;; シーケンス seq の長さ (要素数) を負以外の整数形式で返す。
;;; seq がフィルポインタを持つベクタの場合は、フィルポインタによって
;;; 示される実際の長さを返す。
;;;
;;; <例>
;;;         (common:length "abcde") -> 5
;;;         (common:length '(a b c))  -> 3
;;;         (!v (vcons "vec" 5))
;;;         	-> {vector}1839901("vector" . 5)
;;;         (common:length v) -> 5
;;;         (common:length #(a b c d)) -> 4
;;; ＠

(defun tao:length (arg)
  "length                                 関数[#!subr]

<説明>
  形式 : length arg
arg がリストなら、その長さ (要素の数) を返し、そうでなければ 0 を返す。

<例>
        (length '(a b c)) -> 3
        (length '(a b . c)) -> 2
        (length nil) -> 0
        (length 123) -> 0
        (length \"abcde\") -> 0"
  (and (listp arg)
       (do ((l arg (cdr l))
	    (cnt 0 (1+ cnt)))
	   ((endp l) cnt))))

#|(defun length (arg)
  (and (listp arg)
       (if arg
	   (1+ (length (cdr arg)))
	   0)))|#

(defun tao:lessp (&rest numbers)
  "lessp                                  関数[#!macro]

<説明>
  形式 : lessp &rest number1 number2 ... numberN
number1 number2 ... numberN を左から右に順に比較し、完全に単純増加
 (等しいものがあってもいけない) している場合は t を返し、そうでなければ
nil を返す。

<例>
        (lessp 9 10) -> t
        (lessp 10 9) -> nil
        (lessp 1 2 3 4 5) -> t
        (lessp 1 1) -> nil
        (lessp) -> t
        (lessp #c(2 3) #c(4 5)) -> エラー"
  (every #'< numbers (cdr numbers)))



(eval-when (:compile-toplevel :load-toplevel :execute)
(defun canonicalize-bvl (bvl)
  (mapcar (lambda (v)
            (etypecase v
              (symbol `(,v ,(if (var-name-p v)
                                `(tao:undef)
                                nil)))
              (cons v)))
          bvl)))

(deftype locative-declaration ()
  `(member tao:signed-integer-locatives
           tao:unsigned-integer-locatives
           tao:float-locatives))

(defun locative-declaration-p (form)
  (case (and (consp form) (car form))
    (tao:signed-integer-locatives :int64)
    (tao:unsigned-integer-locatives :uint64)
    (tao:float-locatives :double)
    (otherwise nil)))


(defmacro locally/deref (&body body)
  `(locally
     ,@(butlast body)
     (values-list (mapcar (lambda (v)
                            (if (tao:locbitp v)
                                (tao:deref v)
                                v))
                          (multiple-value-list ,@(last body))))))

(defmacro tao:let ((&rest bindings) &body body)
  "let                                    関数[#!subr]

<説明>
  形式 : let ((var1 val-form1)
              (var2 val-form2)
              ... )
             body
まず、フォーム val-form1 val-form2 ... を各々左から右へ順に評価する。
次に変数 val1 val2 ... を各々 val-form1 val-form2 ... の値に同時に束縛
する。そして body 中のフォームを左から右へ順に評価し、最後のフォームの
値を返す。val-formI は省略可能で、その場合、varI は nil となる。

<例>
        (!x 2) -> 2
        (let ((x 3) (y (* x x)))
              (* x y y)) -> 48"
  #+lispworks
  (if (locative-declaration-p (car body))
      (let ((type (locative-declaration-p (car body)))
            (locative-vars (cdr (car body))))
        `(fli:with-dynamic-foreign-objects (,@(mapcar (lambda (b)
                                                        (check-type b (and symbol (not null)))
                                                        `(,b ,type))
                                                      locative-vars))
                                           (cl:let (,@(remove-if (lambda (v)
                                                                   (find (car v) locative-vars))
                                                                 (canonicalize-bvl bindings)))
                                                   (locally/deref
                                                     ,@body))))
      `(cl:let (,@(canonicalize-bvl bindings))
               ,@body))
  #-lispworks
  `(cl:let (,@(canonicalize-bvl bindings))
           ,@body))


(defmacro tao:let* ((&rest bindings) &body body)
    "let*                                   関数[#!subr]

<説明>
  形式 : let* ((var1 val-form1)
               (var2 val-form2)
               ... )
              form1 form2 ...
まずフォーム val-form1 を評価し変数 var1 をその評価結果に束縛する。
次に val-form2 を評価し var2 をその評価結果に束縛する。
以下、逐次的に、val-formI を評価し varI をその評価結果に束縛していく。
そして、form1 form2 ... を順に評価し、最後のフォームの評価結果を返す。
val-formI は省略可能で、その場合、varI は nil となる。
ローカル変数の初期値の評価とその束縛は順に実行されるので先に束縛された
ローカル変数 (例えば var1) の束縛結果を次のローカル変数 (例えば var2)
の初期値の評価に使うことができる。

<例>
        (let* ((x 3) (y (* x x)))
              (* x y y)) -> 243"
    `(cl:let* (,@(canonicalize-bvl bindings)) ,@body))

(defun tao:lins (vector key)
  "lins                                   関数[#!subr]

<説明>
  形式 : lins vector key
vector の偶数番目の要素に key があれば、その次の要素の値を返し、
なければ nil を返す。ベクタの大きさが奇数の場合、最後の構成要素は検索
しない。

<例>
        (lins vec 2) -> \"b\""
  (do ((lim (1- (cl:length vector)))
       (idx 0 (+ idx 2)))
      ((< lim idx) nil)
    (and (equal (aref vector idx) key)
	 (>= lim (1+ idx))
	 (return (aref vector (1+ idx))))))

#|(let ((v #(1 one 2 two 3 three 4 four 5 five 6 six 7 seven 8 eight 9)))
  (lins v 9))|#

#|(let ((v (coerce (index 1 1000000) 'vector)))
  (time (lins v 999999)))
なんか知らんが、arefの方がsvrefより速い。allegroでは。
|#

(defclsynonym tao:lisp-implementation-type
    #.(string '#:|lisp-implementation-type               関数[#!expr]

<説明>
  特定の Common Lisp 処理系の一般の名前を表す文字列を返す。
ELIS システムでは、"TAO" を返す。

<例>
        "TAO"
        "SpiceLisp"
        "ZetaLisp"|))

(defclsynonym tao:lisp-implementation-version
    #.(string '#:|lisp-implementation-version            関数[#!expr]

<説明>
  特定の Common Lisp 処理系のバージョンを識別する文字列を返す。
ELIS システムでは、"25-Mar-87 TAO interpreter" を返す。

<例>
        "25-Mar-87 TAO interpreter"
        "1192"
        "53, 7 with complex numbers"
        "1746.9A,NEWIO 53, ETHER 5.3"|))

(defclsynonym tao:list
    "list                                   関数[#!subr]

<説明>
  形式 : list &rest arg1 arg2 ... argN
arg1 arg2 ... argN を要素とするリストを作成し、返す。各要素は、適用
される前に評価される。listq 参照。

<例>
        (list) -> ()
        (list 1) -> (1)
        (list 1 2 3 4 5) -> (1 2 3 4 5)
        (list '(1 2 3) '(4 5) 6) -> ((1 2 3) (4 5) 6)
        (list 3 4 'a (car '(b . c)) (+ 6 -2)) -> (3 4 a b 4)
        (list a b)      a または b が unboundならエラー")

;;; ＠

;;; #### CL
;;; list*                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : list* &rest arg1 arg2 ... argN
;;; arg1 arg2 ... argN を要素とするリストを作成し、返す。
;;; 最後の 2 つの要素はコンス (cons) される。
;;; 各要素は適用される前に評価される。
;;;
;;; <例>
;;;         (list* 'a 'b 'c 'd) = (cons 'a (cons 'b (cons 'c 'd)))
;;;                             = (a b c . d)
;;;         (list* 'a 'b '(c d)) = (a b c d)
;;;         (list* 'a) = a

;;; list-all-global-packages               関数[#!expr]
;;;
;;; <説明>
;;;   システムに存在する全ての大域パッケージのリストを返す。
;;;
;;; <例>
;;;         (package-name (list-all-global-packages))
;;;                 -> ("apropos" "net" "step" "sys" "key" "bas")
;;; ＠
;;; list-all-packages                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : list-all-packages &opt root
;;; 根パッケージ root (既定値 sys:univ-package) からアクセスできる全ての
;;; パッケージのリストを返す。
;;;
;;; <例>
;;;         (package-name (bas:list-all-packages))
;;;                 -> ("univ" "apropos" "net" "step" "sys" "key" "bas"
;;;                     "gonta" "hanako" "gonbe" "etc")
;;;         (list-all-packages) -> ({vector}61776(package . 12)
;;;         			{vector}41496(package . 12)
;;;                                 ...

;;; #### CL
;;; list-length                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : list-length list
;;; list が巡回リスト以外のリストの場合、list の長さを整数形式で返し、巡回
;;; リストなら nil を返す。
;;;
;;; <例>
;;;     (list-length'( ))  ->  0
;;;     (list-length'(a b c d)) -> 4
;;;     (list-length'(a (b c) d) -> 3
;;;     (let ((x (list 'a 'b 'c))
;;;           (rplacd (last x) x)
;;;           (list-length x)) -> nil
;;; ＠
;;; list-stream                            クラス
;;;
;;; <説明>
;;;   インスタンスがリストストリームであるクラス。
;;; このストリームに送られたデータは列を作り、そのデータは、FIFO 的に
;;; このストリームからとられる。
;;; ＠
;;; listen                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : listen &opt stream
;;; stream から即座に使用可能な文字が存在するのであれば (#¥return) 、
;;; そうでなければ ("" null ストリング) を返す。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1171307file-stream
;;;         (read aa) -> kyouwaiitenki
;;;         (listen aa) -> #¥return
;;;         (read aa) -> ashitamoiitenki
;;;         (listen aa) -> ""
;;;         (read aa) -> :eof
;;; ＠

(defun tao:listp (lst)
  "listp                                  関数[#!subr]

<説明>
  形式 : listp object
object がリストか、car 関数と cdr 関数が両方とも適用できるものであれば、
その評価値を返し、それ以外なら nil を返す。
object は listp 関数に適用される前に評価される。

<例>
        (listp '(a b c d)) -> (a b c d)
        (listp '[a b c d]) -> [a b c d]
        (listp 'a) -> nil
        (listp ''a) -> 'a
        (listp '^(a b `c d)) -> ^(a b `c d)
        (listp '(!x (fn y))) -> (!x (fn y))
        (listp (caddr '(!!cons 123 !x))) -> !x
        (listp (caddr '(list a b))) -> nil
        (listp ()) -> t
        (listp '(a . b)) -> (a . b)"
  (and (cl:listp lst) lst))

(defmacro tao:listq (&body args)
  "listq                                  関数[#!subr]

<説明>
  形式 : listq &rest arg1 arg2 ... argN
arg1 arg2 ... argN を要素とするリストを作成し、返す。
各要素は適用される前に評価されない。list 参照。

<例>
        (listq) -> ()
        (listq a) -> (a)
        (listq a b) -> (a b)
        (listq 'a 'b) -> ('a 'b)
        (listq a b c d e) -> (a b c d e)"
  `(list ,@(mapcar (lambda (x) `',x) args)))

;;; ＠
;;; load                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : load file
;;; file をロードする。
;;; file の既定値は、変数 *default-pathname-defaults* からとられる。
;;;
;;; <例>
;;;         (load "<dir1>file1.typ1")
;;;         (load "file2.tao")
;;; ＠
;;; load-factor-min                        関数[#!expr]
;;;
;;; <説明>
;;;   最新 1 分間の負荷係率を返す。
;;;
;;; <例>
;;;         (load-factor-min) -> 0
;;; ＠
;;; load-factor-sec                        関数[#!expr]
;;;
;;; <説明>
;;;   最新 1 秒間の負荷係率を返す。
;;;
;;; <例>
;;;         (load-factor-sec) -> 1
;;; ＠

(defun tao:load-if-non-existent (func file)
  "load-if-non-existent                   関数[#!expr]

<説明>
  形式 : load-if-non-existent func file
もし関数 func がロードされていなければ file をロードする。ロードされて
いれば nil を返す。"
  (if (fboundp func)
      nil
      (load file)))

#|(load-if-non-existent 'listz "tmp.tmp")|#

;;; ＠
;;; loc-diff                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-diff locbit1 locbit2
;;; ロックビット locbit1 のオフセット値 ( 0 から始まる数字) から、locbit2
;;; のオフセット値を引いた結果をメモリブロック内の語アドレスを示す
;;; shortnum で返す。これらのロックビットは同じメモリブロック内にある必要は
;;; ない。
;;;
;;; <例>
;;;         aaa を大きさが 20 の 8 ビットメモリブロックとする。
;;;         (!bbb (locbit aaa 5)) ->
;;;            {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}5)
;;;         (!ccc (locbit aaa 12)) ->
;;;            {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;;         (loc-diff bbb ccc) -> -7
;;;         (loc-diff ccc bbb) -> 7
;;; ＠
;;; loc-equate                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-equate locbit1 locbit2
;;; ロックビット locbit2 の値を locbit1 に代入し、その値を返す。
;;; locbit2 が shortnum なら locbit1 のオフセット値 (メモリブロック内の語
;;; アドレスを示す 0 から始まる数字) を locbit2 に設定し、その値を返す。
;;;
;;; <例>
;;;         aaa を大きさが 20 の 8 ビットメモリブロックとする。
;;;         (!bbb (locbit aaa 5)) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}5)
;;;         (!ccc (locbit aaa 12)) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;;         (loc-offset bbb) -> 5
;;;         (loc-offset ccc) -> 12
;;;         (loc-equate bbb ccc) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;;         (loc-offset bbb) -> 12
;;;         (loc-equate bbb 17) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}17)
;;;         (loc-offset bbb) -> 17
;;; ＠
;;; loc-greaterp                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-greaterp locbit1 locbit2
;;; ロックビット locbit1 のオフセット値 (メモリブロック内の語を指す 0 から
;;; 始まる数字) が、locbit2 のオフセット値より大きければ、locbit1 の値を返
;;; し、そうでなければ nil を返す。
;;;
;;; <例>
;;;         aaa を大きさ 20 の 8 ビットメモリブロックとする。
;;;         (!bbb (locbit aaa 5)) ->
;;;             {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}5)
;;;         (!ccc (locbit aaa 12) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;;         (loc-greaterp bbb ccc) -> nil
;;;         (loc-greaterp ccc bbb) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;; ＠
;;; loc-lessp                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-lessp locbit1 locbit2
;;; ロックビット locbit1 のオフセット値 (メモリブロック内の語を指す 0 から
;;; 始まる数字) が、locbit2 のオフセット値より小さければ、locbit1 の値を返
;;; し、そうでなければ nil を返す。
;;;
;;; <例>
;;;         aaa を大きさ 20 の 8 ビットメモリブロックとする。
;;;         (!bbb (locbit aaa 5)) ->
;;;           {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}5)
;;;         (!ccc (locbit aaa 12) ->
;;;            {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}12)
;;;         (loc-lessp bbb ccc) -> nil
;;;         (loc-lessp ccc bbb) ->
;;;             {locbit}({memblk}480396(#!8b-memblk . {dnil}20) . {dnil}5)
;;; ＠
;;; loc-memblk                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-memblk object
;;; object がロックビットなら、そのロックビットが格納されている
;;; メモリブロックへのポインタを返し、そうでなければエラーを返す。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!8b-memblk 20)) ->
;;;                               {memblk}480569(#!8b-memblk . {dnil}20)
;;;         (!bbb (locbit aaa)) ->
;;;                     {locbit}({memblk}480569(#!8b-memblk . {dnil}20))
;;;         (loc-memblk bbb) -> {memblk}480569(#!8b-memblk . {dnil}20)
;;; ＠
;;; loc-offset                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-offset locbit
;;; ロックビット locbit のオフセット値 (メモリブロック内の語を指す 0 から始
;;; まる番号) を shortnum で返す。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!8b-memblk 20)) ->
;;;                              {memblk}480569(#!8b-memblk . {dnil}20)
;;;         (!bbb (locbit aaa 15))->
;;;           {locbit}({memblk}480569(#!8b-memblk . {dnil}20) . {dnil}15)
;;;         (loc-offset bbb) -> 15
;;; ＠
;;; loc-size                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-size object
;;; object がロカティブなら、そのロカティブが格納されているメモリブロック
;;; の大きさを返し、ロカティブ以外の場合は、エラーを返す。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!8b-memblk 20)) ->
;;;                               {memblk}480569(#!8b-memblk . {dnil}20)
;;;         (!bbb (locbit aaa)) ->
;;;                      {locbit}({memblk}480569(#!8b-memblk . {dnil}20))
;;;         (loc-size bbb) -> 20
;;; ＠
;;; loc-type                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : loc-type object
;;; object がロカティブなら、そのロカティブが格納されているメモリブロック
;;; の種別を codnum で返し、ロカティブ以外ならエラーを返す。
;;;
;;; <例>
;;;         (!x (locbit (get-memblk #!8b-memblk 20) 3)) ->
;;;             {locbit}({memblk}480560(#!8b-memblk . {dnil}20) . {dnil}3)
;;;         (loc-type x) -> #!8b-memblk
;;; ＠
;;; local-echo                             関数[#!expr]
;;;
;;; <説明>
;;;   ローカルエコーモードにする。つまりターミナルストリームへの全ての入力
;;; がターミナルにエコーバックされる。
;;; ＠
;;; locally                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : locally &rest form1 form2 ... formN
;;; form1 form2 ... formN を局所的かつ浸透的なフォームとして定義する。
;;; いかなる変数をも束縛しない。
;;;
;;; <例>
;;;           (locally (declare (inline floor) (notinline car cdr))
;;;         	   (declare (optimize space))
;;;       		(floor (car x) (cdr y))
;;; ＠
;;; locativep                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : locativep object
;;; object が 64 ビットの符号なしの整数ロカティブ、64 ビットの符号付きの
;;; 整数ロカティブ、64 ビットの浮動小数点ロカティブ、あるいはロックビット
;;; ならば評価値を返し、それ以外なら nil を返す。
;;; (locativep x) = (or (64b-signedp x) (64b-unsignedp x)
;;;                     (64b-floatp x)  (locbitp x) )

#+lispworks
(defun tao:locbit (memblk &optional (offset 0))
  "locbit                                 関数[#!subr]

<説明>
  形式 : locbit memblk &opt offset
ロックビットを生成し返す。
生成されたロックビットは、メモリブロック memblk と、memblk 内の offset
が指定する語をポイントする。
offset の値は 0 から始まる番号で既定値は 0 。

<例>
        (!a (get-memblk #!8b-memblk 16))
        -> {memblk}480764(#!8b-memblk . {dnil}16)
        a は、生成された8ビットメモリブロックへのポインタ。
        (!b (locbit a 10)) ->
        {locbit}({memblk}480764(#!8b-memblk . {dnil}16) . {dnil}10)
        b は、メモリブロック a とメモリブロック a の 10 番目の語を指す。
        (loc-offset b) -> 10"
  (let ((locbit (fli:make-pointer :address (fli:pointer-address memblk)
                                  :type (fli:pointer-element-type memblk))))
    (fli:incf-pointer locbit offset)
    locbit))

#+lispworks
(defun tao:locbitp (obj)
  "locbitp                                関数[#!subr]

<説明>
  形式 : locbitp object
object がロックビットなら評価値を返し、それ以外なら nil を返す。

<例>
        (!a (get-memblk #!8b-memblk 16))
                           -> {memblk}480764(#!8b-memblk . {dnil}16)
        a は生成された 8-bit のメモリブロックへのポインタ。
        (!b (locbit a 10)) ->
        {locbit}({memblk}480764(#!8b-memblk . {dnil}16) . {dnil}10)
        (signed-integer-locatives c) -> (c)
        (locbitp b) ->
        {locbit}({memblk}480764(#!8b-memblk . {dnil}16) . {dnil}10)
        (locbitp 'b) -> nil
        (locbitp c) -> nil
        (locbitp 12) -> nil"
  (fli:pointerp obj))

;;; ＠
;;; log                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : log number1 &opt number2
;;; number2 (既定値は e : 自然対数の底) を底とする number1 の対数を返す。
;;;
;;; <例>
;;;         (log 8.0 2) -> 3.0f0
;;;         (log 100.0 10) -> 2.0f0
;;;         (log 8 2) -> 3.0f0
;;; ＠
;;; logand                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : logand integer &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の論理積を求め、その結果を
;;; 8 進数で返す。
;;;
;;; <例>
;;;         (logand #10 #34) -> #10
;;;         (logand #10 #10) -> #10
;;;         (logand 8 8) -> #10
;;;         (logand #b1011 #b1101) -> #11
;;; ＠
;;; common:logand                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:logand &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の論理積を求め、その結果を
;;; 10 進数で返す。引数が指定されなければ -1 を返す。
;;;
;;; <例>
;;;         (common:logand 10 10) -> 10
;;;         (common:logand #o10 #o10) -> 8
;;;         (common:logand #b1011 #b1101) -> 3
;;; ＠
;;; logandc1                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logandc1 integer1 integer2
;;; integer1 の値の補数と integer2 の値のビット毎の論理積を求め、その結果を
;;; 8 進数で返す。
;;; (boole boole-andc1 integer1 integer2) と同じ。
;;;
;;; <例>
;;;         (logandc1 #b1011 #b1101) -> #2
;;;         (logandc1 10 20) -> #24
;;;         (logandc1 20 10) -> #12
;;; ＠
;;; logandc2                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logandc2 integer1 integer2
;;; integer1 の値と integer2 の値の補数のビット毎の論理積を求め、その結果を
;;; 8 進数で返す
;;; (boole boole-andc2 integer1 integer2) と同じ。
;;;
;;; <例>
;;;         (logandc2 #b1101 #b1011) -> #2
;;;         (logandc2 20 10) -> #24
;;;         (logandc2 10 20) -> #12
;;; ＠
;;; logbitp                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logbitp index integer
;;; integer のビット位置 index のビットが 1 の場合は t を、0 の場合は
;;; nil を返す。 ビット位置は 0 から数える非負の整数。
;;; (logbitp x y) = (bit-test y x)
;;;
;;; <例>
;;;         (logbitp 2 6) -> t
;;;         (logbitp 0 6) -> ()
;;;         (logbitp 2 12) -> t
;;;         (logbitp 3 12) -> t
;;; ＠
;;; logcount                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : logcount integer
;;; integer の値が正の場合、その値を 7 ビットの 2 進数で表現した時にその中
;;; に含まれる 1 のビット数を数え、その結果を返す。負の場合は 0 のビット数
;;; を数え、その結果を返す。
;;;
;;; <例>
;;;         (logcount 13)  -> 3
;;;         (logcount -13) -> 2
;;;         (logcount 30)  -> 4
;;;         (logcount -30) -> 4
;;; ＠
;;; logeqv                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logeqv &rest integer1 integer2 .. integerN
;;; integer1 integer2 ... integerN の排他的否定論理和を求め、その結果を
;;; 8 進数で返す。
;;; (lognot (logxor integer1 integer2 ...  )) と同じ。
;;;
;;; <例>
;;;         (logeqv 2 2) -> #77777777
;;;         (logeqv 2 0) -> #77777775
;;; ＠
;;; logic                                  クラス
;;;
;;; <説明>
;;;   インスタンスは _x, _y などのような論理変数。
;;; ＠
;;; logical-names                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : logical-names &opt process
;;; process (既定値はカレントプロセス) で使われるパス名にロジカルなパス名と
;;; 実際のパス名をペアとする連想リストを返す。
;;;
;;; <例>
;;;         (logical-names) -> (("Co" ("n" "bs" . "ntec")
;;;         			  ("sys" "bs" . "sys")
;;;         			  ("z" "bs" . "zuk")))
;;; ＠
;;; logicp                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : logicp object
;;; object が論理変数なら、評価値を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (logicp '_x) -> _x
;;;         (logicp '_) -> _
;;;         (logicp 'x) -> nil
;;; ＠
;;; login                                  クラス
;;;
;;; <説明>
;;;   インスタンスはログインプロセス。ログインプロセスは、ユーザが ELIS に
;;; ログインした時に生成され、端末を通して送られるユーザの要求を扱う。
;;; ＠
;;; logior                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : logior integer1 &rest integer2 ... integerN
;;; integer1 integer2 ... integerN の論理和を求め、その結果  (引数が指定
;;; されない場合 0) を 8 進数で返す。
;;;
;;; <例>
;;;         (logior #10 #2 #230) -> #232
;;;         (logior #b1011 #b1001) -> #13
;;;         (logior 2 3) -> #3
;;; ＠
;;; common:logior                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:logior &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN の論理和を求め、その結果 (引数が指定され
;;; ない場合 0 ) を 10 進数で返す。
;;;
;;; <例>
;;;         (common:logior 2 2) -> 2
;;;         (common:logior 2 3) -> 3
;;;         (common:logior #b1011 #b1001) -> 11
;;; ＠
;;; logmask                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : logmask integer1 integer2
;;; integer2 の補数と integer1 とのビット毎の論理積を求め、その結果を返す。
;;; (logand integer1 (lognot integer2)) と同じ。
;;;
;;; <例>
;;;         (logmask #12 #14) -> #2
;;; ＠
;;; lognand                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : lognand integer1 integer2
;;; integer1 と integer2 のビット毎の論理積を求め、その結果を否定演算した後、
;;; 8 進数で返す。
;;; (lognot (logand integer1 integer2) と同じ。
;;;
;;; <例>
;;;         (lognand 2 1) -> #7777777
;;;         (lognand 2 2) -> #7777775
;;; ＠
;;; lognand*                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : lognand* &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の論理積を求め、その結果を否定
;;; 演算した後、8 進数で返す。
;;; ＠
;;; lognor                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : lognor integer1 integer2
;;; integer1 と integer2 のビット毎の論理和を求め、その結果を否定演算した後、
;;; 8 進数で返す。
;;; (lognot (logior integer1 integer2)) と同じ。
;;;
;;; <例>
;;;         (lognor 2 1) -> #77777774
;;;         (lognor 2 2) -> #77777775
;;; ＠
;;; lognor*                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : lognor* &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の論理和を求め、その結果を否定
;;; 演算した後、8 進数で返す。
;;;
;;; <例>
;;;         (lognor* 1 2) -> #77777774
;;;         (lognor* 1 0) -> #77777776
;;; ＠
;;; lognot                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : lognot integer
;;; integer の補数を求め、得られた結果を 8 進数で返す。
;;;
;;; <例>
;;;         (lognot #777) -> #77777000
;;;         (lognot 12) -> #77777763
;;; ＠
;;; common:lognot      未インプリメント    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:lognot integer
;;; integer の補数を求め、得られた結果を 10 進数で返す。
;;;
;;; <例>
;;;         (common:lognot 1) -> -2
;;;         (common:lognot 0) -> -1
;;;         (common:lognot -2) -> 1
;;; ＠
;;; logorc1                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logorc1 integer1 integer2
;;; integer1 の補数と integer2 のビット毎の論理和を求め、その結果を
;;; 8 進数で返す
;;; (boole boole-orc1 integer1 integer2) と同じ。
;;;
;;; <例>
;;;         (logorc1 0 0) -> #7777777
;;;         (logorc1 0 1) -> #7777776
;;;         (logorc1 1 0) -> #7777776
;;;         (logorc1 1 1) -> #7777777
;;; ＠
;;; logorc2                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logorc2 integer1 integer2
;;; integer1 と integer2 の補数のビット毎の論理和を求め、その結果を 8 進数
;;; で返す。
;;; (boole boole-orc2 integer1 integer2) と同じ。
;;;
;;; <例>
;;;         (logor2 0 0) -> #77777777
;;;         (logor2 0 1) -> #77777776
;;;         (logor2 1 0) -> #77777777
;;;         (logor2 1 1) -> #77777777
;;; ＠
;;; logout                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : logout &opt login
;;; ログアウトは次の順番で行われる。最初に、全ユーザのディレクトリが
;;; フィクスされる。2 番目に、ログインで生成された全プロセスがキルされる。
;;; 3 番目に、当該プロセスにより占められている全 I/O と semaphore が
;;; 開放される。4 番目に、対応する端末がキルされる。 5 番目に、端末の状態が
;;; リセットされる。
;;;
;;; <例>
;;;         (logout) ->
;;;         Sweeping your symbols from system symbol's plist.
;;;         Fixing all directories ....
;;;         Killing all subprocess.
;;;         BYE! (USER'S NAME)
;;; ＠
;;; sys:logout-him                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : sys:logout-him terno
;;; ターミナル番号 terno の端末を使うログインを強制的にログアウトさせる。
;;;
;;; <例>
;;;         sys:logout-him 2 -> t   (2 番のターミナルが使われていたとき)
;;;         sys:logout-him 2 -> not-loged-in
;;;          		(2 番のターミナルが使われていなかったとき)
;;; ＠
;;; logtest                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : logtest integer1 integer2
;;; integer1 の 1 の値を持ったビットに対応する integer2 のビットが全て
;;; 1 ならば t を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (logtest 2 8) -> nil
;;;         (logtest 3 13) -> t
;;;         (logtest 5 23) -> t
;;;         (logtest 2 23) -> t
;;; ＠
;;; logxor                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : logxor integer1 &rest integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の排他的論理和を求め、その結果
;;; を 8 進数で返す。
;;;
;;; <例>
;;;         (logxor #1234 #567) -> #1753
;;;         (!x '#1234) -> #1234
;;;         (!y '#567) -> #567
;;;         (logxor x y) -> #1753
;;; ＠
;;; common:logxor                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:logxor &rest integer1 integer2 ... integerN
;;; integer1 integer2 ... integerN のビット毎の排他的論理和を求め、その結果
;;; を 10 進数で返す。
;;;
;;; <例>
;;;         (common:logior #b1100 #b1010) -> 6
;;;         (common:logior 12 10) -> 6
;;; ＠
;;; long-float-epsilon                     定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の正の long-float が格納されている
;;; システム定数であり、本システムでは、1.11022302462516f-16。
;;; ＠
;;; long-float-negative-epsilon            定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の負の long-float が格納されている
;;; システム定数であり、本システムでは、5.55111512312579f-17。
;;; ＠
;;; long-site-name                         関数[#!expr]
;;;
;;; <説明>
;;;   ハードウェアの物理的な位置を識別する文字列を長い名前で返す。
;;; ELIS システムでは、"NTT Integrated comm. Lab takuso NUE group" を返す。
;;;
;;; <例>
;;;         "NTT Integrated comm. Lab takuso NUE group"
;;;         "MIT Artificial Intelligence Laboratory"
;;;         "Massachusetts Institute of Technology Artificial
;;;         	Intelligence Laboratory"
;;;         "Carnegie-Mellon University Computer Science Department"
;;; ＠


(defmacro tao:loop (&body body)
  "loop                                   関数[#!subr]

<説明>
  形式 : loop [exit-id] [(&aux var ...)]
                        [(:init init-form ...)]
        		[(:until pred exit-form1 exit-form2 ...)]
 			[(:while pred exit-form1 exit-form2 ...)]
 			form1 form2 ... formN
TAO の基本的な繰り返しの機能。
&aux で loop の中だけで有効な補助変数を宣言する。
:init があれば init-form を最初に一度だけ評価する。
:until の述語が成立するか、または :while の述語が成立しなくなるまで
form1 form2 ... を順に評価する。そして、:until 文が成立、または :while
文が成立しなくなった時、対応する exit-form1 ... を順に評価して loop
から抜け、最後の exit-form の値を返す。
:until や :while は何回でも使えるし省略可能。 exit-form は省略可能。

<例>
        (defun f (n)
          (loop (&aux c result)
                (:init (setq c 0 result 1))
                (:until (= c n) result)
                (setq result (* (incf c) result))))

        ;; TAO
        (de f (n)
            (loop (&aux c result)
                  (:init (!c 0) (!result 1))
                  (:until (c = n) result)
                  (!result ((inc c) * result)) ))
        n の階乗を計算する。"
  (let ((exit-id (and (atom (car body)) (pop body)))
        (loop-tag (gensym))
        (exit-result (gensym))
        aux init newbody)
    (dolist (l body)
      (case (car l)
        (&aux   (setq aux (cdr l)))
        (:init  (setq init (cdr l)))
        (otherwise (push l newbody))))
    `(macrolet ((tao:cycle (&optional exit-id)
                  ;;--- TODO exit-id
                  (declare (cl:ignore exit-id))
                  `(go ,',loop-tag))
                (tao:exit (&optional ,exit-result)
                  `(return-from ,',exit-id ,,exit-result)))
       (block ,exit-id
         (tao:let (,@aux)
           (tagbody
            (progn ,@init)
            ,loop-tag
            ,@(mapcar
               (lambda (x)
                 (cond ((eq :while (car x))
                        `(or ,(cadr x) (tao:exit (progn ,@(cddr x)))))
                       ((eq :until (car x))
                        `(and ,(cadr x) (tao:exit (progn ,@(cddr x)))))
                       ('T x)))
               (nreverse newbody))
            (go ,loop-tag)))))))


#|(defun f (n)
  (loop (&aux c result)
        (:init (setq c 0 result 1))
        (:until (= c n) result)
        (setq result (* (incf c) result))))|#

;; TAO
#|(defun f (n)
  (loop (&aux c result)
        (:init (!c 0) (!result 1))
        (:until (= c n) result)
        (!result (* (incf c) result)) ))|#



#|

 (loop (exit-loop 3))

 (loop abc (&aux x y)
     (:init (setq x 0))
     (setq x (1+ x))
     (setq y (+ (expt x 3) (expt x 2) x 1))
     (cond ((<= y 50) (print y))
	    (t (exit-loop 'end abc))))
     -> 4
   15
   40
   end

 (let ((n 100))
  (loop (&aux c result)
     (:init (setq c 0) (setq result 1))
     (:init (setq c 0) (setq result 1))
     (:until (= c n) result)
     (setq result (* (incf c) result)) ))

 (loop (&aux l)
   (:init (setq l '(foo bar baz)))
   (:until (endp l))
   (print l)
   (setq l (cdr l)))

 (let ((l '(foo bar baz)))
  (loop
     (:until (endp l))
     (print l)
     (setq l (cdr l))))
||#

;;; ＠
;;; common:loop                            関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:loop &rest form1 form2 ... formN
;;; nil ブロックを設定し、form1 form2 ... formN  を順に繰り返し実行する。
;;; 普通 throw や return-from による非局所的な飛び出しにより繰り返しを終了
;;; する。
;;;
;;; <例>
;;;         (!x '(aho usunoro manuke wao))→(aho usunoro manuke wao)
;;;         (common:loop (print x) (cdr! x) (if (null x) (return nil))) ->
;;;         	(aho usunoro manuke wao)
;;;         	(aho usunoro manuke)
;;;         	(aho usunoro)
;;;         	(aho) nil
;;; ＠
;;; lower-case-p                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : lower-case-p char
;;; 文字 char が小文字であれば t を返し、それ以外の場合 nil を返す。
;;; standard-char-p 関数で定義された標準文字の場合は、a から z までが小文字。
;;;
;;; <例>
;;;         (lower-case-p #¥a) -> "a"
;;;         (lower-case-p #¥A) -> nil
;;; ＠
;;; lsh                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : lsh shortnum1 shortnum2
;;; shortnum2 の評価結果が正または 0 なら、その数だけ shortnum1 の内容を
;;; 左へビットシフトし、負ならその数だけ shortnum1 の内容を右へビットシフト
;;; し、その結果を返す (shortnum1 の内容は破壊されない)。
;;; 左右いずれの方向でもシフトされて空白になるビット部分には 0 がうめら
;;; れる。 ビットのサインは変更しない。
;;;
;;; <例>
;;;         (lsh 4 1) -> #10
;;;         (lsh 14 -2) -> #3
;;;         (lsh #123 3) -> #1230
;;;         (lsh #123 -3) -> #12
;;;         (lsh #123 0) -> #123
;;;         (!x 1234) -> 1234
;;;         (lsh x -3) -> #232
;;;         x -> 1234
;;; ＠
;;; sys:lt                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sys:lt number1 number2
;;; number1 の評価値が、number2 (31 ビットのデータ) の評価値より小さければ
;;; t 、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (de bar (x y) (sys:lt x y)) -> bar
;;;         (bar a b) -> nil
;;;         (bar 1 2) -> t
;;; ＠
;;;
