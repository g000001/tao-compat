(tao:tao)


(in-package #:tao-internal)


(define
 "backquotedp"
 (subr nil)
 :documentation
 "形式 : backquotedp object
object がバッククォートのついた式ならば、それを返し、そうでなければ 
nil を返す。"
 :example
 "(backquotedp '‘(a b)) -> ‘(a b)")


(define
 "backquotify"
 (expr nil)
 :documentation
 "形式 : backquotify object
object に、バッククォート文字をつけて返す。"
 :example
 "(backquotify 'a) -> `a")


(define
 "backtrace"
 (expr nil)
 :documentation
 "形式 : backtrace &opt flag
ネストされた関数呼びだしと、それに対応した変数束縛を見ることができる。
flag が nil でない値なら、この関数が呼ばれた時点から関数
backtrace-stopper が呼ばれた時点までの情報が示される。
flag の既定値は nil 。"
 :example
 "(de fact (x)
            (cond ((= x 0) (break))
                  (t (x * (fact (x - 1))))))
        (fact 3)   プロンプト break-> が生じる
        (backtrace) -> 
        (write catcher
         (bas:prompt \"break->\")
         (bas:$$u ((backtrace))
         loop catcher (bas:out {udo}39285echo-stream)
         (bas:in {udo}39271echo-stream)
         (bas:n {undef}0)
         break (x 0)
         fact (x 1)
         fact (x 2)
         fact (x 3)
         fact backtrace-stopper)
        カッコのない id は関数を表し、リストは car 部が変数で cdr 部が
        その値である束縛を表している。")


(define
 "backtrace-stopper"
 (subr nil)
 :documentation
 "形式 : backtrace-stopper &rest form1 form2 ... formN
関数 backtrace は、それが呼ばれた時点（普通は break が起こった時点）
から、関数 backtrace-stopper の始まりまで form1 form2 ... formN を
トレースする。"
 :example
 "(de foo (x)
            form1 form2 ...
            (backtrace-stopper form1 form2 ... formN)
            form=1 form=2 ... )")


'(define
 "sys:bas-package"
 (constant nil)
 :documentation
 "パッケージ \"bas\" へのポインタ。\"bas\" は、パッケージ \"univ\" の
サブパッケージ。\"bas\" は、ユーザパッケージの親パッケージ。
\"bas\" には、基本的な関数が登録されている。"
 :example
 "sys:bas-package -> {vector}32228(package . 12)")


(define
 "belongs-to"
 (subr (arg list)
   (block nil
     (mapl (lambda (x) (if (equal arg (car x))
                           (return x)))
           list)
     nil))
 :documentation
 "形式 : belongs-to arg list
list のトップレベルの要素を左から右に探し、arg と equal な要素を
見つけたら、それ以降の要素をリストとして返し、なければ nil を返す。
memqu mem 参照。"
 :example
 "(belongs-to 'a (c d a g b a)) -> (a g b a)
        (belongs-to 's (a b c) -> nil")


'(define
 "bex-stream"
 (class nil)
 :documentation
 "インスタンスが bex-stream であるクラス。
\"bex\" はバイナリ表現を表す。bex-stream は、入力または出力方向の両方に
動作する。データの圧縮が行われる。入出力のスピードは通常のストリーム
より非常に速く、データサイズも通常のストリームより小さく作成される。
bex-stream は、コンパイルコード等のデータとしてはよいストリームと言える。"
 :example
 "")


(define
 "bigfloatp"
 (not-implemented "bigfloatp" (&rest args)
   (declare (ignore args)))
 :documentation
 "形式 : bigfloatp number
number が (big floating-point number) なら評価値を返し、それ以外なら 
nil を返す。"
 :example
 "")


(define
 "bigp"
 (subr (number)
   (and (integerp number)
        (or (> number most-positive-fixnum)
            (< number most-negative-fixnum))
        number))
 :documentation
 "形式 : bigp number
number が (bignum) なら評価値を返し、それ以外なら nil を返す。
  -(2**23) < bignum < 2**23 (整数)"
 :example
 "(bigp -8388609) ->  -8388609
        (bigp 8388608) -> 8388608")


(define
 "bins"
 (subr (vector key)
   (do* ((lim (1- (length vector)))
         (i 0 (+ i 2)))
        ((= i lim) nil)
     (and (eq (aref vector i) key)
          (return (aref vector (1+ i))))))
 :documentation
 "形式 : bins vector key
vector の偶数番目の要素に、key と eq な値があるかどうかを捜し、あれば
その次の要素を返す。"
 :example
 "(bins v 123) -> \"a\"
        (bins v \"x\") -> \"s\"
        (bins v 'cat) -> nil")


(define
 "bit"
 #'bit
 :documentation
 "形式 : bit bit-array &rest data
ビット配列 bit-array の要素 data をアクセスし、その値を返す。
返される値は常に 0 か 1 。"
 :example
 "(!c (make-array 3 :element-type 'bit))
           -> {memblk}1343978(#!1b-memk . {dnil}3)
        (bit c 1) -> #0")


(define
 "bit-and"
 #'bit-and
 :documentation
 "形式 : bit-and bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビットごとの論理積 and を実行し、演算
結果を bit-array3 に破壊的に代入する。3 つの bit-array はすべて、同一の
大きさと次元を持っていなければならない。bit-array3 で nil を指定するか、
または省略すると、演算結果を含む新しい bit-array が生成される。
bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}1343978(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}1396762(#!1b-memk . {dnil}3)
        (bit-and c d) -> {memblk}1396768(#!1b-memk . {dnil}3)")


(define
 "bit-andc1"
 #'bit-andc1
 :documentation
 "形式 : bit-andc1 bit-array1 bit-array2 &opt bit-array3
bit-array1 の補数と bit-array2 に対して、ビットごとの論理積 and を実行
し、演算結果を bit-array3 に破壊的に代入する。3 つのbit-array はすべて、
同一の大きさと次元を持っていなければならない。bit-array3 で nil を指定
するか、または、省略すると、演算結果を含む新しい bit-array が生成される。
bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}1343978(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}1396762(#!1b-memk . {dnil}3)
        (bit-andc1 c d) -> {memblk}489255(#!1b-memk . {dnil}3)")


(define
 "bit-andc2"
 #'bit-andc2
 :documentation
 "形式 : bit-andc2 bit-array1 bit-array2 &opt bit-array3
bit-array1 と bit-array2 の補数に対して、ビットごとの論理積 and を実行
し、演算結果を bit-array3 に破壊的に代入する。3 つの bit-array はすべて、
同一の大きさと次元を持っていなければならない。bit-array3 で nil を指定
するか、または省略すると、演算結果を含む新しい bit-array が生成される。
bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}1343978(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}1396762(#!1b-memk . {dnil}3)
        (bit-andc2 c d) -> {memblk}1340731(#!1b-memk . {dnil}3)")


(define
 "bit-array-p"
 (expr nil)
 :documentation
 "形式 : bit-array-p array
array が bit-array なら、そのオブジェクトを返し、そうでなければ nil を
返す。"
 :example
 "(!a (make-array 3 :element-type 'bit)) -> 
        	{memblk}489538(#!1b-memk . {dnil}3)
        (!b (make-array '(5 5))) -> 
        	{applobj}1317623(#!array . 10)
        (bit-array-p a) -> {memblk}489538(#!1b-memk . {dnil}3)
        (bit-array-p b) -> nil")


(define
 "bit-eqv"
 #'bit-eqv
 :documentation
 "形式 : bit-eqv bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビットごとの「論理的な equivalence また
は排他的 nor」を実行し、演算結果を bit-array3 に破壊的に代入する。3 つ
の bit-array はすべて、同一の大きさと次元を持っていなければならない。
bit-array3 で nil を指定するか、または、省略すると、演算結果を含む
新しい bit-array が生成される。bit-array3 で t を指定すれば、演算結果は、
bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-eqv c d) -> {memblk}492841(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-eqv a c) -> エラー")


(define
 "bit-ior"
 #'bit-ior
 :documentation
 "形式 : bit-ior bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビットごとの「論理的 inclusive or」を
実行し、演算結果を bit-array3 に破壊的に代入する。3 つの bit-array は
すべて、同一の大きさと次元を持っていなければならない。bit-array3 で 
nil を指定するか、または省略すると、演算結果を含む新しい bit-array が
生成される。bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入
される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-ior c d) -> {memblk}488974(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-ior a c) -> エラー")


(define
 "bit-nand"
 #'bit-nand
 :documentation
 "形式 : bit-nand bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビットごとの「論理的 not-and」を実行し、
演算結果を bit-array3 に破壊的に代入する。3 つの bit-array はすべて、
同一の大きさと次元を持っていなければならない。bit-array3 で nil を指定
するか、または省略すると、演算結果を含む新しい bit-array が生成される。
bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-nand c d) -> {memblk}488977(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-nand a c) -> エラー")


(define
 "bit-nor"
 #'bit-nor
 :documentation
 "形式 : bit-nor bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビットごとの「論理的 not-or」を実行し、
演算結果を bit-array3 に破壊的に代入する。3 つの bit-arrayはすべて、
同一の大きさと次元を持っていなければならない。bit-array3 で nilを指定
するか、または省略すると、演算結果を含むような新しい bit-array が生成
される。bit-array3 で t を指定すれば、演算結果は bit-array1 に代入
される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-nor c d) -> {memblk}488980(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-nor a c) -> エラー")


(define
 "bit-not"
 #'bit-not
 :documentation
 "形式 : bit-not bit-array1 &opt bit-array2
bit-array1 に対して、ビットごとの論理否定 not を実行し、演算結果を
bit-array2 に破壊的に代入する。2 つの bit-array は、同一の大きさと次元
を持っていなければならない。bit-array2 で nil を指定するか、または省略
すると、演算結果を含む新しい bit-array が生成される。bit-array2 で t を
指定すれば、演算結果は bit-array に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-not c d) -> {memblk}489544(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-not a c) -> {memblk}492844(#!1b-memk . {dnil}5)")


(define
 "bit-off"
 (subr nil)
 :documentation
 "形式 : bit-off bit-array &rest position1 position2 ... positionN
bit-array (locative データ型か、64ビット以内で表現できる数値) のビット
位置 position1 position2 ... positionN のビットを 0 にクリアする。"
 :example
 "(bit-off #777 0) -> #776
        (bit-off #177777 15 1 0) -> #77774")


(define
 "bit-on"
 (subr nil)
 :documentation
 "形式 : bit-on bit-array &rest position1 position2 ... positionN
bit-array (locative データ型か、64 ビット以内で表現できる数値) のビット
位置 position1 position2 ... posionN のビットを 1 にセツトする。"
 :example
 "(bit-on #100 0) -> #101
        (bit-on #100 15 1 0) -> #100103")


(define
 "bit-orc1"
 #'bit-orc1
 :documentation
 "形式 : bit-orc1 bit-array1 bit-array2 &opt bit-array3
bit-array1 の補数と bit-array2 に対して、ビットごとの「論理和 or」を
実行し、演算結果を bit-array3 に破壊的に代入する。3 つの bit-array は
すべて、同一の大きさと次元を持っていなければならない。bit-array3 で
nil を指定するか、または省略すると、演算結果を含む新しい bit-array が
生成される。bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入
される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-orc1 c d) -> {memblk}488983(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-orc1 a c) -> エラー")


(define
 "bit-orc2"
 #'bit-orc2
 :documentation
 "形式 : bit-orc2 bit-array1 bit-array2 &opt bit-array3
bit-array1 と、bit-array2 の補数に対して、ビットごとの「論理和 or」を
実行し、演算結果を bit-array3 に破壊的に代入する。3 つの bit-array は
全て、同一の大きさと次元を持っていなければならない。bit-array3 で nil
を指定するか、または省略すると、演算結果を含む新しい bit-array が生成
される。bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入
される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-orc2 c d) -> {memblk}488989(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-orc2 a c) -> エラー")


(define
 "bit-test"
 (subr nil)
 :documentation
 "形式 : bit-test bit-array position
ビット配列 bit-array の各ビットを評価し、position で指定されたビットが
1 なら t を返し、0 ならば nil を返す。ビット位置は 0 から数える。"
 :example
 "(bit-test #100 6) -> t
        (bit-test #100 5) -> ()
        (bit-test #100 7) -> ()")


(define
 "common:bit-vector-p"
 (expr nil)
 :documentation
 "形式 : common:bit-vector-p arg
arg がビットベクタ であれば nil 以外の値、それ以外なら nil を返す。"
 :example
 "(!a (make-array 5))
             -> {vector}1208230(common:simple-general-vector . 5)
        (!b (make-array 5 :element-type t :initial-element 'bit)) 
             -> {vector}1208144(common:simple-general-vector . 5)
        (common:bit-vector-p a) -> nil
        (common:bit-vector-p b)
             -> {vector}1208144(common:simple-general-vector . 5)")


(define
 "bit-xor"
 #'bit-xor
 :documentation
 "形式 : bit-xor bit-array1 bit-array2 &opt bit-array3
bit-array1 bit-array2 に対して、ビット毎の排他的論理和を実行
し、演算結果を bit-array3 に破壊的に代入する。3 つの bit-array はすべて、
同一の大きさと次元を持っていなければならない。bit-array3 で nil を指定
するか、または省略すると、演算結果を含む新しい bit-array が生成される。
bit-array3 で t を指定すれば、演算結果は、bit-array1 に代入される。"
 :example
 "(!c (make-array 3 :element-type 'bit)) -> 
        	{memblk}492005(#!1b-memk . {dnil}3)
        (!d (make-array 3 :element-type 'bit)) -> 
        	{memblk}489544(#!1b-memk . {dnil}3)
        (bit-xor c d) -> {memblk}488992(#!1b-memk . {dnil}3)
        (!a (make-array 5 :element-type 'bit)) -> 
        	{memblk}492844(#!1b-memk . {dnil}5)
        (bit-xor a c) -> エラー")


(define
 "blank"
 (subr (&optional (stream t))
   (write-char #\space stream))
 :documentation
 "形式 : blank &opt stream
stream に空白文字を出力した後、文字 #\\space を返す。stream が省略された
場合、変数 *standard-input* の値 (通常コンソールターミナル) が指定され
たものと見なす。"
 :example
 "blank -> #\\space
        (!aa (open \"kkk.tao\" :direction :output)) -> 
        	{udo}1321209file-stream
        (prin1 \"test1\" aa) -> \"test1\"
        (blank aa) -> #\\space
        (prin1 \"test2\" aa) -> \"test2\"
        kkk.tao には、\"test1\" \"test2\" のようにブランクが 1 つ入る。")


(define
 "blanks"
 (subr (&optional (number 1) (stream t))
   (cond ((< 1 number)
          (dotimes (i number t)
            (write-char #\space stream)))
         ((minusp number) nil)
         ('T (write-char #\space stream))))
 :documentation
 "形式 : blanks &opt number stream
stream に number 個ブランクを出力し、t を返す。stream が省略された場合、
変数 *standard-input* の値 (通常コンソールターミナル) が指定されたもの
と見なす。number と stream が省略されると、blanks は、リターン値も含め
て関数 blank と全く等しくなる。number が負のときはブランクは出力しない。"
 :example
 "(blanks 3) ->    t    (3 ブランク出力)
        (!aa (open \"kkk.tao\" :direction :output))
        (prin1 \"test1\" aa) -> \"test1\"
        (blanks 5 aa) -> t
        (prin1 \"test2\" aa) -> \"test2\"
        kkk.tao には、\"test1\"     \"test2\" のようにブランクが 5 つ入る。")


(defmacro tao:block (tag &body body)
  "形式 : block name &rest form1 form2 .. formN
ブロック name を設定して、form1 form2 ... formN を逐次評価し、formN の
評価結果を返す。しかし評価中に、(return-from name val) というフォームと
出会うと評価を直ちに終了し val の評価値（省略時は nil）を返すが、
ブロック名の後にフォームが省略された時は nil を返す。name の既定値も
nil 。nil という名前のブロックに関しては、(return-from nil val) また
は単に (return val) というフォームと出会うと val の値を返す。ブロックは、
form1 form2 ... formN をスコープとする。

\(!x 1) -> 1
        (block there (print x)
        	(when (numberp x)
        	      (return-from there (1+ x)))
        	(print 'not-a-number)
        	(list x x)) -> 1 2
        (!item '((1 2) (3 4))) -> ((1 2) (3 4))
        (block namae
               ((cond ((consp (car items))
        	       (return-from namae (car items)))
        	      (t (cdr items))))
         	-> (1 2)"
  `(block ,tag ,@body))


(define
 "boole"
 #'boole
 :documentation
 "形式 : boole op integer1 integer2
演算子 op に従って、integer1 と integer2 をブール演算し、その結果を整数
形式で返す。演算子(定数)とリターン値は以下の通り。
boole-clr    : integer1 と integer2 を 0 クリア。
boole-set    : integer1 と integer2 の各ビットを全て 1 にする。
boole-1      : integer1 を返す。
boole-2      : integer2 を返す。
boole-c1     : integer1 の補数を返す。
boole-c2     : integer2 の補数を返す。
boole-and    : integer1 と integer2 のビット毎の論理積を返す。
boole-ior    : integer1 と integer2 のビット毎の包含的論理和
               (inclusive or) を返す。
boole-xor    : integer1 と integer2 のビット毎の排他的論理和を返す。
boole-eqv    : integer1 と integer2 のビット毎の等価結果
               (equivalence) を返す。
boole-nand   : integer1 と integer2 のビット毎の否定的論理積を返す。
boole-nor    : integer1 と integer2 のビット毎の否定的論理和を返す。
boole-andc1  : integer1 の補数と integer2 のビット毎の論理積を返す。
boole-andc2  : integer1 と integer2 の補数のビット毎の論理積を返す。
boole-orc1   : integer1 の補数と integer2 のビット毎の論理和を返す。
boole-ocr2   : integer1 と integer2 の補数のビット毎の論理積を返す。"
 :example
 "(boole boole-clr 1 1) -> 0
        (boole boole-set 0 0) -> -1
        (boole boole-1 1 0) -> 1
        (boole boole-2 1 0) -> 0
        (boole boole-c1 1 0) -> #77777776
        (boole boole-c2 1 0) -> #77777777
        (boole boole-and 1 1) -> #1
        (boole boole-ior1 1 0) -> #1
        (boole boole-xor 1 0) -> #1
        (boole boole-eqv 1 0) -> #77777776
        (boole boole-nand 1 0) -> #77777777
        (boole boole-nor 0 1) -> #77777776
        (boole boole-andc1 0 1) -> #0
        (boole boole-andc2 0 1) -> #1
        (boole boole-orc1 0 1) -> #77777776
        (boole boole-orc2 0 1) -> #77777777")


(define
 "both-case-p"
 #'both-case-p
 :documentation
 "形式 : both-case-p char
char が大文字又は小文字であり、対応する大文字又は小文字が存在すれば
char を返し、それ以外の場合 nil を返す。
standard-char-p 参照。"
 :example
 "(both-case-p \"a\") -> \"a\"
        (both-case-p \"1\") -> nil
        (both-case-p \"&\") -> nil")


(define
 "boundp"
 (subr (symbol)
   (if (boundp symbol)
       (symbol-value symbol)
       nil))
 :documentation
 "形式 : boundp var
var が束縛されていればその値を返し、それ以外は nil を返す。"
 :example
 "(de count-t () (!x (1+ x))) -> count-t
        (!x 0) -> 0
        (count-t) -> 1
        x = 1
        (boundp 'x) -> t
        (boundp 'y) -> nil
        (progv '(y) '((a b c)) ((cdr y)) -> (b c)
        (boundp 'y) -> nil")


(define
 "common:boundp"
 #'boundp
 :documentation
 "形式 : common:boundp var
スペシャル変数 var が値をもっているなら t、そうでなければ nil を返す。"
 :example
 "(common:boundp 'a) -> nil
        (!a nil) -> nil
        (common:boundp 'a) -> t
        (!b '(1 2 3)) -> (1 2 3)
        (common:boundp 'b) -> t")


(define
 "bra-cons"
 (expr nil)
 :documentation
 "形式 : bra-cons object1 object2
object1 と objectr2 を組み合わせたブラケットリストを作成し、返す。"
 :example
 "(bra-cons '(a b) '(c d)) -> [(a b) c d]
        (bra-cons 'a '(c d)) -> [a c d]")


(define
 "bra-list"
 (expr nil)
 :documentation
 "形式 : bra-list &rest object1 object2 ... objectN
object1 object2 ... objectN を組み合わせたブラケットリストを作成し、
返す。"
 :example
 "(bra-list 'a) -> [a]
        (bra-list '(a b) '(c d)) -> [(a b) (c d)]
        (bra-list '[a b] '[c d]) -> [[a b] [c d]]
        (bra-list '[a b] '(c d)) -> [[a b] (c d)]")


(define
 "bracketp"
 (subr nil)
 :documentation
 "形式 : bracketp object
object がブラケットリストなら、それを返し、そうでなければ nil を返す。"
 :example
 "(bracketp '[x + y]) -> [x + y]
        (bracketp (cdr '[x + y])) -> nil
        (ブラケットリストのcdrはブラケットではなくセルである。)")


(define
 "break"
 #'break
 :documentation
 "形式 : break &opt string stream1 stream2
プロンプト break- \"string\" > を出して、別の read-eval-write ループに
入る。string の既定値は、\"\" (null-string) 。ループから出るためには、
プロンプトの後に ok を入力する。入力ストリーム stream1 と出力ストリーム
stream2 を指定することにより、break での対話を行なう。各々の既定値は、
*standard-input* と *standard-output* 。"
 :example
 "read-eval-write loop by (break \"keypoint3\")
                break-keypoint3>(trace)
        	..
        	break-keypoint3>(!qwe 123)
        	..
        	break-keypoint3>ok
        	Tao>")


(define
 "broadcast"
 (expr nil)
 :documentation
 "形式 : broadcast &rest arg1 arg2 ... argN
端末 A がコンピュータ Z に接続されており、この関数が端末 A で呼ばれた
とする。arg1 arg2 ... argN は、コンピュータ Z に接続されたすべての端末
に出力される。"
 :example
 "")


(progn
  (deftype tao:broadcast-stream ()
    'cl:broadcast-stream)
  (setf (find-class 'tao:broadcast-stream)
        (find-class 'cl:broadcast-stream))
  (setf (documentation 'tao:broadcast-stream 'function) "<説明>
インスタンスが broadcast-stream であるクラス。
broadcast-stream は、メンバーが通常のストリームの集合体。
出力方向のみ動作し、broadcast-stream に送られたデータもメンバーである
すべてのストリームに送られる。
多くのストリームに同一データを送りたいときに便利。
"))


(define
 "butlast"
 #'butlast
 :documentation
 "形式 : butlast list &opt number
list の要素を、list の最後から、number 個削除したリストを返す。
number の既定値は 1 。list は破壊されない。"
 :example
 "x = (1 2 3 4 5) の場合
        (butlast x) -> (1 2 3 4)
        x -> (1 2 3 4 5)
        (butlast '(a b c d e f g) 3) -> (a b c d)")


(define
 "byte"
 #'byte
 :documentation
 "形式 : byte number1 number2
ビット数 number1 とビット位置 number2 を指定するバイト指定子を持つ
バイトを定義し、以後のバイト操作に必要なバイト指定子を返す。"
 :example
 "")


(define
 "byte-position"
 #'byte-position
 :documentation
 "形式 : byte-position byte
byte の位置を整数形式で返す。"
 :example
 "(byte-position (byte 1 3)) -> 3
        (byte-position (byte 5 2)) -> 2")


(define
 "byte-size"
 #'byte-size
 :documentation
 "形式 : byte-size byte
byte の大きさを整数形式で返す。"
 :example
 "(byte-size (byte 1 3)) -> 1
        (byte-size (byte 5 2)) -> 5")


;;; *EOF*
