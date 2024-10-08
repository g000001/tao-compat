(tao:common-lisp)


(in-package #:tao-internal)


(define
 "nambra-cons"
 (expr nil)
 :documentation
 "形式 : nambra-cons object1 object2
object1 と object2 で構成される名前付ブラケットリストを作成し、返す。"
 :example
 "(nambra-cons 'head (list 'body 'tail)) -> head[body tail]")


(define
 "nambra-list"
 (expr nil)
 :documentation
 "形式 : nambra-list &rest object1 object2 ... objectN
object1 object2 ... objectN で構成される名前付ブラケットを作成し、
返す。"
 :example
 "(nambra-list 'head 'body 'tail) -> head[body tail]")


(define
 "nambracketp"
 (subr nil)
 :documentation
 "形式 : nambracketp object
object が名前付きブラケットリストなら、それを返し、
そうでなければ nil を返す。"
 :example
 "(!a (nambra-cns 'head (list 'body 'tail))) -> head[body tail]
        (nambracketp a) -> head[body tail]")


(define
 "namcell-cons"
 (expr nil)
 :documentation
 "形式 : namcell-cons object1 object2
object1 と object2 で構成される名前付きセルを作成し、返す。"
 :example
 "(namcell-cons 'head (list 'body 'tail)) -> head(body tail)")


(define
 "namcell-list"
 (expr nil)
 :documentation
 "形式 : namcell-list &rest object1 object2 ... objectN
関数 list と同じだが、ナムセル型の識別子に用いられ、ナムセルリストが
作られる。ナムセルは名前つきセルのことで、name (element1 element2 ...)
のように書かれる。"
 :example
 "(namcell-list 'head 'body 'tail) -> head(body tail)")


(define
 "namcellp"
 (subr nil)
 :documentation
 "形式 : namcellp object
object がナムセルリストなら、それを返し、そうでなければ nil を返す。
ナムセルは名前つきセルのことで、name (element1 element2 ... ) のように
書かれる。"
 :example
 "(!a namcell-list 'this 'is 'a 'cat)) -> this(is a cat)
        (namcellp a) -> this(is a cat)")


(define
 "name-char"
 #'name-char
 :documentation
 "形式 : name-char name
name が、文字型オブジェクトの名前なら name を返し、
そうでなければ、nil を返す。
char-name 参照。"
 :example
 "(name-char \"Backspace\") -> #\\Backspace
        (name-char \"Space\") -> #\\Space
        (name-char \"a\") -> nil")


(define
 "named-for"
 (macro (exit-id var list &body body)
     (let ((exit-id/g exit-id))
       `(block ,exit-id/g
          (dolist (,var ,list nil)
            ,@body))))
 :documentation
 "形式 : named-for &rest exit-id var list form1 form2 ... formN
form1 form2 ... formN を順に var を使って実行する。
var は list の中の各要素に逐次束縛されたもの。
exit-id を持つ exit-for 関数により脱出できる。"
 :example
 "(named-for abc x (index 1 100)
        	   (!y (+ (x ** 3) (x ** 2) x 1))
        	   (cond ((y <= 50) (write y))
        		 (t (exit-for 'end abc))))
        	-> 4
        	   15
        	   40
        	   end")


(define
 "namep"
 (expr (object)
   (and (typep object '(or symbol string))
        object))
 :documentation
 "形式 : namep object
object がシンボルまたは文字列なら object を返し、それ以外なら
nil を返す。"
 :example
 "(namep 'gegege-no) -> gegege-no
        (namep \"kitarou\") -> \"kitarou\"
        (namep 123) -> nil")


(define
 "namestring"
 #'namestring
 :documentation
 "形式 : namestring pathname
pathname の full-pathname を返す。"
 :example
 "(namestring (merge-pathname \"ara.yo\"))
        	 -> \"Ti::bs:<korasa>ara.yo\"
        (namestring *default-pathname-defaults*)
        	 -> \"Ho::bs:<dire>foo.tao\"
        (namestring (make-pathname :host \"Ti\")) -> \"Ti::.\"")


(define
 "nand#"
 ;;(locative-operator nil)
 (subr nil)
 :documentation
 "形式 : loc1 nand# loc2
loc1 と loc2 のビット nand 操作を行う。"
 :example
 "")


(define
 "nbutlast"
 #'nbutlast
 :documentation
 "形式 : nbutlast list &opt number
list の終わりから number の個数 (既定値は 1) の要素を削除したリストを
返す (butlast の破壊版)。"
 :example
 "x = (1 2 3 4 5)  とすると
        (nbutlast x) -> (1 2 3 4)   そして
        x = (1 2 3 4)   さらに
        (nbutlast x 2) -> (1 2)  そして
        x = (1 2)")


(define
 "ncadblep"
 (subr nil)
 :documentation
 "形式 : ncadblep object
object が car 関数でも cdr 関数でもエラーにならないなら、nil を返し、
それ以外なら t を返す。
(ncadblep x) = (not (cadblep x))"
 :example
 "(ncadblep '(a b c)) -> nil
        (ncadblep \"abc\") -> t
        (ncadblep 'abc) -> t
        (ncadblep nil) -> t")


(define
 "nconc"
 #'nconc
 :documentation
 "形式 : nconc &rest list1 list2 ... listN
list1 list2 ... listN をこの順序で、しかもコピーしないで連結する。"
 :example
 "(nconc (list 1 2 3) (list 'a 'b 'c 'd)) -> (1 2 3 a b c d)
        x = (h e a d)  y = (a n d)  z = (t a i l)  なら
        (nconc x y z) -> (h e a d a n d t a i l)
        そして   x = (h e a d a n d t a i l)   y = (a n d t a i l)
        z = (t a i l)
        x = (h e a d)  y = nil  z = (t a i l)  なら
        (nconc x y z) -> (h e a d t a i l)
        そして  x = (h e a d t a i l)   y = nil    z = (t a i l)")


(define
 "nconc!"
 (macro (var list)
     `(tao:selfass nconc !,var ,list))
 :documentation
 "形式 : nconc! var list
(nconc! var list) = (!var (nconc list var)) 
ただし左辺では var は一度だけしか評価されない。"
 :example
 "a = '(1 2 3) のとき
        (nconc! a '(a b c)) -> (a b c 1 2 3)")


(define
 "ncons"
 (subr (x)
   (cons x nil))
 :documentation
 "形式 : ncons list
car 部が、list で、cdr 部が nil である新しいコンスを作り、それを返す。
(ncons x) = (cons x nil)"
 :example
 "x が (a b c)ならば
        (ncons x) -> ((a b c))
        (ncons nil) -> (nil)")


(define
 "neg"
 (subr (number)
   (- number))
 :documentation
 "形式 : neg number
number の負数を返す。"
 :example
 "(neg 100) -> -100
        (neg -100) -> 100")


(define
 "neq"
 (subr (object1 object2)
   (not (eq object1 object2)))
 :documentation
 "形式 : neq object1 object2
object1 と object2 が以下の条件を満足すれば、nil、それ以外なら t を返す。
    (1) 両方とも nil  
    (2) 等値の小整数 (shortnum)
    (3) 同一のシンボル
    (4) 同じコード化整数 (codnum)
    (5) 同じ文字 (a character string)
    (6) 等値の shortfloat (24 ビットで示された浮動小数点数)
2 つの引数のアドレスがメモリー上の同位置にあるときに、nil を返す。
(neq object1 object2) = (not (eq object1 object2))"
 :example
 "(neq nil nil) -> nil
        (neq -345678 -345678) -> nil
        (neq #12 #12) -> nil
        (neq 12345678 12345678) -> t
        (neq 'abc 'cba) -> t
        (neq \"a\" \"b\") -> t
        (neq \"a\" \"A\") -> t
        (neq '(1 2) '(1 2)) -> t
        (neq \"abc\" \"abc\") -> t")


(define
 "nidp"
 (subr (identifier)
   (typep identifier '(not symbol)))
 :documentation
 "形式 : nidp identifier
identifier が識別子なら、nil を返し、それ以外なら t を返す。
(nidp identifier) = (not (idp identifier))"
 :example
 "(nidp 'asdf) -> nil
        (nidp 3) -> t
        (nidp '_x) -> t
        (nidp #!expr) -> t")


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
(define
 "nil"
 (constant nil)
 :documentation
 "\"偽\" を表す。nil は、空のリスト () を意味するシンボル。
TAO では、nil は nil と呼ばれるデータを作る。"
 :example
 "")
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#


(define
 "nintersection"
 #'nintersection
 :documentation
 "形式 : nintersection list1 list2 &key :test :test-not :key
list1 と list2 の共通要素を抽出し、その結果をリスト形式で返す。
共通要素がない場合は、nil を返す。
list1 は破壊されるが、list2 は破壊されない。関数 intersection の破壊版。"
 :example
 "(nintersection '(a b c) '(b d)) -> (b)
        a = (1 2 3)  b = (4) の時
        (nintersection a b)  ->  nil")


(define
 "ninth"
 #'ninth
 :documentation
 "形式 : ninth list
list の 9 番目の要素を返す。(ninth list) = (nth 8 list)。"
 :example
 "(ninth '(0 1 2 3 4 5 6 7 8 9)) -> 8")


(define
 "nleft"
 (expr (number list1 &optional list2)
   (let ((len (cl:length list1)))
     (if (or (minusp number) (floatp number))
         (error "~S is illegal N for NUMBER." number)
         (and (tailp list2 list1)
              (nthcdr (- len number) list1)))))
 :documentation
 "形式 : nleft number list1 &opt list2
list2 が nil または省略された場合、list1 の最後から number 個の要素から
成る尾リストを返す。list2 の既定値は nil。
この尾リストに関数 cdr を number 回適用した結果が list2 と eq でなくて
はならない。list2 が list1 のどの尾リストとも eq でない場合、nil を返す。"
 :example
 "x = (1 2 3 4 5 6 7 8)  なら
        (nleft 3 x) -> (6 7 8)  であり、そして
        x = (1 2 3 4 5 6 7 8)
        (nleft 3 x (last x)) -> (5 6 7 8)
        (nleft 3 x (nleft 2 x)) -> (4 5 6 7 8)
        (nleft 3 x '(7 8)) -> nil")


(declaim (inline tao:nlistp))


(define
 "nlistp"
 (subr (list)
   (not (listp list)))
 :documentation
 "形式 : nlistp list
list がリストか、car 関数と cdr 関数が両方とも適用できるものであれば、
nil を返し、それ以外なら t を返す。
list は listp 関数に適用される前に評価される。
(nlistp list) = (not (listp list))"
 :example
 "(nlistp '(a b c d)) -> nil
        (nlistp '[a b c d]) -> nil
        (nlistp 'a) -> t
        (nlistp ''a) -> nil
        (nlistp '^(a b `c d)) -> nil
        (nlistp '(!x (fn y))) -> nil
        (nlistp (caddr '(list a b))) -> t
        (nlistp ()) -> nil
        (nlistp '(a . b)) -> nil")


(define
 "no-dumb"
 (expr nil)
 :documentation
 "形式 : no-dumb &opt terno
ターミナル番号 terno のターミナルが dumb モードからぬける。
terno の既定値は、no-dumb が入力されたターミナル。"
 :example
 "")


(define
 "no-local-echo"
 (expr nil)
 :documentation
 "形式 : no-local-echo &opt terno
ターミナル番号 terno のターミナルが local-echo モードからぬける。
terno の既定値は、no-local-echo が入力されたターミナル。"
 :example
 "")


(define
 "no-more"
 (expr nil)
 :documentation
 "形式 : no-more &opt terno
ターミナル番号 terno のターミナルが more モードからぬける。
terno の既定値は、no-more が入力されたターミナル。"
 :example
 "")


(define
 "no-screen"
 (expr nil)
 :documentation
 "形式 : no-screen &opt terno
ターミナル番号 terno のターミナルが screen モードからぬける。
terno の既定値は、no-screen が入力されたターミナル。"
 :example
 "")


(define
 "no-wrap"
 (expr nil)
 :documentation
 "形式 : no-wrap &opt terno
ターミナル番号 terno のターミナルが wrap モードからぬける。
terno の既定値は、no-wrap が入力されたターミナル。"
 :example
 "")


(define
 "nor#"
 ;;(locative-operator nil)
 (subr nil)
 :documentation
 "形式 : loc1 nor# loc2
loc1 と loc2 のビット nor 操作を行う。"
 :example
 "")


(define
 "normal-stream-p"
 (expr nil)
 :documentation
 "形式 : normal-stream-p stream
stream が normal-stream ならば stream を返し、そうでなければ nil を返す。"
 :example
 "(!aa (open \"asd.tao\")) -> {udo}71499file-stream
        (normal-stream-p aa) -> {udo}71499file-stream")


(define
 "not"
 #'not
 :documentation
 "形式 : not object
object の値を評価し、nil なら t を返し、それ以外なら nil を返す。
(not object) = (null object)"
 :example
 "(not (eq 'a 'b)) -> t    ( not (eq 'a 'b)) = (null (eq 'a 'b))
        (not (eq 'a 'a)) -> nil  ( not (eq 'a 'a)) = (null (eq 'a 'a))")


(define
 "notany"
 #'notany
 :documentation
 "形式 : notany pred seq1 &rest seq2 seq3 ... seqN
条件 pred をシーケンス seq1 seq2 ... seqN の各要素に順番に適用し、nil
以外の値になった場合には、nil を返し、最後まで nil であった場合には、t
を返す。"
 :example
 "(notany #'oddp '(1 2 3)) -> nil
        (notany #'evenp '(1 3 5)) -> t")


(define
 "notevery"
 #'notevery
 :documentation
 "形式 : notevery pred seq1 &rest seq2 seq3 ... seqN
条件 pred をシーケンス seq1 seq2 ... seqN の各要素に順番に適用し、nil
になった場合には、t を返し、最後まで nil にならなかった場合には nil を
返す。"
 :example
 "(notevery #'oddp '(1 3 5)) -> nil
        (notevery #'evenp '(1 2 3)) -> t")


(define
 "nreconc"
 #'nreconc
 :documentation
 "形式 : nreconc list1 list2
list の要素の並び順を逆転させたリストと、list2 をこの順序で、
しかもコピーしないで連結 (append) する。
(nreconc list1 list2) は (nconc (nreverse list1) list2) と等しいが、
より効率的。list1 は破壊される。revappend の破壊版。"
 :example
 "(nreconc (list 3 2 1) (list 4 5 6 7)) -> (1 2 3 4 5 6 7)")


(define
 "common:nreverse"
 #'nreverse
 :documentation
 "形式 : common:nreverse seq
シーケンス seq の要素を逆順に並びかえ、その結果を返す。
seq は破壊される。common:reverse の破壊版。
seq は、TAO ではリストでなければならないが、リストでなかったら nil 。
Common Lisp では、列でもよい。"
 :example
 "(!x '(a b c d)) -> (a b c d)
        (!y (common:nreverse x)) -> (d c b a)
        y -> (d c b a)
        x -> (a)")


(define
 "nreverse"
 (subr (list)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (typecase list
     (list (cl:nreverse list))
     (otherwise nil)))
 :documentation
 "形式 : nreverse list
list の要素の並び順を逆転させたリストを返す。
list は破壊される。reverse の破壊版。"
 :example
 "x = (1 2 3)  なら
        (nreverse x) -> (3 2 1)
        そして  x は '(1) となる")


(define
 "nset-difference"
 #'nset-difference
 :documentation
 "形式 : nset-difference list1 list2 &key :test :test-not :key
list1 には存在するが list2 には含まれていない要素を抽出し、
抽出した要素で構成するリストを返す。list1 は破壊される。
set-difference の破壊版。"
 :example
 "(nset-difference '(1 2 3)  '(2 3 4)  ->  (1)
        (nset-difference '(1 2 3)  '(1 2 3)  ->  nil")


(define
 "nset-exclusive-or"
 #'nset-exclusive-or
 :documentation
 "形式 : nset-exclusive-or list1 list2 &key :test :test-not :key
list1 か list2 かのいずれか一方に含まれている要素を抽出し、
抽出した要素で構成するリストを返す。
list1 list2 は共に破壊される。set-exclusive-or の破壊版。"
 :example
 "(nset-exclusive-or  '(1 2 3)  '(2 4))  ->  (1 3 4)
        (ndet-exclusive-or  '(1 2 3)  '(1 2 3)  ->  nil")


(define
 "nstring-capitalize"
 (subr (string &optional (start 0) end)
   (nstring-capitalize string :start start :end end))
 :documentation
 "形式 : nstring-capitalize string &opt start end
string の start と end の範囲内の文字列の文頭を大文字に変換し、その他の
文字を小文字に変換し、変換した結果を返す。
start end の既定値は、それぞれ 0 、string の長さ。
string-capitalize の破壊版。"
 :example
 "(nstring-capitalize \"abcde\" 0 2) -> \"Abcde\"
        (nstring-capitalize \"i loVE FLOWERS.\" 0 6)
             -> \"I Love FLOWERS.\"")


(define
 "common:nstring-capitalize"
 #'nstring-capitalize
 :documentation
 "形式>: ommon:nstring-capitalize string &key :start :end
string の :start と :end の範囲内の文字列の文頭を大文字に変換し、その他
の文字を小文字に変換し、変換した結果を返す。
:start :end の既定値は、それぞれ 0 、string の長さ。
common:string-capitalize の破壊版。"
 :example
 "(common:nstring-capitalize \"abcdef\") -> \"Abcdef\"
        (common:nstring-capitalize \"i loVE FLOWERS.\")
               -> \"I Love Flowers.\"")


(define
 "nstring-downcase"
 (subr (string &optional (start 0) end)
   (nstring-downcase string :start start :end end))
 :documentation
 "形式 : nstring-downcase string &opt start end
string の start と end の範囲内の大文字をすべて、対応する小文字に変換し、
その結果を返す。string は破壊される。string-downcase の破壊版。
start end の既定値は、それぞれ 0 、string の長さ。"
 :example
 "(nstring-downcase \"ABCDE\") -> \"abcde\"
        (nstring-downcase \"ABCDERD\" 2 4) -> \"ABcdERD\"
        (nstring-downcase \"AAAAAAA\" 0 2) -> \"aaAAAAA\"
        (nstring-downcase \"漢字\") -> \"漢字\"")


(define
 "common:nstring-downcase"
 #'nstring-downcase
 :documentation
 "形式 : common:nstring-downcase string &key :start :end
string の :start と :end の範囲内の大文字をすべて、対応する小文字に変換
し、その結果を返す。string は破壊される。
:start :end の既定値は、それぞれ 0 、string の長さ。
common:string-downcase の破壊版。"
 :example
 "(common:nstring-downcase \"ABC\") -> \"abc\"
        (common:nstring-downcase \"abc\") -> \"abc\"
        (common:nstring-downcase \"あいう\") -> \"あいう\"
        (common:nstring-downcase \"ABCDERGBD\" :start 1 :end 3)
               -> \"AbcDERGBD\"")


(define
 "nstring-fill"
 (subr (string character &optional (start 0) end)
   (fill string character :start start :end end))
 :documentation
 "形式 : nstring-fill string character &opt start end
string の start と end の範囲内のすべて文字を、character に変更し、
その結果を返す。
start end の既定値は、それぞれ 0 、string の長さ。
string-fill の破壊版。"
 :example
 "(nstring-fill \"abcdefghij\" \"x\" 2 4) -> \"abxxefghij\"
        (nstring-fill \"abcdefghij\" \"x\") -> \"xxxxxxxxxx\"
        (nstring-fill \"abcdefghij\" \"xyz\" 2 4) ->エラー
        (nstring-fill \"abcdefghij\" 'x 4 2) -> \"abcdefghij\"
        (nstring-fill \"abcdefghij\" 'x 12 14) -> \"abcdefghij\"")


(define
 "common:nstring-fill"
 #'fill
 :documentation
 "形式 : common:nstring-fill string character &key :start :end
string の :start と :end の範囲内のすべての文字を、character に変更し、
その結果を返す。
:start :end の既定値は、それぞれ 0 、string の長さ。
common:string-fill の破壊版。"
 :example
 "(common:nstring-fill \"abcdefg\" 'z :start 2 :end 5)
                          -> \"abzzzfg\"
        (common:nstring-fill \"abcdefg\" 'k ) -> \"kkkkkkk\"
        (common:nstring-fill \"あいうえお\" 'か :start 1 :end 3)
                          -> \"あかかえお\"")


(define
 "nstring-left-trim"
 (subr (string1 string2)
   (string-left-trim string1 string2))
 :documentation
 "形式 : nstring-left-trim string1 string2
string2 の文字列を左から右へ調べて行き、string1 の中のいずれかの文字
と等しい文字を捜す。等しい文字があれば、その文字を string2 から抜き取り、
さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
なった文字列を返す。string-left-trim の破壊版。"
 :example
 "(!p \"acbcaxyabcpqbcaba\") -> \"acbcaxyabcpqbcaba\"
        (nstring-left-trim \"abc\" p) -> \"xyabcpqbcaba\"
        p -> \"acbcaxyabcpqbcaba\"")


(define
 "common:nstring-left-trim"
 #'string-left-trim
 :documentation
 "形式 : common:nstring-left-trim char-bag string
string の文字列を左から右へ調べて行き、char-bag の中のいずれかの文字と
等しい文字を捜す。等しい文字があれば、その文字を string から抜き取り、
さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
なった文字列を返す。 char-bag はストリング、文字のリスト、アトムの
リストのどれかでよい。"
 :example
 "(!p \"acbcaxyabcpqbcaba\") -> \"acbcaxyabcpqbcaba\"
        (common:nstring-left-trim \"abc\" p) -> \"xyabcpqbcaba\"
        p -> \"acbcaxyabcpqbcaba\"
        (common:nstring-left-trim \"あか\" \"かさがあかい\")
                                        -> \"さがあかい\"")


(define
 "nstring-right-trim"
 (subr (string1 string2)
   (string-right-trim string1 string2))
 :documentation
 "形式 : nstring-right-trim string1 string2
string2 の文字列を右から左へ調べて行き、string1 の中のいずれかの文字
と等しい文字を捜す。等しい文字があれば、その文字を string2 から抜き取り、
さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
なった文字列を返す。string-right-trim の破壊版。"
 :example
 "(!p \"acbcaxyabcpqbcaba\") -> \"acbcaxyabcpqbcaba\"
        (nstring-right-trim \"abc\" p) -> \"acbcaxyabcpq\"
        p -> \"acbcaxyabcpqbcaba\"
        (!q \"abcabcabcxabcyabccabca\") -> \"abcabcabcxabcyabccabca\"
        (nstring-right-trim \"abc\" q) -> \"abcabcabcxabcy\"
        (!p \"さかのうえのさかなやさん\") -> \"さかのうえのさかなやさん\"
        (nstring-right-trim \"さか\" p) -> \"さかのうえのさかなやさん\"")


(define
 "common:nstring-right-trim"
 #'string-right-trim
 :documentation
 "形式 : common:nstring-right-trim char-bag string
string の文字列を右から左へ調べて行き、char-bag の中のいずれかの文字
と等しい文字を捜す。等しい文字があれば、その文字を string から抜き取り、
さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
なった文字列を返す。 char-bag はストリング、文字のリスト、アトムのリス
トでよい。common:string-right-trim の破壊版。"
 :example
 "(!x \"abcdefg\") -> \"abcdefg\"
        (common:nstring-right-trim \"fg\" x) -> \"abcde\"
        x -> \"abcdefg\"
        (common:nstring-right-trim \"ab\" x) -> \"abcdefg\"
        (!y \"さしすせそたち\") -> \"さしすせそたち\"
        (common:nstring-right-trim \"たち\" y) -> \"さしすせそ\"
        y -> \"さしすせそたち\"")


(define
 "nstring-trim"
 (subr (string1 string2)
   (string-trim string1 string2))
 :documentation
 "形式 : nstring-trim string1 string2
まず最初に、string2 の文字列を左から右へ調べて行き、string1 の中のい
ずれかの文字と等しい文字を捜す。等しい文字があれば、string-trim はそ
の文字を string2 から抜き取り、さらに検索を進める。なければそこで検索
を終える。次に string1 の中の文字と等しい文字が string2 にあるかどうか
を、今度は string2 の右から左へ調べて行く。この検索も最初と同じ方法で
進められる。最後にこの 2 つのステップで作られた新しい文字列を返す。
string-trim の破壊版。"
 :example
 "(!p \"acbcaxyabcpqbcaba\") -> \"acbcaxyabcpqbcaba\"
        (nstring-trim \"abc\" p) -> \"xyabcpq\"
        p -> \"acbcaxyabcpqbcaba\"
        (!r \"さくらさくさ\") -> \"さくらさくさ\"
        (nstring-trim \"さく\") -> \"ら\"
        q -> \"さくらさくさ\"")


(define
 "common:nstring-trim"
 #'string-trim
 :documentation
 "形式 : common:nstring-trim char-bag string
まず最初に、string の文字列を左から右へ調べて行き、char-bag の中のい
ずれかの文字と等しい文字を捜す。等しい文字があれば、その文字を string 
から抜き取り、さらに検索を進める。なければそこで検索を終える。次に 
char-bag の中の文字と等しい文字が string にあるかどうかを、今度は 
string の右から左へ調べて行く。この検索も最初と同じ方法で進められる。
最後にこの 2 つのステップで作られた新しい文字列を返す。 char-bag は
ストリング、文字のリスト、アトムのリストのどれか。
common:string-trim の破壊版。"
 :example
 "(common:nstring-trim  '(#\\Space #\\Tab #\\Newline)
                              \" garbanzo beans\")
                           -> \"garbanzo beans\"
        (common:nstring-trim '(\"a\") \"agarbanzo beans\")
                                 -> \"garbanzo beans\"
        (!x \"さくらさく\") -> \"さくらさく\"
        (common:nstring-trim \"さ\" x) -> \"くらさく\"
        x -> \"さくらさく\"")


(define
 "nstring-upcase"
 (subr (string &optional (start 0) end)
   (nstring-upcase string :start start :end end))
 :documentation
 "形式 : nstring-upcase string &opt start end
string の start と end の範囲内の小文字をすべて、大文字に変換し、
その結果を返す。string は破壊される。string-upcase の破壊版。
start end の既定値はそれぞれ 0 、string の長さ。"
 :example
 "(nstring-upcase \"Dr.Livingston,I presume?\")
                     -> \"DR.LIVINGSTON,I PRESUME?\"
        (nstring-upcase \"Shell we dance?\" 6 8)
                     -> \"Shell WE dance?\"
        (nstring-upcase \"ロケット\") -> \"ロケット\"")


(define
 "common:nstring-upcase"
 #'nstring-upcase
 :documentation
 "形式 : common:nstring-upcase string &key :start :end
string の :start と :end の範囲内の小文字をすべて大文字に変換し、
その結果を返す。string は破壊される。common:string-upcase の破壊版。
:start :end の既定値は、それぞれ 0 、string の長さ。"
 :example
 "(common:string-upcase \"Dr.Livingston,I presume?\") -> 
        n	\"DR.LIVINGSTON,I PRESUME?\"
        (common:string-upcase \"Dr.Livingston,I presume?\" :start 4)
                           -> \"Dr.LIVINGSTON,I PRESUME?\"
        (common:string-upcase \"ぁぃぅぇぉ\" :start 0 :end 3)
                           -> \"ぁぃぅぇぉ\"")


(define
 "nsublis"
 #'nsublis
 :documentation
 "形式 : nsublis a-list tree &key :test :test-not :key
連想リスト a-list のキーとなるオブジェクトを tree 中で検索し、
見つけたオブジェクトをそのキーと対応する値に変更して、返す
(tree は破壊される)。sublis の破壊版。"
 :example
 "(nsublis '(((+ x y) . (- x y)) ((+ x y) . (- x y)))
                 '(* (/ (+ x y) (+ x p) ( - x y)) :test #'equal)
     		-> (* (/ (- x y) (+ x p))(- x y))
        x = (Hanako is kirei)
        (nsublis '((kirei . woman) (small . big)) x)
        	-> (Hanako is woman)
        x -> (Hanako is woman)")


(define
 "nsublisq"
 (expr (a-list tree)
   (nsublis a-list tree :test #'eq))
 :documentation
 "形式 : nsublisq a-list tree
tree 内のシンボルを、連想リスト a-list の cdr 部と置き換えることに
よって新しい tree を作り、その結果を返す。
a-list のいずれかの要素の car 部 (キー) と eq なシンボルが tree に
あったら、そのシンボルを、その car部に対応する cdr 部で置き換える。
tree は、破壊される。sublisq の破壊版。"
 :example
 "")


(define
 "nsubst"
 #'nsubst
 :documentation
 "形式 : nsubst new old tree &key :test :test-not :key
tree を、値 new で書き換える (破壊的)。そして、値 old を満足する値に
戻し、その結果を返す。subst の破壊版。"
 :example
 "x = (shakespeare wrote (the hurricane))
        (nsubst 'tempest 'hurricane x)
        	-> (shakespeare wrote (the tempest))
        x -> (shakespeare wrote (the tempest))
        (nsubst '(a . cons) '(old . pair)
              '((old . spice) ((old . shoes) old . pair) (old . pair))
               :test #'equal)
               -> ((old . spice) ((old . shoes) a .cons) (a . cons))")


(define
 "nsubst-if"
 #'nsubst-if
 :documentation
 "形式 : nsubst-if new test tree &key :key
tree のうち、条件 test を満足するものを、値 new で書き換え (破壊的)、
その結果を返す。subst-if の破壊版。"
 :example
 "x = ((\"asd\" . asd) (qwe . \"qwe\")) 
        (nsubst-if 'a #'stringp x ) -> ((a . asd) (qwe .a))
        x -> ((a . asd) (qwe . a))")


(define
 "nsubst-if-not"
 #'cl:nsubst-if-not
 :documentation
 "形式 : nsubst-if-not new test tree &key :key
tree のうち、条件 test を満足しないものを、値 new で書き換え (破壊的)、
その結果を返す。subst-if-not の破壊版。"
 :example
 "")


(define
 "nsubstitute"
 #'nsubstitute
 :documentation
 "形式 : nsubstitute newitem olditem seq
        	&key :from-end :test :test-not :start :end :count :key
シーケンス seq の :start から :end までの範囲内で、
olditem を :count 個だけ、newitem に変更し、その結果を返す。
substitute の破壊版。"
 :example
 "(!x  '(1 2 4 1 3 4 5))
        (nsubstitute 9 4 x) -> (1 2 9 1 3 9 5)
        x -> (1 2 9 1 3 9 5)
        (nsubstitute 3 1 x :count 1) -> (3 2 9 1 3 9 5)
        x -> (3 2 9 1 3 9 5)
        (nsubstitute 2 3 x :count 1 :from-end t) -> (3 2 9 1 2 9 5)
        x -> (3 2 9 1 2 9 5)
        (nsubstitute 9 3 x :test #'>) -> (3 9 9 9 9 9 5)
        x -> (3 9 9 9 9 9 5)")


(define
 "nsubstitute-if"
 #'nsubstitute-if
 :documentation
 "形式 : nsubstitute-if newitem test seq
        	 &key :from-end :start :end :count :key
シーケンス seq の :start から :end までの範囲内で、条件 test を満足
する値を :count 個だけ、newitem で変更し、その結果を返す。
substitute-if の破壊版。"
 :example
 "(!x '(1 2 4 1 3 4 5))
        (nsubstitute-if 9 #'oddp x) -> (9 2 4 9 9 4 9)
        x -> (9 2 4 9 9 4 9)
        (nsubstitute-if 9 #'evenp x :count 1 :from-end t)
        	-> (9 2 4 9 9 9 9 )
        x -> (9 2 4 9 9 9 9)")


(define
 "nsubstitute-if-not"
 #'cl:nsubstitute-if-not
 :documentation
 "形式 : nsubstitute-if-not newitem test seq
        	 &key :from-end :start :end :count :key
シーケンス seq の :start から :end までの範囲内で、条件 test を満足
しない値を :count 個だけ、newitem に変更し、その結果を返す。
substitute-if-not の破壊版。"
 :example
 "(!x '(1 2 4 1 3 4 5))
        (nsubstitute-if-not 9 #'oddp x) -> (1 9 9 1 2 9 5)
        x -> (1 9 9 1 3 9 5)
        (nsubstitute-if-not 9 #'evenp x :count 1 :from-end t)
 		             ->	(1 9 9 1 3 9 9)
        x -> (1 9 9 1 3 9 5)")


(define
 "nsubstqu"
 (expr nil)
 :documentation
 "形式 : nsubstqu new old tree
リスト tree 内の、値 old と equal な要素を、値 new に置き換え
(破壊的)、その結果を返す。substqu の破壊版。"
 :example
 "x = (a b (foo . c) foo . foo)
        (nsubstqu 'bar 'foo x) -> (a b (bar . c) bar . bar)
        x -> (a b (bar . c) bar . bar)")


(define
 "nsubstring"
 (subr (string start &optional end)
   (multiple-value-bind (string len start end)
                        (string*-arg-check (string string) start end)
     (declare (ignore len))
     (if (> start end)
         ""
         (make-array (- end start)
                     :element-type (array-element-type string)
                     :displaced-to string
                     :displaced-index-offset start))))
 :documentation
 "形式 : nsubstring string start &opt end
string の start から end までの文字からなる部分ストリングを作り、
その結果を返す。end がない場合は、string の長さを用いる。
start と end は 0 から始まる数字で指定し、負の数の場合、逆インデックス
となる。例えば、\"abcd\" の中の a, b, c, d の位置を示す逆インデックスは
各々 -4, -3, -2, -1 。引数はストリングかアトムでなければならない。
nsubstring は substring と同じだが、リターン値は、string を使っており、
substring のようにコピーされたものではない。"
 :example
 "(nsubstring \"abcd\" 2 3) -> \"c\"
        (nsubstring \"abcd\" 2) -> \"cd\"
        (nsubstring 'abcdefg 3 0) -> \"\"
        (nsubstring \"あいうえお\" 1 3) -> \"いう\"
        (nsubstring \"さしすせそ\" 2 3) -> \"す\"")


(define
 "nsymbolp"
 (subr nil)
 :documentation
 "形式 : nsymbolp symbol
symbol がシンボルなら、nil を返し、それ以外なら t を返す。
(nsymbolp symbol) = (not (symbolp symbol))"
 :example
 "(nsymbolp 'x) -> nil
        (nsymbolp \"abc\") -> t")


(define
 "nth"
 #'nth
 :documentation
 "形式 : nth number list
list の、number が示す位置の要素の値を返す。
(要素の位置は 0 から数える)。number は非負の整数でなければならない。"
 :example
 "(nth 0 (list 1 2 3)) -> 1
        (nth 2 '(a b c)) -> c
        (nth 5 '(a b c)) -> nil
        (nth 2 '((a b) (c d) (e f) g)) -> (e f)")


(define
 "nthcdr"
 #'nthcdr
 :documentation
 "形式 : nthcdr number list
list に、number 回、関数 cdr を適用して得られた結果を返す。"
 :example
 "(nthcdr 0 (list 1 2 3)) -> (1 2 3)
        (nthcdr 4 '(a b c d e f g h i)) -> (e f g h i)
        (nthcdr 10 '(s h o r t)) -> nil")


(define
 "nthm"
 (subr nil)
 :documentation
 "形式 : nthm position memblk
メモリブロック memblk のオフセットの position であるような、n-ビット語
のビットパタンデータを 8 進数に変換して返す。"
 :example
 "(!aaa (get-memblk #!8b-memblk 20)) ->
                         {memblk}480569(#!8b-memblk . {dnil}20)
        (!bbb (locbit aaa 5)) ->
           {locbit}({memblk}480569(#!8b-memblk . {dnil}20) . {dnil}5)
        (!(deref bbb) 12) -> 12
        (nthm aaa 5) -> #14
        (!(nthm aaa 5) 8) -> 8
        (nthm aaa 5) -> #10")


(define
 "nthv"
 (subr nil)
 :documentation
 "形式 : nthv number vector
vector の number 番目の要素の値を返す。
(要素の位置は 0 から数える)。"
 :example
 "(!v (vcons 'vec 8)) -> {vector}70004(vec . 8)
        (70004 はメモリロケーション)、
        (nthv 0 v), (nthv 1 v), ... (nthv 7 v) は全て nil を返す。
        (!(nthv n v) newvalue) は、ベクタ v の n 番目の要素に 
        newvalue を代入する。
        (!(nthv 5 v) 14) -> 14")


(define
 "ntrim"
 (subr nil)
 :documentation
 "形式 : ntrim &rest var list form1 form2 ... formN
list の第 1 要素を変数 var の値として form1 form2 ... formN を順に評価
する。次に、list の第 2 要素を var の値として form1 form2 ... formN を
順に評価する。以下、list の最後の要素までこれを繰り返す。
そして、最後のフォーム formN の評価結果の値をすべて調べ、それが 
non-nil であった時の、var の値を並べてリストとして返す。
trim の破壊版。"
 :example
 "x= '(1 2 3 4 5)
        (ntrim a x (!b (+ a 1))) -> (1 2 3 4 5)  b -> 6")


(define
 "null"
 #'null
 :documentation
 "形式 : null object
object が nil ならば t を返し、それ以外なら nil を返す。"
 :example
 "(null ()) -> t
        (null nil) -> t
        (null t) -> nil
        (null 123) - nil")


(define
 "null-stream"
 (variable (make-two-way-stream (make-concatenated-stream)
                                (make-broadcast-stream)))
 :documentation
 "クラス fundamental-stream のインスタンス。
あらゆるデータは、null-stream へ出力できるが、出力されたデータに
アクセスはできない。
null-stream へ出力するとそのデータは保存されない。
null-stream から得られるデータは :eof。
(!*standard-output* null-stream) とすると、コンソールターミナル
(*standard-output*) には、入力文字しか現れれない。"
 :example
 "")


(define
 "null-string"
 (subr (string)
   (and (stringp string)
        (zerop (length string))
        string))
 :documentation
 "形式 : null-string string
string が空ストリング (\"\") なら、\"\" を返し、そうでなければ nil を返す。"
 :example
 "(null-string \"\") -> \"\"
        (null-string 123) -> nil
        (null-string \"abc\") -> nil
        (null-string \"あいう\") -> nil")


(define
 "numberp"
 #'cl:numberp
 :documentation
 "形式 : numberp object
object が、数値データタイプの 1 つなら、object を返し、それ以外なら
nil を返す。数値データタイプは、integer, ratio, real number, codnum  。"
 :example
 "(numberp 1) -> 1
        (numberp 1234567890) -> 1234567890
        (numberp 1.41) -> 1.41
        (numberp 22/7) -> 22/7
        (numberp #!array) -> #!array
        (numberp \"string\") -> nil")


(define
 "numerator"
 #'numerator
 :documentation
 "形式 : numerator number
有理数 number の分子を整数に変換して返す。"
 :example
 "(numerator -1/2) -> -1
        (numerator 8/2) -> 4
        (numerator 5/3) -> 5
        (numerator 10) -> 10")


(define
 "nunion"
 #'nunion
 :documentation
 "形式 : nunion list1 list2 &key :test :test-not :key
list1 と list2 の論理和をとり、その結果を返す。
2 つのリストに重複して含まれる要素があれば、重複したものの 1 つだけが
結果に含まれる。2 つのリストは共に破壊される。union の破壊版。"
 :example
 "(nunion  '(1 2 3)  '(4 5 1))  ->  (2 3 4 5 1)
        (nunion  '(1 2 3)  '( ))  ->  (1 2 3)")


(define
 "nxor#"
 ;;(locative-operator nil)
 (subr nil)
 :documentation
 "形式 : loc1 nxor# loc2
loc1 と loc2  のビット nxor 操作を行う。"
 :example
 "")


;;; *EOF*
