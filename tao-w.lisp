;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)


(in-package #:tao-internal)


(define
 "wait-until-timeout"
 (subr nil)
 :documentation
 "形式 : wait-until-timeout tick
tick 刻み (1 刻み 20 ミリ秒) 過ぎるまで待つ。"
 :example
 "(wait-until-timeout 100) -> nil")


(define
 "warn"
 #'warn
 :documentation
 "形式 : warn format-string &rest args
エラーメッセージを出力し、nil を返す。普通はデバッガにはいかない。
(*break-on-warnings* によって制御される)。"
 :example
 "")


(define
 "when"
 (cl-macro when)
 :documentation
 "形式 : when pred &rest body
pred を評価し、nil の場合は nil を返し、nil 以外の場合は、body を左から
右に逐次評価し、最後の評価結果を返す。
(when p a b c) = (and p (progn a b c))
               = (cond (p a b c))
               = (if p (progn a b c) nil)
               = (unless (not p) a b c)"
 :example
 "")


#+message
(define
 "which-operations"
 (message nil)
 :documentation
 "メッセージとして使われた which-operations は、関数として使われた
which-operations と同じ。"
 :example
 "")


(define
 "which-operations"
 (expr nil)
 :documentation
 "形式 : which-operations  object
object に送ることのできるメッセージのリストを返す。"
 :example
 "(defclass a () ((aa 1)) () :gettable) -> a
        (defclass b () ((bb 1)) (a) :gettable) -> b
        (defmethod (a mult) () (!!* !aa 2)) -> mult
        (defmethod (b mult) () (!!* !bb 3)) -> mult
        (which-operations a) -> 
        	(belongs-to /= = \\. \\.. bas:send-if-handles 
        	 bas:eval-inside-yourself describe-operations 
        	 which-operations operation-handle-p get-handler-for 
        	 describe)
        [1 which-operations] -> 
        	(mod ** / * - + > >= <= < belong-to /= = \\. \\..
        	 bas:send-if-handles bas:eval-inside-yourself
        	 describe-operations which-operations
        	 operation-handlep-p get-handle-for describe)")


(define
 "who-spy"
 (expr nil)
 :documentation
 "形式 : who-spy &opt terno
ターミナル terno をスパイしているターミナル番号をリストにして返す。
terno の既定値はログインしているターミナル。"
 :example
 "")


(define
 "with-input-from-string"
 (cl-macro with-input-from-string)
 :documentation
 "形式 : with-input-from-string pair-list &rest form1 form2 ... formN
pair-list は、(var string) という形式。pair-list の var が、string の
値から連続するキャラクタを与える文字入力ストリームに束縛されて、form1
form2 ... formN が実行され、formN の結果を返す。このストリームは、自動
的に with-input-from-stream 形式の出口でクローズされる。この時、その
出口が正常か異常かは問われない。"
 :example
 "")


(define
  "with-open-file"
 (cl-macro with-open-file)
 :documentation
 "形式 : with-open-file sfo &rest form1 form2 ... formN
ファイルをオープンし、ファイルへのストリームを、変数とパス名のリスト
sfo にバインドし、form1 form2 ... formN を評価する。
制御が本体を離れた時、正常であっても、異常であっても、ファイルは自動的
に閉じられる。"
 :example
 "(with-open-file (ifile name :direction :input)
          (with-open-file (ofile (merge-pathname-defaults ifile nil 
        			\"out\")
                             :direction :output :if-exists :supersede)
        (transduce-file ifile ofile)))")


(define
 "with-open-stream"
 (cl-macro with-open-stream)
 :documentation
 "形式 : with-open-stream pair-list &rest form1 form2 ... formN
pair-list は (var stream) という形。pair-list の stream が評価され、
ストリームが生成される。pair-list の var はその値として生成された
ストリームに束縛され、form1 form2 ... formN が実行され、formN の評価
結果が、返される。このストリームは、自動的に with-open-stream 形式の
出口でクローズされ、その出口が正常か異常かは問われない。"
 :example
 "")


(define
 "with-output-to-string"
 (cl-macro with-output-to-string)
 :documentation
 "形式 : with-output-to-string vs &rest form1 form2 ... formN
vs を文字出力ストリームに束縛して、form1 form2 ... formN が実行される。
そのストリームへのすべての出力はストリングにセーブされる。この
ストリームは、自動的に with-output-to-stream 形式の出口でクローズされる。
この時、その出口が正常か異常かは問われない。"
 :example
 "")


(define
 "within-class"
 (macro (class &body body)
  `(macrolet ((tao:assert ((pred &rest args) &rest clauses)
                `(tao:deflogic-method (,',class ,pred) (,@args)
                   ,@clauses))
              (tao:retract ((pred &rest args))
                `(tao:undeflogic-method (,',class ,pred) (,@args))))
     ,@body))
 :documentation
 "形式 : within-class 'class-name forms
クラス class-name 内の論理メソッドを宣言、消去する。
forms は (assert ...) または (retract ...) から構成される。"
 :example
 "(defclass aclass () ()) -> aclass
        (within-class aclass
        		(assert (amethod _x _y) (cons _x _y))
        		(assert (amethod _x _y _z) [ _x + _y + _z])
        		(assert (amethod (_p . _)) _p))
        -> amethod
        これは以下のような 3 つの式を実行したのと同じ。
        (deflogic-method (aclass amethod) (_x _y) (cons _x _y)) 
        	-> amethod
        (deflogic-method (aclass amethod) (_x _y _z) [_x + _y + _z]) 
        	-> amethod
        (deflogic-method (aclass amethod) (_p . _) _p) -> amethod
        deflogic-method の例を参照。")


(define
 "within-package"
 (macro nil)
 :documentation
 "形式 : within-package package form1 form2 ...
package 内で、順にform1 form2 ... を評価し、最後の評価値を返す。"
 :example
 "(!gg (make-package \"gonta\")), then
        (within-package gg (car '(a b c))) -> a")


(define
 "without-interrupts"
 (subr nil)
 :documentation
 "形式 : without-interrupts &rest form1 form2 ... formN
中断無しに、form1 form2 ... formN を実行する。control-C や control-\\ 等
のような中断は無視される。"
 :example
 "")


(define
 "wrap"
 (expr nil)
 :documentation
 "形式 : wrap &opt terno
ターミナル terno を wrap モードにする。terno の既定値は wrap が入力され
たターミナル。wrap モードでは、画面を表示した後、スクロールを行わず
その代わりホームポジション (画面の最上の左端) から表示を行う。"
 :example
 "")


(define
 "write"
 (subr (object &optional (stream *standard-output*) (crlf T))
       (cl:write object :stream stream)
       (when crlf (cl:write-char #\Newline stream)))
 :documentation
 "形式 : write object &opt stream crlf
stream に object 及び改行文字をプリントした後、object を返す。stream が
省略されると *standard-output* の値が使われる。crlf の指定がないと、
改行を行なう。"
 :example
 "(write 'abc) -> abc
                        abc
        (write 'abc *standard-output* 'xyz) -> abcabc
        (write 'abc *standard-output* nil) -> abcabc")


(define
 "common:write"
 #'cl:write
 :documentation
 "形式 : common:write object &key :stream :escape :radix :base :circle
        	 :pretty :level :length :case :gensym :array
object を、指定されたモードで出力し、object の値を返す。
キーワード引数を以下に示す。
:stream  (出力用のストリーム)    既定値は変数 *standard-output* の値
:escape  (再入力可能か)              〃       *print-escape*     〃
:radix   (基数を出力するか)          〃       *print-radix*      〃
:base    (出力用の基数)              〃       *print-base*       〃
:circle  (循環リストを考慮するか)    〃       *print-circle*     〃
:pretty  (プリティプリントするか)    〃       *print-pretty*     〃
:level   (深さの制限)                〃       *print-level*      〃
:length  (長さの制限)                〃       *print-length*     〃
:gensym  (#: を出力するか)           〃       *print-gensym*     〃
:array   (配列を出力するか)          〃       *print-array*      〃"
 :example
 "(common:write \"asdf\") -> \"asdf\"
        			 \"asdf\"")


(define
 "write-byte"
 #'write-byte
 :documentation
 "形式 : write-byte integer stream
1 バイトの integer を、stream に書き、integer を返す。"
 :example
 "(write-byte 97 *standard-output*) -> a97")


(define
 "write-char"
 #'write-char
 :documentation
 "形式 : write-char char &opt stream
char を、stream へ出力し、その文字を返す。stream が省略されると、
*standard-output* の値が使われる。"
 :example
 "(write-char \"a\") -> a\"a\"")


(define
 "write-line"
 #'write-line
 :documentation
 "形式 : write-line string &opt stream &key :start :end
string の :start から :end までの副文字列を、stream に出力し、その後
改行を出力する。string を返す。
write-string 参照。"
 :example
 "(write-line \"abc def ghi\") -> abc def ghi
        			      \"abc def ghi\"
        (write-line \"abc def ghi\" *standard-output* :start 2 :end 7)
        			   -> c def
        			      \"abc def ghi\"")


(define
 "write-string"
 #'write-string
 :documentation
 "形式 : write-string string &opt stream &key :start :end
string の :start から :end までの副文字列を、stream に出力し、string 
を返す。write-line 参照。"
 :example
 "(write-line \"abc def ghi\") -> abc def ghi\"abc def ghi\"
        (write-line \"abc def ghi\" *standard-output* :start 2 :end 7)
        			   -> c def\"abc def ghi\"")


(define
 "write-string-justify"
 (expr nil)
 :documentation
 "形式 : write-string-justify string maxn &opt stream
string を右詰めに出力する。つまり、string の最後の文字が maxn 番目め
のカラムに出力されるように string を出力する。まず maxn カラムからな
る 1 行をとり、次に行の右端に位置するように string を出力する。行の
左端部分にはブランクが埋められる。string の長さが maxn より大きければ、
string は第 1 カラムから maxn カラムを無視して出力する。またブランクは
出力されない。string を返す。"
 :example
 "(write-string-justify \"abcde\" 10) ->      abcde\"abcde\"
        				(前ブランク5)
        (write-string-justify \"abcdefgh\" 10) ->    abcdefgh\"abcdefgh\"
        				(前ブランク3)
        (write-string-justify \"abcdefghijklm\" 10) ->
        				 abcdefghijklm\"abcdefghijklm\"")


(define
 "write-string-with-blank"
 (expr nil)
 :documentation
 "形式 : write-string-with-blank string maxn &opt stream
string を左詰めに出力する。つまり、まず maxn カラムからなる 1 行をとり、
次に string を出力し、t を返す。string の最後の文字が maxn より小さい
範囲で出力されると、残りにブランクが埋めらる。string の長さが maxn より
大きいと string は、第 1 カラムより maxn を無視して出力される。ブランク
は出力されない。"
 :example
 "(write-string-with-blank \"abcde\" 10) -> abcde     t
        (write-string-with-blank \"abcdefgh\" 10) ->abcdefgh  t
        (write-string-with-blank \"abcdefghijklm\" 10)) -> 
        	abcdefghijklmt")


(define
 "write-to-string"
 #'write-to-string
 :documentation
 "形式 : write-to-string object
object をダブルクォートではさんだ形が作られプリントされる。プリントの
表現は、字下げを除いては、vprint と同じ。"
 :example
 "(write-to-string (list 1 2 3 4)) -> \"(1 2 3 4)\"
        (write-to-string ()) -> \"()\"
        (write-to-string \"abcdefg\") -> \"\\\"abcdefg\\\"\"")


(define
 "common:write-to-string"
 #'write-to-string
 :documentation
 "形式 : common:write-to-string object &key :escape :radix :base
        	 :circle :pretty :level  :length :case :gensym :array
object をそのまま文字列にして返す。

&key
:escape  (再入力可能か)           既定値は変数 *print-escape* の値
:radix   (基数を出力するか)           〃       *print-radix*   〃
:base    (出力用の基数)               〃       *print-base*    〃
:circle  (循環リストを考慮するか)     〃       *print-circle*  〃
:pretty  (プリティプリントするか)     〃       *print-pretty*  〃
:level   (深さの制限)                 〃       *print-level*   〃
:length  (長さの制限)                 〃       *print-length*  〃
:gensym  (#: を出力するか)            〃       *print-gensym*  〃
:array   (配列を出力するか)           〃       *print-array*   〃"
 :example
 "(common:write-to-string (list 1 2 3 4)) -> \"(1 2 3 4)\"
        (common:write-to-string ()) -> \"()\"
        (common:write-to-string \"abcdefg\") -> \"\\\"abcdefg\\\"\"")


;;; *EOF*
