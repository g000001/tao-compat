(tao:common-lisp)
(in-package #:tao-internal)

(defun tao:sass (pred item a-list func)
  "sass     未インプリメント              関数[#!subr]

<説明>
  形式 : sass pred item a-list func
連想リスト a-list中に、第 1 要素が item と共に条件 pred を満足するペア
があれば、そのペアを返し、なければ func を評価し、その結果を返す。"
  (cond ((assoc item a-list :test pred))	;thenret
	('T (funcall func))))

(defun tao:sassq (item alist func)
  "sassq                                  関数[#!subr]

<説明>
  形式 : sassq item a-list func
連想リスト a-list中に、第 1 要素が item と eq なペアがあれば、そのペア
を返し、なければ func を評価し、その結果を返す。

<例>
        (!xx '((a . 1) (b . 2) (c . 3) ((d e) . 4)))
        (sassq 'a xx (lambda () (!z 5))) -> (a . 1)
        (sassq 'e xx (lambda () (!z 5))) -> 5
        (sassq '(d e) xx (lambda () (!z 5))) -> 5"
  (tao:sass #'eq item alist func))

(defun tao:sassql (item alist func)
  "sassql 未インプリメント                関数[#!subr]

<説明>
  形式 : sassql item a-list func
連想リスト a-list 中に、第 1 要素が item と eql なペアがあれば、その
ペアを返し、なければ func を評価し、その結果を返す。"
  (tao:sass #'eql item alist func))


(defun tao:sassqu (item alist func)
  "sassqu                                 関数[#!subr]

<説明>
  形式 : sassqu item a-list func
連想リスト a-list 中に、第 1 要素が item と equal なペアがあれば、その
ペアを返し、なければ func を評価し、その結果を返す。

<例>
        (!xx '((a . 1) (b . 2) (c . 3) ((d e) . 4)))
        (sassqu 'a xx (lambda () (!z 5))) -> (a . 1)
        (sassqu '(d e) xx (lambda () (!z 5))) -> ((d e) . 4)
        (sassqu 'f xx (lambda () (!z 5))) -> 5"
  (tao:sass #'equal item alist func))

;;; ＠
;;; save-sstatus                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : save-sstatus &opt terno
;;; ターミナル terno の状態をセーブする。terno の既定値はこの関数が入力さ
;;; れたターミナル。
;;; ＠
;;; sbit                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sbit bit-array &rest subscripts
;;; 関数 bit と同じ働きをする。
;;; ビット配列 bit-array の、添字 subscripts により指定されたビットを返す。
;;; bit-array は単純ビット配列でなければならない。
;;;
;;; <例>
;;;         (!x (make-array 3))
;;;         	-> {vector}77794(common:simple-general-vector . 3)
;;;         (sbit x 1) -> nil
;;;         (!y (make-array '(5 5) :element-type 'bit))
;;;         	-> {applobj}70768(#!array . 10)
;;;         (sbit y 1 1) -> #0
;;; ＠
;;; scale-float                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : scale-float number integer
;;; number は、浮動小数点。(* number (expt (float b number) integer)) の
;;; 結果を返す。 b は number の内部表現に使われる基数。
;;;
;;; <例>
;;;         (scale-float 1.0 3) -> 8.0
;;;         (scale-float -1.0 3) -> -8.0
;;;         (scale-float 12.3 2) -> 49.2
;;; ＠

(defun tao:schar (string index)
  "schar                                  関数[#!subr]

<説明>
  形式 : schar string index
string の index (0 から始まる数字) の位置の文字を返す。index は、
string の長さより小さくなければならない。

<例>
        (schar \"abcdefghij\" 0) -> \"a\"
        (schar \"abcdefghij\" 3) -> \"d\"
        (schar \"asd\" 5) -> \"\""
  (if (< (cl:length string) index)
      ""
      (coerce (list (char string index)) 'string)))


(defun tao:sconc (&rest strings)
  "sconc                                  関数[#!subr]

<説明>
  形式 : sconc &rest string1 string2 ... stringN
string1 string2 ... stringN を 1 つの文字列に結合し、その結果を返す。

<例>
        (sconc \"a\" \"b\") -> \"ab\"
        (sconc \"123\" \"45\" \"6789\") -> \"123456789\"
        (sconc \"abc\" nil) -> \"abc\""
  (declare (optimize (safety 0) (speed 3))
           (dynamic-extent strings))
  (let ((len 0)
        (pos 0))
    (declare (fixnum len pos))
    (dolist (s strings)
      (declare (simple-string s))
      (incf len (length s)))
    (let ((result (make-string len)))
      (declare (simple-string result))
      (dolist (s strings)
        (declare (simple-string s))
        (loop :for c :across s
              :do (setf (schar result pos) c) (incf pos)))
      result)))

;;; screen                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : screen &opt terno
;;; ターミナル terno を screen モードにする。terno の既定値はこの関数が入力
;;; されたターミナル。screen モードにおいて画面表示は、terminal ストリーム
;;; に store される。入出力処理は多少遅くなるが、画面をファイルにしたり、
;;; ハードコピーを取るというような、いくつかの関数を利用できる。
;;; ＠

(defsynonym tao:search cl:search
  "search                                 関数[#!macro]

<説明>
  形式 : search seq1 seq2 &key :from-end :test :test-not :key  :start1
                               :end1 :start2 :end2
シーケンス seq1 の :start1 から :end1 までの文字が、seq2 の :start2
から :end2 までに含まれているか探し、あれば seq2 の左端 (:from-end で
nil でない値が指定された場合は右端) の要素の添字番号を返し、なかった場
合はnil を返す。

<例>
        (search '(b c) '(a b c d e)) -> 1
        (search '(a b c) '(a b c d e) :start1 2 :end1 3) -> 2
        (serch '(a b c) '(a b c d e d c b a) :start1 2 end1 3
        	:start2 3 end2 7) -> 6")

(defsynonym tao:second cl:second
  "second                                 関数[#!macro]

<説明>
  形式 : second list
list の 2 番目の要素の値を返す (最初の要素が 1 番目)。

<例>
        (second '(a b c))  ->  b")

(defmacro tao:select (item &body cases)
  "select                                 関数[#!macro]

<説明>
  形式 : select item (case1 form11 form12 ...)
                     (case2 form21 form22 ...)
                     ...
順に case1 case2 ... が item と eq かどうか調べ、最初に eq になった
後のフォームを順に評価し、結果を返す。一番最後の caseK を t または
otherwise に指定し、それまでの全てが、eq でなかった時は、その後にくる
フォームを無条件に評価する。各 casei は item と比較される前に評価される。
casej がリストのときには eq のかわりに memq でテストを行なう。

<例>
        (select 3 ((3 4) 'abc) (t 'xyz)) -> abc
        (select 'd ((a) \"a\") ((b) \"b\") ((c) \"c\"))
                -> (unbound-variable a)"
  (let ((v (gensym)))
    `(let ((,v ,item))
       (cond
	 ,@(mapcar (lambda (x)
		     (let ((case (car x))
			   (forms (cdr x)))
		       (cond ((consp case)
			      (if (and (eq 'quote (car case))
				       (eq 't (cadr case)))
				  `('T ,@forms)
				  `((member ,v (list ,@case) :test #'eq) ,@forms)))
			     ((atom case)
			      (if (or (eq t case) (eq 'otherwize case))
				  `('T ,@forms)
				  `((eq ,v ,case) ,@forms)))
			     ('T nil))))
		   cases)))))

(defmacro tao:selector (item fn &body cases)
  "selector                               関数[#!macro]

<説明>
  形式 : selector item fn (case1 form11 form12 ...)
                          (case2 form21 form22 ...)
                           ...
selector は selectq と同じ。ただし selector では、eq の代わりに equal
greaterp, string-lessp, などが利用できる。

<例>
        (selector 1 'greaterp
             (1 \"1\")
             (2 \"2\")
             (t \"3\")) -> \"3\""
  (let ((itm (gensym)))
    (let ((ecases
	   (mapcar (lambda (xx)
		     (let ((key (car xx)))
		       (cond ((or (eq t key)
				  (equal '(quote t) key)
				  (eq 'otherwise key))
			      `('T ,@(cdr xx)))
			     ((consp key)
			      `((member ,itm ',key :test ,fn) ,@(cdr xx)))
			     ((atom key)
			      `((funcall ,fn ,itm ,key) ,@(cdr xx)))
			     ('T nil))))
		   cases)))
      `(let ((,itm ,item))
	 (cond ,@ecases)))))

(defmacro tao:selectq (item &body cases)
  "selectq                                関数[#!subr]

<説明>
  形式 : selectq item (case1 form11 form12 ...)
                      (case2 form21 form22 ...)
                       ...
順に case1 case2 ... が item と eq かどうか調べ、最初に eq になった
後のフォームを順に評価し、結果を返す。一番最後の caseK を t または
otherwise に指定し、それまでの全てが、eq でなかった時は、その後にくる
フォームを無条件に評価する。casej がリストのときには eq のかわりに
memq でテストを行なう。

<例>
        (selectq 'a (c (!x 1)) (b (!x 2)) (a (!x 3))) -> 3
        (selectq 'a ((p q r) (!x 1)) ((a b c) (!x 2))) -> 2
        (selectq (car x)
          ((end terminate) (do-closing-work))
          (break (enter-break-mode))
          (inq (reply-to-inquiry (cdr x)))
          (update (update-data (cdr x)))
          (otherwise (report-error)) )"
  (let ((v (gensym)))
    `(let ((,v ,item))
       (cond
	 ,@(mapcar (lambda (x)
		     (let ((case (car x))
			   (forms (cdr x)))
		       (cond ((consp case)
			      (if (equal '(quote t) case)
				  `('T ,@forms)
				  `((member ,v ',case :test #'eq) ,@forms)))
			     ((atom case)
			      (if (or (eq t case) (eq 'otherwize case))
				  `('T ,@forms)
				  `((eq ,v ',case) ,@forms)))
			     ('T nil))))
		   cases)))))

(defmacro tao:selectq-every (item &body cases)
  "selectq-every                          関数[#!macro]

<説明>
  形式 : selectq-every item (case1 form11 form12 ... form1N)
                            (case2 form21 form22 ... form2N)
                            ...
順に case1 case2 ... が item と eq かどうか調べ、eq になる全てを
選び、対応するフォームを評価し、最後のフォームの評価結果を返す。一番
最後の caseK を t または otherwise に指定し、それまでの全てが、eq で
なかった時は、その後のフォームを無条件に評価する。casej がリストのとき
には eq のかわりに memq でテストを行なう。

<例>
        (selectq-every 'a (a (!x 1))
                          (b (!x 2))
                          (c (!x 3))
                          (a (!y 4))) -> 4
        x = 1, y = 4"
  (let ((uqitem (cadr item)))
    (let ((picked-cases
	   (remove-if-not (lambda (x)
			    (cond ((or (equal '(quote t) x) (eq t x)) 'T)
				  ((consp x)
				   (member uqitem x :test #'eq))
				  ((atom x)
				   (or (eq uqitem x) (eq 'otherwise x)))
				  ('T nil)))
			  cases :key #'car)))
    `(progn ,@(mapcan #'cdr picked-cases)))))

(defun tao:self-eval-form-p (form)
  "self-eval-form-p                       関数[#!expr]

<説明>
  形式 : self-eval-form-p arg
arg が self-eval 式ならば t 、そうでなければ nilを返す。self-eval 式と
は、nil、number, string, codnum, keyword, vector applobj で全てである。

<例>
        (self-eval-form-p 123) -> t
        (self-eval-form-p \"hihihi\") -> t
        (self-eval-form-p #expr) -> t
        (self-eval-form-p 'a) -> nil
        (self-eval-form-p '(1 2 3)) -> nil"
  (cond ((null form))
	((numberp form))
	((stringp form))
	;; codnum
	((keywordp form))
	((vectorp form))
	;; applobj
	('T nil)))

(defun tao:selfass-cons (object1 object2)
  "selfass-cons                           関数[#!expr]

<説明>
  形式 : selfass-cons object1 object2
cons と同じだが自己投入式を作るのに用いられる。

<例>
        (selfass-cons 'func (list 'x (assignee-cons 'y)))
        	 -> (!!func x !y)"
  (cons (intern (concatenate 'string "!!" (string object1)))
	object2))

(defun tao:selfass-list (&rest x)
  "selfass-list                           関数[#!expr]

<説明>
  形式 : selfass-list &rest x
list と同じだが自己投入式を作るのに用いられる。

<例>
        (selfass-list 'func 'x '(assignee-cons 'y)) -> (!!func x !y)
        (selfass-list 'hana (assign-cons 'sakura 'tsubaki))
        	 -> (!!hana (!sakura . tsubaki))"
  (cons (intern (concatenate 'string "!!" (string (car x))))
	(apply #'funcall #'list (cdr x))))


(defun tao:selfassp (form)
  "selfassp                               関数[#!subr]

<説明>
  形式 : salfassp arg
arg が自己投入式なら、それを返し、そうでなければ nil を返す。

<例>
        (selfassp '(!!cons 1234 !x)) -> (!!cons 1234 !x)"
  (and (eq 'tao:selfass
           (car form))
       form))

;;; semaphore                              クラス
;;;
;;; <説明>
;;;   CPU が、ただ 1 つの入出力リソースを持っている場合に、2 つのプロセス
;;; P と Q が、そのリソースを要求したとする。この場合 P と Q がそのリソース
;;; を共有することは不可能。 P と Q は、排他的に自身だけでリソースを使わな
;;; ければならない。セマフォは、このように、リソースの互いに排他的な使用の
;;; ための最も基本的なツール。TAO では、セマフォは、クラス SEMAPHORE の
;;; インスタンスとして提供され、3 つのインスタンス変数 :name
;;; sys:semaphore-process-queue :process をとる。:name の値は、セマフォの
;;; 名前を表す。プロセスには複数のセマフォが存在する可能性もあるので、名前
;;; が必要。:process の値は、セマフォを占有しているプロセスを表す。

;;; semi-globals                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : semi-globals &rest 'var
;;; var が指定された時は、それらをセミグローバル変数として定義し、nil に
;;; 初期化する。指定されない時は、セミグローバル変数の名前と値を返す。
;;;
;;; <例>
;;;         (semi-globals a b c) ->
;;;         	(+ - * / ** ++ // *** *prompt-function*
;;;                 *history-command-over it that +++ ///
;;;  		*screen-out-file* *ansi$caution-type* c b a)
;;;         a -> nil
;;;         b -> nil
;;;         c -> nil

;;; send                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : send receiver list-message-and-arg
;;; メッセージをインスタンスに送る。receiver で、メッセージを受け取る
;;; インスタンスを指定し、list-message-and-arg で、送られるメッセージの名前
;;; と送られたメッセージにより呼び出されるメソッドで使われる引数のリストを
;;; 書く。
;;;
;;; <例>
;;;         (send 1 '(+ 10)) -> 11

;;; send-class-message                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : send-class-message 'class-name 'message &rest 'args
;;; クラスメッセージ message を、クラス class-name へ送る。
;;;
;;; <例>
;;;         (defclass a (q) () ()) -> a
;;;         (defclass-method (a abc) () (!(cvar q) 10)) -> abc
;;;         (calss-variable q (class-of 'a)) -> nil
;;;         (send-calss-message a abc) -> 10
;;;         (class-variable 'q (class-of 'a)) -> 10

;;; send-mail                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : send-mail mailbox mail
;;; メイル mail を、メイルボックス mailbox に送る。
;;;
;;; <例>
;;;         (!m-box (make-instance 'mailbox)) -> m-box
;;;         (send-mail m-box 12345) -> 12345

(defmacro tao:seq (&rest forms)
  "seq                                    関数[#!subr]

<説明>
  形式 : seq &resat form1 form2 ... formN
form1 form2 ... formN を順に実行し、最後のフォームの値を返す。

<例>
         x = \"enclosed\" ならば
        (seq (prins \"[\") (prins x) (prins \"]\") x) -> \"enclosed\"
        [enclosed] を出力。"
  `(block nil
     ,@forms))

(defmacro tao:seqt (&rest forms)
  "seqt                                   関数[#!subr]

<説明>
  形式 : seqt &rest form1 form2 ... formN
form1 form2 ... formN を順に評価し、t を返す。ロジックプログラミング
の際、役立つ。

<例>
        (!x '(1 2 3)) -> (1 2 3)
        (seqt (!y (cdr x) (!w (car y))) -> t
        x = (1 2 3), y = (2 3), w = 2"
  `(block nil
     ,@forms
     t))

(defun tao:sequal (string1 string2)
  "sequal                                 関数[#!subr]

<説明>
  形式 : sequal string1 string2
stirng1 と string2 を比較し、一致した場合、string2 の値を返し、
そうでなければ nil を返す。大文字は小文字に変換する。

<例>
        (sequal \"abc\" \"abc\") -> \"abc\"
        (sequal \"abc\" 'abc) -> \"abc\"
        (sequal \"abc\" 'AbC) -> \"abc\"
        (sequal nil nil) -> エラー
        (sequal 123 123) -> エラー"
  (cond ((some #'null (list string1 string2))
	 (error "nil?"))
	((string-equal string1 string2)
	 (string-downcase string2))
	('T nil)))

(defun tao:sequencep (arg)
  "sequencep                              関数[#!expr]

<説明>
  形式 : sequencep arg
arg がシーケンスなら arg を返し、それ以外なら nil を返す。"
  (typecase arg
    (sequence t)
    (otherwise nil)))

(defsynonym tao:set cl:set
  "set                                    関数[#!subr]

<説明>
  形式 : set x val
x に val を代入する。

<例>
        (!x 'a) -> a, ここで x = a
        (set x '(p q r)) -> (p q r)
        a -> (p q r)
        (set (car x) 123) -> 123
        p -> 123")

;;; set-char-bit                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-char-bit char bit flag
;;; 文字 char のビット属性のうち、bit をクリアまたはセットした文字のデータ
;;; を返す。flag が nilならクリアし、 nil 以外ならセットする。
;;;
;;; <例>
;;;         (set-char-bit #¥c :control t) -> #¥control-c
;;;         (set-char-bit #control-x :control t) -> #¥control-x
;;;         (set-char-bit #control-x :control nil) -> "x"
;;;         (set-char-bit #¥x :meta t) -> "x"

;;; set-date                               関数[#!expr]
;;;
;;; <説明>
;;;   西暦日付及び時間を設定する。
;;;
;;; <例>
;;;         (set-date)
;;;         System assumes today is  6-Apr-87.
;;;         Input [date and] time: dd-mmm-yy hh:mm 07-Apr-87 17:31

;;; set-default-keep-generation-count      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-default-keep-generation-count pathname &opt kgc
;;; ディレクトリ pathname について、ファイルの世代をいくつにするかを決定
;;; する。例えば、kgc が 3 のとき、最新バーションを含めてそれより古い 3 世
;;; 代のバージョンを保存する。kgc の既定値は 1 。
;;;
;;; <例>
;;;         (set-default-keep-generation-count "cs:<dire>" 3) -> 3
;;;         vdir "test.tao"
;;;          -> test.tao.5    最新ファイルを含めて 3 世代ファイルを保存
;;;                     .4
;;;                     .3
;;;         ここで test.tao を更新すると、
;;;         vdir "test.tao"
;;;          -> test.tao.6
;;;                     .5
;;;                     .4

(defsynonym tao:set-difference cl:set-difference
  "set-difference                         関数[#!macro]

<説明>
  形式 : set-difference list1 list2 &key :test :test-not :key
list1 と list2 を対比し、list1 には存在するが list2 には含まれていない
要素を抽出し、リストにして返す。nset-difference は破壊版。

<例>
        (set-difference  '(1 2 3)  '(2 4 6))  ->  (1 3)
        (set-difference  '(1 2 3)  '(1 2 3))  ->  nil")

(defun tao:set-differenceq (list1 &rest lists)
  "set-differenceq                        関数[#!subr]

<説明>
  形式 : set-differenceq list1 &rest list2 ... listN
list1 には含まれているが、list2 ... listN には含まれていない要素を抽出
し、リストにして返す。ただし、返されるリストの中で重複した要素は、それ
が eq であれば、一方は削除される。list1 での要素の並び順序と返される
リストでの要素の並び順序は必ずしも一致しない。

<例>
        (set-differenceq '(1 2 3 4 5 6 4 2) '(5 3 1)) -> (2 4 6)
        (set-differenceq '(1 2 3 4 5 6 7) '(1 2) '(3 4) '(2 5))
        	 -> (6 7)"
  (do ((l list1 (cdr l))
       result
       (cmp (apply #'append lists)))
      ((endp l) (nreverse result))
      (or (member (car l) (append result cmp) :test #'equal)
	  (push (car l) result))))

;;; set-dispatch-macro-character           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-dispatch-macro-character char1 char2 func &opt readtable
;;; 読み込み表 readtable にディスパッチ文字として登録されている char1 の
;;; char2 に対する入力マクロ定義を関数 func で置き換える。char1 の表す文字
;;; を X、char2 の表す文字を Y とするとき、read が "XY" で始まるデータを
;;; 読み込む際には、入力ストリームと char2、nil を引数として func が呼び
;;; 出される。"XnY" で始まるデータを読み込む際には，n の表す数値が func
;;; への第 3 引数となる。ここで n はある非負整数の 10 表現である。
;;; make-dispatch-macro-charactor、get-dispatch-macro-charactor 参照。

;;; set-error-function                     関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-error-function f-name &opt process
;;; process において、f-name をエラー廃棄関数として定義する。
;;; process が省略されるとカレントプロセスが使われる。f-name は、引数を
;;; 3 つとり、x, y, z とする。x はエラー名を表すストリング。 y は最初の補助
;;; 情報 （大抵の場合、エラーが起こった関数を表す）。 z は 2 番目の補助
;;; 情報（大抵の場合、エラーを起こした不正引数 (illegal argument) を表す）。

;;; set-exclusive-or                       関数[#!macro]
;;;
;;; <説明>
;;;   形式 : set-exclusive-or list1 list2 &key :test :test-not :key
;;; list1 または list2 のいずれか一方に含まれている要素を抽出し、リストに
;;; して返す。nset-exclusive-or は破壊版。
;;;
;;; <例>
;;;         (set-exclusive-or  '(1 2 3) '(a b c))  ->  (1 2 3 a b c)
;;;         (set-exclusive-or  '(1 2 3) '())  ->  (1 2 3)

;;; set-in-instance                        関数[#!macro]
;;;
;;; 説明
;;;   形式 : set-in-instance instance var-name value
;;; instance のインスタンス変数 var-name に値 value を設定し、その値を返す。
;;;
;;; <例>
;;;         (defclass a () (x y) () :gettable :settable) -> a
;;;         (!bb (make-instance 'a x 5 y 6)) -> {udo}43848a
;;;         [bb x] -> 5
;;;         [bb y] -> 6
;;;         (set-in-instance bb 'x 10) -> 10
;;;         [bb x] -> 10

;;; set-job-name                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-job-name j-name &opt process
;;; process に、ジョブ名 j-name を与える。process が省略されるとカレント
;;; プロセスが使われる。

(defun tao:set-keep-generation-count (pathname &optional kgc)
  "set-keep-generation-count              関数[#!expr]

<説明>
  形式 : set-keep-generation-count pathname &opt kgc
ファイル pathname が、ファイルシステムで何世代保存されるかを決定する。
例えば、kgc が 3 のとき、pathname の最新バーションを含めてそれより古い
 3 世代のバージョンを保存する。kgc の既定値は 1 。

<例>
        (set-keep-generation-count \"cs:<dire>test.tao\" 3) -> 3
        vdir \"test.tao\"
         -> test.tao.5   最新ファイルを含めて 3 世代ファイルを保存
                    .4
                    .3
        ここで test.tao を更新すると、
        vdir \"test.tao\"
         -> test.tao.6
                    .5
                    .4"
  (declare (ignore pathname kgc))
  (values))

(defun tao:set-loc-offset (loc offset)
  "set-loc-offset                         関数[#!subr]

<説明>
  形式 : set-loc-offset x y
ロックビット x のオフセット を、y (メモリブロック内の語アドレスを示す
0 から始まる数字) にセットする。ロックビットのオフセットへのアクセス
関数は、loc-ossset 。

<例>
        (!a (get-memblk #!8b-memblk 16)) ->
        	{memblk}489557(#!8b-memblk . {dnil}16)
        (!b (locbit a 10)) ->
            {locbit}{memblk}489557(#!8b-memblk . {dnil}16) . {dnil}10)
        (loc-offset b) -> 10
        (set-loc-offset b 1) ->
            {locbit}{memblk}489557(#!8b-memblk . {dnil}16) . {dnil}1)
        (loc-offset b) -> 1"
  (setf (fli::pointer-%offset loc) 0)
  (fli:incf-pointer loc offset))

;;; set-macro-character                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-macro-character char func
;;;         	 &opt non-terminating-p readtable
;;; char が read によって見つけられた時、func を呼び出すマクロ文字となる
;;; ようにし、t を返す。non-terminating-p が 省略または nil が指定された
;;; 場合、char は非終端マクロ文字。すなわち、拡張されたトークンの中に組み
;;; 込まれることができる。get-macro-character 参照。

;;; set-priority                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-priority n &opt process
;;; process に優先順位 n を与える。process が省略されるとカレントプロセス
;;; が使われる。

;;; set-quantum                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-quantunm n &opt process
;;; process に quantum 値 n を与える。process が省略されるとカレント
;;; プロセスが使われる。

;;; set-syntax-from-char                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 set-syntax-from-char to-char from-char
;;;         	 &opt to-readtable from-readtable
;;; 読み込み表 to-readtable 中の to-char を、読み込み表 from-readtable 中
;;; の from-char と同一にする。to-readtable の既定値は現在の読み込み表
;;; (変数 *readtable* の値) 。from-readtable の既定値は nil で、標準の
;;; Lisp 読み込み表からの構文を用いることを意味している。

;;; set-sysmode                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-sysmode &key :car-nil-error :cdr-nil-error
;;;                     :one-char-string :common-lisp :negation-as-failure
;;; カレントプロセスの処理モードをセットする。
;;; (1)  :car-nil-error が nil 以外の値なら、フォーム (car nil) はエラー
;;; になる。 nil なら、nil を返す。既定値は t 。
;;; (2)  :cdr-nil-error が nil 以外の値なら、フォーム (cdr nil) はエラー
;;; になる。 nil なら、nil を返す。既定値は nil 。
;;; (3)  :one-char-string の値が nil 以外なら、キャラクタ "a" は "a" として
;;; 読まれる。nil なら、キャラクタ "a" は #¥a として読まれる。キャラクタ
;;; #¥a は、この引数の値が nil であろうとなかろうと #¥a として読まれる。
;;; 既定値は nil 。
;;; (4)  :common-lisp の値が nil 以外なら、以下の 4 つのことが行われる。
;;;       � :car-nil-error の値は nil になる
;;;       � :cdr-nil-error の値は nil になる
;;;       � :one-char-string の値は t になる
;;;       � 入出力廃棄ルーチンやストリング廃棄ルーチン等の種々のスイッチ
;;;          は、TAO モードから Common Lisp モードに変わる。
;;;       既定値は nil 。
;;; (5)  :negation-as-failure が nil 以外の値なら、ロジックプログラミング
;;; で、エラー "unbound variable" となると、バックトラックが起こる。つまり、
;;; "unbound variable" は nil と同じであると見なされる。値が nil なら、
;;; エラー "unbound variable" はエラーそのまま。既定値は nil 。
;;;
;;; <例>
;;;         (set-sysmode :common-lisp t) -> ok
;;;         (set-sysmode :car-nil-error t) -> ok

;;; sys:set-tage                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sys:set-tage x
;;; tage ビットが on になり、x が返る。
;;;
;;; <例>
;;;         (sys:set-tage nil) -> ()
;;;         (sys:set-tage 100) -> #144 [8進の shortnum にする]

;;; set-terminal-type                      関数[#!expr]
;;;
;;; <説明>
;;;   形式 : set-terminal-type def-type
;;; ユーザに terminal タイプを次の様に尋ねる。
;;; Please input terminal type (default=def-type):
;;; cit101e,vt100 の様な terminal タイプをタイプするとユーザのターミナル
;;; が、その terminal タイプになる。crlf をタイプすると def-type がユーザ
;;; のターミナルになる。login するとき使用される。

;;; setf                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : setf &rest place datum
;;; datum が place に格納される。datum を格納できる場所を示しうる関数に、
;;; car,cdr, cadr,nth,first,symbol-value,nthv,array,shead,substring 等が
;;; ある。これらの関数は setf の最初の引数として使用可能。
;;; TAO では、setf の代わりにスペシャルシンボルマクロ ! を使う方が良い。
;;;
;;; <例>
;;;         (setf x 1 y 2) -> 2
;;;         x = 1, y = 2
;;;         (setf x '((a b) (c d) (e f))) -> ((a b) (c d) (e f))
;;;         (setf y "qwertyu") -> "qwertyu"
;;;         (setf (cadr x) '(1 2 3)) -> (1 2 3)
;;;         x -> ((a b) (1 2 3) (e f))
;;;         (setf (stail y 3) "asdfg") -> "asdfg"
;;;         y -> "qweasdf"

;;; setq                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : setq &rest x1 val1 x2 val2 ...
;;; まず val1 を評価しその結果を x1 に格納する。次に val2 を評価しその結果
;;; を x2 に格納する。 ...  そして、代入された最後の値を返す。
;;; setq より ! のほうが速い。
;;;
;;; <例>
;;;         (setq x 1 y 2 z 3) -> 3
;;;         x = 1, y = 2 , z = 3
;;;         (setq x (+ y z) y (+ z x) z (+ x y)) -> 13
;;;         x = 5, y = 8 ,z = 13

;;; seventh                                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : seventh list
;;; list の 7 番目の要素の値を返す (最初の要素が 1 番目)。
;;;
;;; <例>
;;;         (seventh '(0 1 2 3 4 5 6 7 8))  ->  6

;;; sg-value                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sg-value var
;;; セミグローバル変数 var の値を返す。
;;;
;;; <例>
;;;         (semi-globals abc) ->
;;;         (+ - * / ** ¥++ // *** *prompt-function*
;;;          *history-command-overwrite* it that ¥*** /// *screen-outfile*
;;;          *ansi$caution-type* abc)
;;;         (!abc '(a b c)) -> (a b c)
;;;         (sg-value 'abc) -> (a b c)

;;; shadow                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : shadow symbol &opt package
;;; symbol と同じ印字名のシンボルが package に存在しなければ新しいシンボル
;;; をこの印字名で生成し、package に挿入し、t を返す。生成されたシンボルは
;;; package がユースするパッケージにある同一印字名の外部シンボルをシャドウ
;;; する。
;;; <例>
;;;         (shadow 'if) -> t
;;;         (de if (x)
;;;         	(cond ((zerop x) i)
;;;         	     (t (* x (if (1- x)))))) -> if
;;;         (if 0) -> 1
;;;         (if 2) -> 2

;;; shadowing-import                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : shadowing-import symbol &opt package
;;; import と同じ操作を行なうが symbol が package 中にすでにアクセス可能な
;;; シンボルをシャドウするような場合であってもエラーを警告しない点が異なる。
;;; symbol を package の内部シンボルとして登録し、t を返す。 symbol と同じ
;;; 印字名のシンボルが既にパッケージにあれば、そのシンボルを unintern する。
;;; symbol は package がユースするパッケージに登録されている同一印字名の
;;; 外部シンボルをシャドウする。
;;; <例>
;;;         (!x 10)
;;;         (import sys:x) -> Name confilict! x
;;;         x -> エラー
;;;         (sys:with-privilege (!x 20)) -> 20
;;;         (sys:with-privilege (!window:x)) -> 30
;;;         (window:x) -> 30
;;;         (sys:x) -> 20
;;;         x -> 20
;;;         (shadowing-import window:x) -> t
;;;         x -> 30

(defun tao:shead (object &optional (n 1))
  "shead                                  関数[#!subr]

<説明>
  形式 : shead object &opt n
n >= 0 のときは、object の最初の n 文字から成る部分ストリングを作成し、
その結果を返す。
n < 0 のときは、object の最後の n 文字から成る部分ストリングを作成し、
その結果を返す。 n の既定値は 1。 object はストリングまたはアトム。

<例>
        (shead \"head\") -> \"h\"
        (shead 'head 2) -> \"he\"
        (shead 'string) -> \"s\"
        (shead \"larger\" -3) -> \"ger\"
        (shead \"short\" 10) -> \"short\"
        (shead \"abc\" 0) -> \"\"
        (shead \"\" 3) -> \"\""
  (let ((str (typecase object
	       (string object)
	       (atom   (string-downcase (string object)))
	       (otherwise (error "~S is not type of ATOM." object)))))
    (let ((len (cl:length str)))
      (if (or (zerop n) (zerop len))
	  ""
	  (let ((n (* (mod n len) (/ n (abs n)))))
	    (cond ((> n 0) (subseq str 0 n))
		  ((< n 0) (subseq str (+ len n) len))
		  ('T (subseq str 0 len))))))))

;;; shift#                                 ロカティブオペレータ
;;;
;;; <説明>
;;; 形式 : loc shift# n
;;;   loc について論理シフト操作を行う。 n で左へ何ビットシフトされるかが
;;; 指定される。
;;;
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- #5252) -> 2730
;;;         (s <- (p shift# 1 )) -> 5461 (#12524)
;;;         (s <- (p shift# 2 )) -> 1365 (#2525)

;;; shiftf                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : shiftf &rest x1 x2 ... xn value
;;; x1 x2 x3 ... value を評価し、各値を右から左にシフトする。
;;; x2 の値は、x1 に格納され、... value は xnに格納され、x1 のもとの値が
;;; 返される。
;;;
;;; <例>
;;;         (setq x (list 'a 'b 'c)) -> (a b c)
;;;         (shiftf (cadr x ) 'd) -> b
;;;         x -> (a d c)
;;;         (!x '(1 a b c)) -> (1 a b c)
;;;         (shiftf (car x) (cadr x) (caddr x) (cadddr x) 'd) -> 1
;;;         x -> (a b c d)

;;; short-float-epsilon                    定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の負の short-float が格納されている
;;; システム定数であり、本システムでは、5.72204e-6。

;;; short-float-negative-epsilon           定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の負の short-float が格納されている
;;; システム定数であり、本システムでは、3.8147e-6。

;;; short-site-name                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : short-site-name
;;; コンピュータハードウェアの物理的な位置を識別する文字列を短い名前で返す。
;;; ELIS システムでは、"NUE group" が返る。
;;;
;;; <例>
;;;         "NUE group"
;;;         "MIT AI Lab"
;;;         "CMU-CSD"

;;; shortfloatp                            関数[#!subr]
;;;
;;; <説明>
;;;   形式 : shortfloatp number
;;; number が shortfloat なら、number を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (shortfloatp 3.6) -> 3.6
;;;         (shortfloatp 3.6f0) -> nil
;;;         (shortfloatp 23/37) -> nil
;;;         (shortfloatp 10) -> nil

;;; shortnump                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : shortnump number
;;; number が shortnum ( -2**23 から 2**23-1) ならば、number を返し、それ
;;; 以外なら nil を返す。
;;;
;;; <例>
;;;         (shortnump 1) -> 1
;;;         (shortnump -8388608) -> -8388608 (the smallest shortnum)
;;;         (shortnump 8388607) -> 8388607 (the largest shortnum)
;;;         (shortnump 12345678) -> nil

;;; show-bit-vector                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : show-bit-vector arg
;;; arg がビットベクタであれば、そのビットベクタの各要素の値を返す。
;;;
;;; <例>
;;;         (!m (make-array 5 :element-type 'bit)) ->
;;;         	{memblk}488998(#!1b-memblk . {dnil}5)
;;;         (show-bit-vector m) -> (#0 #0 #0 #0 #0)
;;;         (!(aref m 1) '1) -> 1
;;;              (show-bit-vector m) -> (#0 #1 #0 #0 #0)
;;;         (!a (64b-unsigned)) -> a
;;;         (a <- 0) -> #0
;;;         (show-bit-vector a) -> (#0 #0 #0 .... #0)   (#0 が 64個)
;;;         (!c (array #!1b-memblk 6)) -> {applobj}51002(#!array . 8)
;;;         (show-bit-vector c) -> (#0 #0 #0 #0 #0 #0)

;;; show-class                             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : show-class class
;;; class のクラスベクタの内容を表示する。
;;;
;;; <例>
;;;         (defclass abc () ((a 1) (b 2)) () :gettable :settable) -> abc
;;;         (show-class 'abc)   (クラスベクタの内容を表示)

;;; show-class-variables                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 : show-class-variables class
;;; class のクラス変数の内容を表示する。
;;;
;;; <例>
;;;
;;;         (defclass test () ((a 1) (b 2)) () :gettable :settable)
;;;         	-> test
;;;         (show-class-variables test) -> No class vars for test
;;;         			       t
;;;         (defclass abc (x) ((a 1) (b 2)) () :gettable :settable) -> abc
;;;         (show-class-variables abc) ->
;;;         	vtitle: {vector}1811502(class . 12) vsize:2
;;;         	0 kdr: x
;;;         	1 kar: nil
;;;         {vector}1806440({vector}1811502(class . 12) .2)

;;; show-terminal                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : show-terminal
;;; 現在のターミナルの状態を表示する。

;;; show-vector                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : show-vector vector
;;; vector の内容をプリントする。トップレベルで使用すること。
;;;
;;; <例>
;;;         (!x (vcons 'asd 3)) -> {vector}57961(asd . 3)
;;;         (show-vector x) ->  vtitle:asd vsize:3
;;;                                 0 kdr: nil
;;;                                 1 kar: nil
;;;                                 2 kdr: nil

;;; signed-integer-locative-arrays         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : signed-integer-locative-arrays &rest array-spec
;;; 要素のデータ型が符号付き整数ロカティブである配列を生成し、array-spec
;;; をリストにして返す。array-spec は、(var dimension) という形式。ここで、
;;; var は生成される配列の名前であり、dimension は配列の次元を指定する整数
;;; のリスト。
;;;
;;; <例>
;;;         (signed-integer-locative-arrays (a1 10) (a2 (-5 4) 47))
;;;         -> ((a1 10) (a2 (-5 4) 47))
;;;         2 つの配列 a1 a2 が生成。
;;;         a1 は、ランクが 1 で、次元が 0 から 9 である配列。
;;;         a2 は、ランクが 2 で、第 1 次元が -5 から 4 、第 2 次元が
;;;         0 から 46 である配列。
;;;         ((a1 5) <- 123) -> 123
;;;         (a1 5) -> 123
;;;         ((a2 -3 39) <- 456) -> 456
;;;         (a2 -3 39) -> 456

#+lispworks
(defmacro tao:signed-integer-locatives (&rest vars)
  "signed-integer-locatives               関数[#!exprdyn]

<説明>
  形式 : signed-integer-locatives &rest var1 var2 ... varN
N 個の 64 ビット符号つき整数ロカティブを生成し、それらを対応する各々の
変数に代入し、リスト (var1 var2 ... varN) を返す。初期設定は行わない。

<例>
        (signed-integer-locatives d e f g h) -> (d e f g h)
        d -> 476365
        e -> 476366"
  `(progn
     ,@(mapcar (lambda (v)
                 `(setf ,v (fli:allocate-foreign-object :type :int64)))
               vars)
     ',vars))

;;; signum                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : signum number
;;; 有理数 number の値に対応して以下の値を返す。
;;;         負      -1
;;;         0        0
;;;         正       1
;;;
;;; <例>
;;;         (signum 0) -> 0
;;;         (signum -3.7l5) -> -1.0
;;;         (signum 415) -> 1
;;;         (signum #c(7.5 10.0)) -> #c(0.60 0.80f0)
;;;         (signum #c(0.0 -14.7)) -> #c(0.0f0 -1.0f0)

;;; common:simple-array-p                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:simple-array-p array
;;; array が単純配列 なら、array を返し、それ以外なら nil を返す。単純配列
;;; とは、共有されてなく、かつ adjustable でなくさらにフィルポインタを持た
;;; ない配列のこと。
;;;
;;; <例>
;;;         (!x (array '(2 2))) -> {applobj}1783962(#!array . 8)
;;;         (common:simple-array-p x) -> {applobj}1783962(#!array . 8)

;;; common:simple-bit-vector-p             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:simple-bit-vector-p vector
;;; vector が単純ビットベクタであれば vector を、それ以外なら nil を返す。

;;; common:simple-string-p                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:simple-string-p string
;;; string が単純文字列であれば string を、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (common:simple-string-p "abcd") -> "abcd"
;;;         (common:simple-string-p "TAO") -> "TAO"
;;;         (common:simple-string-p "a") -> nil

;;; common:simple-vector-p                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:simple-vector-p vector
;;; vector が単純ベクタであれば t を、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (common:simple-vector-p #(1 2 3 4 5))
;;;         	-> {vector}80775(simple-vector . 5)

;;; sin                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sin number
;;; numbner (単位:ラジアン) に対応する正弦値を返す。
;;;
;;; <例>
;;;         (sin [pi / 6]) -> 0.50f0
;;;         (sin [pi / 4]) -> 0.707106781186548f0

;;; single-float-epsilon                   定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の正の single-float が格納されている
;;; システム定数であり、本システムでは、1.11022302462516f-16。

;;; single-float-negative-epsilon          定数
;;;
;;; <説明>
;;;   システムで処理し得る最小の負の single-float が格納されている
;;; システム定数であり、本システムでは、5.55111512312579f-17。

;;; singlefloatp                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : singlefloatp number
;;; number が単精度の浮動小数点数なら t 、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (singlefloatp 1.23) -> nil
;;;         (singlefloatp 1.23f0) -> t

;;; sinh                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sinh number
;;; number の値 (単位:ラジアン) に対応する双曲的正弦値を返す。
;;;
;;; <例>
;;;         (sinh 0.5f0) -> 0.521095305493748f0
;;;         (sinh 1.0f0) -> 1.1752011936438f0

;;; sixth                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : sixth list
;;; list の 6 番目の要素の値を返す (最初の要素が 1 番目)。
;;;
;;; <例>
;;;         (sixth '(0 1 2 3 4 5 6 7 8))  ->  5

;;; sleep                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : sleep number &opt integer
;;; 実時間で、number とほぼ同じ秒数の間、実行を止め、その後、実行を再開する。
;;; 返される値は、t 。
;;;
;;; <例>
;;;         (sleep 8.)     8 秒間実行をやめ、その後実行を続ける。
;;;         (sleep 1 30) 1.5 秒   		"
;;;         (sleep 2.4)  2.4 秒    		"

(defun tao:slength (string)
  "slength                                関数[#!subr]

<説明>
  形式 : slength string
string の長さを返す。

<例>
        (slength \"\") -> 0
        (slength \"a\") -> 1
        (slength \"abcdefghijkl\") -> 12"
  (declare ((or string symbol) string))
  (cl:length (string string)))

(defun tao:slex (string1 string2)
  "slex                                   関数[#!subr]

<説明>
  形式 : slex string1 string2
string1 (文字列またはアトム) が、string2 より辞書順的に大きい場合、
string2 の値を返し、そうでなければ nil を返す。大文字と小文字の相違は
無視。

<例>
        (slex \"story\" 'store) -> \"store\"
        (slex \"はな\" \"はし\") -> \"はし\"
        (slex \"はな\" \"はなし\") -> nil"
  (and (string-greaterp string1 string2)
       (string string2)))

(defun smemq* (string1 string2 eql=)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (let ((len1 (cl:length string1))
	  (len2 (cl:length string2)))
      (and (<= len1 len2)
	   (do ((str string2 (subseq str 1))
		(cnt len2 (1- cnt)))
	       ((cond ((funcall eql= string1 (subseq str 0 len1)))
		      ((and (= cnt len1)
			    (funcall (complement eql=) string1 str))
		       (return nil))
		      ('T nil))
		(- len2 cnt)))))))

(defun tao:smemq (string1 string2)
  "smemq                                  関数[#!subr]

<説明>
  形式 : smemq string1 string2
string1 の文字列パターンが、string2 の部分ストリングとして含まれている
かどうかを調べ、あった場合には string2 内の先頭文字の位置を返し、
なければ nil を返す。1 バイトのアルファベットにおいて大文字、小文字は
無視されるが、日本語文字は全てこれらを区別する。

<例>
        (smemq \"a\" \"ABD\") -> 0
        (smemq \"a\" \"abcdef\") -> 0
        (smemq \"abc\" 'fedcba) -> nil
        (smemq \"efg\" \"abcdefgh\") -> 4
        (smemq \"いう\" \"あいうえお\") -> 1
        (smemq \"アイウエ\" \"アイウ\") -> nil
        (smemq \"あ\" \"ぁっ\") -> nil"
  (smemq* string1 string2 #'string-equal))

(defun tao:smemq-case (string1 string2)
  "smemq-case                             関数[#!subr]

<説明>
  形式 : smemq-case x y
string1 の文字列パターンが、string2 の部分ストリングとして含まれている
かどうかを調べ、あった場合には string2 内の先頭文字の位置を返し、
なければ nil を返す。

<例>
        (smemq-case \"A\" \"aAbc\") -> 1
        (smemq-case \"d\" \"abc\") -> nil
        (smemq-case \"ab\" \"cdab\") -> 2
        (smemq-case \"a\" \"ABC\") -> nil
        (smemq-case \"あ\" \"ぁっ\") -> nil"
  (smemq* string1 string2 #'string=))

(defun tao:snull (string)
  "snull                                  関数[#!subr]

<説明>
  形式 : snull string
string が null ストリング (\"\") なら、\"\" を返し、そうでなければ nil を
返す。

<例>
        (snull \"\") -> \"\"
        (snull 123) -> nil
        (snull \"ab\") -> nil
        (suull \"いちご\") -> nil"
  (and (string-equal string "") ""))

;;; software-type                          関数[#!expr]
;;;
;;; <説明>
;;;   現在サポートされているソフトウェアの一般的な名前を識別する文字列を
;;; 返す。ELIS システムでは、"NUE" が返る。
;;;
;;; <例>
;;;         "NUE"
;;;         "Spice"
;;;         "TOPS-20"
;;;         "ITS"

;;; software-version                       関数[#!expr]
;;;
;;; <説明>
;;;   現在サポートされているソフトウェアのバージョンを識別する文字列を返す。
;;;
;;; <例>
;;;         "0.26 [25-Apr-87] Lap file becomes bex"

;;; some                                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 : some pred seq1 &rest seq2 ... seqN
;;; 条件 pred をシーケンス seq1 seq2 ... seqN に順番に適用し、最初に、nil
;;; 以外の値になったところで、直ちにその値を返し、最後まで nil であった場合
;;; には、nil を返す。シーケンスの数は、pred がとる引数の数と同じでなければ
;;; ならない。
;;;
;;; <例>
;;;         (some #'oddp '(1 2 3)) -> 1
;;;         (some #'oddp '(2 4 6)) -> nil
;;;         (some #'integerp '(1.2 3.4 5 6.7)) -> 5
;;;         (some #'> '(1 2 3 4 5) '(0.5 1.4 2 5.9 3)) -> 2

(defun tao:sort (list func)
  "sort                                   関数[#!subr]

<説明>
  形式 : sort list func
list の全ての要素を、関数 func に従い並べ変え、その結果を返す。
破壊的である。 func は常に t を返す。

<例>
        (sort (list 237 74 3 -43 470) '<) -> (-43 3 74 237 470)
        (sort (list \"abc\" \"ab\" \"bdc\" \"aac\") 'string-lessp)
        	 -> (\"aac\" \"ab\" \"abc\" \"bdc\")
        (sort (list '((3 . c) (1 . d) (1 . e)))
              (lambda (x y) (<= (car x) (car y))) )
        	 -> ((1 . d) (1 . e) (3 . c))"
  ;; func は常に t を返す。はsortの返り値のことではないのでは？
  (cl:sort list func))

(defsynonym common:sort cl:sort
  #.(string '|common:sort                            関数[#!macro]

<説明>
  形式 : common:sort seq pred &key :key
シーケンス seq を条件 pred に従って並びかえ、結果を返す。

<例>
        (!a '("abc" "qwe" "dfg" "ert"))
        (!b (common:sort a #'string-lessp))
        a -> ("abc" "dfg" "ert" "qwe")
        b -> ("abc" "dfg" "ert" "qwe")|))

(defun tao:sortcar (list func)
  "sortcar                                関数[#!expr]

<説明>
  形式 : sortcar list func
list の各要素を各々の car 部に着目して、関数 func に従って並べかえ、
結果を返す。並べかえ処理が list それ自身に対してでなく,各要素の
car に対して適用されるということを除けば、sort と同じ。

<例>
        (!x '((qwe rty uio) (asd fgh jkl) (zxc vbn m,.)))
           -> ((qwe rty uio) (asd fgh jkl) (zxc vbn m,.))
        (!y (sortcar x 'char-lessp))
           -> ((asd fgh jkl) (qwe rty uio) (zxc vbn m,.))
        y -> ((asd fgh jkl) (qwe rty uio) (zxc vbn m,.))
        x -> ((qwe rty uio) (zxc vbn m,.))"
  (cl:sort list func :key #'car))

;;; special-form-p                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : special-form-p symbol
;;; symbol が特殊形式なら t、そうでないならば nilを返す。特殊形式の例は
;;; block, catch, compiler-let, declare, eval-when, flet,common-function,
;;; go, if, labels, let, let*, macrolet, multiple-value-call,
;;; multiple-value-prog1, progn, progv, quote, return-from, setq,
;;; common:tagbody, the, throw, unwind-protect 等。
;;;
;;; <例>
;;;         (special-form-p 'if -> {applobj}36593(#!macro . 6)
;;;         (special-form-p 'setq) -> {applobj}36470(#!macro . 6)
;;;         (special-form-p 'defun) -> {applobj}1761368(#!expr . 6)
;;;         (special-form-p 'car) -> nil
;;;         (special-form-p 'cond -> {applobj}32990(#!subr . 6)

;;; special-stream                         クラス
;;;
;;; <説明>
;;;   ターミナルクラス、文字列入力ストリーム、文字列出力ストリーム、
;;; 双方向ストリーム、エコーストリーム、ブロードキャストストリーム、
;;; コンカティネイティドストリームの root-class である。
;;; special-stream は、s 式によってサポートされる。
;;; ユーザは、normal-stream を変更できないが、special-stream のサブクラスは
;;; 作れる。それは、micro-code プログラムによってサポートされている。

;;; special-stream-p                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : special-stream-p stream
;;; stream が normal-stream ならば stream を返し、そうでなければ nil を返す。

(defmacro tao:special-variables (&rest args)
  "special-variables                      関数[#!subr]

<説明>
  形式 : special-variables &rest var1 var2 ... varN
var1 var2 ... varN をスペシャル変数、つまり静的スコープの外側から
アクセスできる変数として宣言し、その名前を返す。これらの変数を宣言する
これは、プログラムの本体もしくは関数の本体の 1 番最初に指定する。

<例>
        (de main-func (x y &aux status line)
            (special-variables status line)
                 ... )"
  ;;--- TODO
  `'(:todo declare (special ,@args)))


;;; speed                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : speed form
;;; form を実行するのに費やされたネット時間を返す。

(defmacro tao:spop (object &optional (n 1))
  "spop                                   関数[#!subr]

<説明>
  形式 : spop object &opt n
object の先頭から n 文字抽出して返す。破壊的。n の既定値は 1。
object はストリングかアトムでなければならない。

<例>
        (!x \"abcdef\") -> \"abcdef\"
        (spop x) -> \"a\"
        x -> \"bcdef\"
        (spop x 2) -> \"bc\"
        x -> \"def\"
        (!bar '(\"John\" \"runs\" \"fast\")) -> (\"John\" \"runs\" \"fast\")
        (spop (cadr bar) 3) -> \"run\"
        bar -> (\"John\" \"s\" \"fast\")"
  (typecase object
    (string `(setf ,object (subseq ,object ,n)))
    (atom   `(setf ,object (subseq (string ,object) ,n)))
    (otherwise (error "~S is not of type ATOM" object))))

;;; spy                                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : spy terno
;;; ターミナル terno をスパイする。

;;; sqrt                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sqrt number
;;; number の主平方根の値を返す。number が負の場合、複素数になる。
;;;
;;; <例>
;;;         (sqrt 9.0) -> 3.0
;;;         (sqrt -9.0) -> エラー
;;;         (sqrt 9) -> 3.0f0
;;;         (sqrt -9) -> #c(0.0f0 3.0f0)

(defun tao:sreverse (string)
  "sreverse                               関数[#!subr]

<説明>
  形式 : sreverse string
string を反対に並び換え、そのコピーを返す。

<例>
        (sreverse \"abcdefg\") -> \"gfedcba\"
        (sreverse \"あいうえお\") -> \"おえういあ\"
        (sreverse \"漢字をかく\") -> \"くかを字漢\""
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (string string))
  (reverse string))

;;; sstatus                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : sstatus bas:option char bas:arg &opt bas:chn login
;;;                                               bas:meaning
;;; キーボードのキー割り当てを変更する。現在、ttyint だけが、bas:option
;;; に対して許された値。
;;;
;;; <例>
;;;         (seq (sstatus ttyint #101 #102) (sstatus ttyint #102 #101) )
;;;         (それがターミナルなら) *standard-input* についてのキーボード
;;;         指定を A から B 及び B から A に変える。
;;;         (seq (sstatus ttyint #101 #101) (sstatus ttyint #102 #102) )
;;;         元のキーボード指定に戻す。
;;;         (sstatus ttyint #1 'xoff)
;;;         ctrl-A の内容を xoff code にセットする。
;;;         (sstatus ttyint #2 'xon)
;;;         ctrl-B の内容を xon code にセットする。
;;;         (sstatus ttyint #1 'break)
;;;         ctrl-A の内容を通常の ctrl-C としてセットする。
;;;         (sstatus ttyint #1 'inspect)
;;;         ctrl-A の内容を通常の ctrl-T としてセットする。
;;;         (sstatus ttyint #1 'abort)
;;;         ctrl-A の内容を通常の ctrl-¥ としてセットする。
;;;         (sstatus ttyint #1 'suppress)
;;;         ctrl-A の内容を通常の ctrl-O としてセットする。
;;;         (sstatus ttyint #1 `(special-key-interrupt ,terno))
;;;         ctrl-A に新しいキーボード中断関数を登録する。

(defsynonym common:stable-sort cl:stable-sort
  "common:stable-sort   未インプリメント  関数[#!macro]

<説明>
  形式 : common:stable-sort seq pred &key :key
シーケンス seq を条件 pred に従って並べ変え、その結果を返す。
安定なソート。")

(defun tao:stail (string &optional (n 1))
  "stail                                  関数[#!subr]

<説明>
  形式 : stail string &opt n
n >= 0 のとき、string の n 番目の文字 (0 から数える) から始まる部分
文字列を生成し返す。
n < 0 のとき、string の最後から n 文字を取り除いた部分文字列を生成し
返す。 n の既定値は 1。

<例>
        (stail \"tail\") -> \"ail\"
        (stail \"tail-of-snake\" 8) -> \"snake\"
        (stail \"toolarge\" 100) -> \"\"
        (stail 'string -2) -> \"stri\"
        (stail \"string\" 0) -> \"string\"
        (stail \"\" 3) -> \"\"
        (stail \"さようなら\" 3) -> \"なら\""
  (let ((string
	 (typecase string
	   (string string)
	   (atom   (string string))
	   (otherwise (error "~S is not of type ATOM." string)))))
  (let ((len (cl:length string)))
    (cond ((zerop n) string)
	  ((plusp n) (if (<= n len) (subseq string n) ""))
	  ('T (if (<= (- n) len) (subseq string 0 (+ len n)) ""))))))

;;; standard-char-p                        関数[#!subr]
;;;
;;; <説明>
;;;   形式 : standard-char-p char
;;; char が標準文字であれば char を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (standard-char-p #¥a) -> "a"
;;;         (standard-char-p #¥space) -> #¥space
;;;         (standard-char-p #¥Backspce) -> nil
;;;         (standard-char-p #¥Tab) -> nil
;;;         (standard-char-p #¥Rubout) -> nil
;;;         (standard-char-p #¥Linefeed) -> nil
;;;         (standard-char-p #¥Return) -> nil
;;;         (standard-char-p #¥Page) -> nil

(defun tao:standard-read (&optional (stream *standard-input*))
  "standard-read                          関数[#!expr]

<説明>
  形式 : standard-read &opt stream
*read-base* が 10 、*read-eof-value* が :eof のような既定値をとる
パラメータで read を実行する。"
  (let ((*read-base* 10.)
        (tao:*read-eof-value* :eof))
    (read stream t tao:*read-eof-value* t)))

;;; standard-write                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : standard-write object &ot stream
;;; *print-base* が 10、*print-case* が :downcase、*print-package* が t
;;; 等のように標準値をとるパラメータによって write を実行する。stream が
;;; 省略されると、*standard-output* の値が使われる。object を返す。
;;;
;;; <例>
;;;         (standard-write 'abc) -> abc
;;;         			 abc
;;;         (standard-write 97) -> 97
;;;         		       97
;;;         (standard-write "abc def") -> "abc def"
;;;         			      "abc def"
;;; ＠
;;; step                                   関数[#!expr]
;;;
;;; <説明>
;;;   形式 : step &rest func
;;; func を評価し、func が返すものを返す。このとき、評価は 1 ステップずつ
;;; 逐次解釈的に進められる。ユーザは、会話的に 1 ステップずつ評価を進行させ
;;; ることもできる。
;;;
;;; <例>
;;;           step
;;;           step>(car '(a b c))
;;;           a
;;;           stepper-end
;;; ＠
;;; stream-element-type                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : stream-element-type stream
;;; stream から読み出される、あるいはそれに対して書き込まれるオブジェクト
;;; を表す型指定子を返す。
;;; ＠
;;; streamp                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : streamp stream
;;; stream がストリームなら、stream を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (!x (open "test.tao") -> {udo}1785013file-stream
;;;         (streamp x) -> {udo}1785013file-stream
;;; ＠

(defun tao:strh-to-char (string)
  "strh-to-char                           関数[#!subr]

<説明>
  形式 : strh-to-char string
string を文字又はシンボルに変換し、その結果を返す。

<例>
        (strh-to-char \"a\") -> \"a\" {これは文字、すなわち#¥a}
        (strh-to-char \"ab\") -> \"a\""
  (cl:schar string 0))


(defsynonym tao:string cl:string
  #.(cl:string '|string                                 関数[#!expr]

<説明>
  形式 : string arg
arg の属性を評価し、その属性により以下の処理を行なう。
    オブジェクトの属性                       返値
      文字                            -----> オブジェクトの値
      シンボル                        -----> 印字名
      文字列文字(string-char型の文字) -----> 1 個の文字を含む文字列

<例>
        (string "ab") -> "ab"
        (string 'ab) -> "ab"
        (string 1) -> エラー
        (string "漢字") -> "漢字"
        (string 'あさ) -> "あさ"
        (string "1") -> "1"|))

;;; string                                 クラス
;;;
;;; <説明>
;;;   インスタンスは "anpontan", "a", "noroma" のように文字も文字列も可。
;;; 文字には 8 ビット文字と、16 ビット文字の 2 種類ある。8 ビット文字は文字、
;;; ASCII 文字であり、16 ビット文字は日本語文字、16 ビット文字 JIS コード
;;; 文字である。少なくとも 1 つ 16 ビット文字をもつ文字列は jstring と呼ば
;;; れる。文字と文字列はフォント情報を持ち得る。フォント情報を持つ文字列は
;;; fat-string と呼ばれる。

(defun tao:string-append (&rest strings)
  "string-append                          関数[#!subr]

<説明>
  形式 : string-append &rest string1 string2 ... stringN
string1 string2 ... stringN を連結した文字列を作成し、返す。

<例>
        (string-append \"str\" \"ing-app\" 'end) -> \"string-append\"
        (string-append \"va\" nil \"la\") -> \"vala\" ( \"vanilla\"ではない)
        (string-append) -> \"\"
        (string-append \"漢\" \"字\" \"と\" \"ひらがな\") -> \"漢字とひらがな\""
  (apply #'concatenate 'string
	 (mapcar
	  (lambda (s) (if (stringp s)
			  s
			  (string-downcase (string s))))
	  (remove nil strings))))

(defun tao:string-byte-count (string)
  "string-byte-count                      関数[#!subr]

<説明>
  形式 : string-byte-count string
string のバイト数を返す。

<例>
        (!x \"a b c\")
        (string-byte-count x) -> 5
        (string-byte-count \"1 2 3\" -> 5
        (string-byte-count \"あいうえお\") -> 10
        (string-byte-count \"今日は\") -> 6"
  (babel:string-to-octets string))


(defun tao:string-capitalize (string &optional (start 0) end)
  "string-capitalize                      関数[#!subr]

<説明>
  形式 : string-capitalize string &opt start end
string の start と end の範囲の単語の先頭文字を大文字にして返す。
start,end の既定値は、それぞれ 0 、string の長さ。

<例>
        (string-capitalize \"abc def-ghi\")
                        -> \"Abc Def-Ghi\"
        (string-capitalize \"@#$%&<+=/;abcd\")
                        -> \"@#$%&+=/;Abcd\"
        (string-capitalize \"I am a programmer\" 0 5)
                        -> \"I Am a programmer\""
  (cl:string-capitalize string :start start :end end))

(defsynonym common:string-capitalize cl:string-capitalize
  #.(string '#:|common:string-capitalize               関数[#!macro]

<説明>
  形式 : common:string-capitalize string &key :start :end
string の :start と :end の範囲の単語の先頭文字を大文字にして返す。
:start,:end の既定値は、それぞれ 0 、string の長さ。

<例>
        (common:string-capitalize "hello") -> "Hello"
        (common:string-capitalize "hello" :start 2) -> "heLlo"
        (common:string-capitalize "this book is nice" :start 5 :end 8)
                       -> "this Book is nice"|))

(defun tao:string-char-p (char)
  "string-char-p                          関数[#!subr]

<説明>
  形式 : string-char-p char
文字 char が文字列に格納できれば char を返し、それ以外なら nil を返す。

<例>
        (string-char-p #¥a) -> \"a\"
        (string-char-p #¥Backspace) -> #¥Backspace"
  (cond ((eq #\Space char) #\Space)
	((graphic-char-p char) (string char))
	((typep char 'character) char)
	('T nil)))

(defun string-compare-* (object1 object2 elt= elt< elt>)
  (let ((x (->string object1))
	(y (->string object2))
	i)
    (cond ((funcall elt= x y) 0)
	  ((setq i (funcall elt< x y)) (1+ i))
	  ((setq i (funcall elt> x y)) (- (1+ i)))
	  ('T nil))))

(defun tao:string-compare (object1 object2)
  "string-compare                         関数[#!subr]

<説明>
  形式 : string-compare object1 object2
object1 と object2 (文字列又はアトム) を比較し、その結果に従って次の
処理を行なう。
  (1)  2 つの引数の値が sequal で t が得られる場合、0 を返す。
  (2)  object1 の方が辞書的に object2 より大きく、最初の (n-1) 文字
       が object1 と object2 で同じ場合、n を返す。
  (3)  object2 の方が辞書的に object1 より大きく、最初の (n-1) 文字
       が object1 と object2 で同じ場合、-n を返す。
文字列内の文字位置を 1 から数える (1-origin のインデックス)。

<例>
        (string-compare \"foo\" \"foo\") -> 0
        (string-compare \"abcdfghi\" \"abcdefghi\") -> 5
        (string-compare \"abcdefghi\" \"abcdfghi\") -> -5
        (string-compare \"abcdefg\" \"ABCdefg\") -> 0
        (string-compare \"あいうそかき\" \"あいうえお\") -> 6 ?"
  (string-compare-* object1 object2
		    #'string-equal #'string-greaterp #'string-lessp))

(defun tao:string-compare-case (object1 object2)
  "string-compare-case                    関数[#!subr]

<説明>
  形式 : string-compare-case object1 object2
関数 string-compare と同じ動作をするが、大文字と小文字を区別する。
object1 と object2 (文字列又はアトム) を比較し、その結果に従って次の
処理を行なう。
  (1)  2 つの引数の値が sequal で t が得られる場合、0 を返す。
  (2)  object1 の方が辞書的に object2 より大きく、最初の (n-1) 文字
       が object1 と object2 で同じ場合、n を返す。
  (3)  object2 の方が辞書的に object1 より大きく、最初の (n-1) 文字
       が object1 と object2 で同じ場合、-n を返す。
文字列内の文字位置を 1 から数える (1-origin のインデックス)。

<例>
        (!x \"aBCDef\")
        (!y \"abCdEF\")
        (string-compare-case x y) -> -2
        (!x \"a b c\")
        (!y \"a B\")
        (string-compre-case x y) -> 3
        (!x \"あいうえお\")
        (!y \"アイウエオカキ\")
        (string-compare-case x y) -> -1"
  (string-compare-* object1 object2
		    #'string= #'string> #'string<))

;;; string-downcase                        関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-downcase string &opt start end
;;; string において、start と end の範囲の大文字をすべて対応する小文字に
;;; 変換し、その結果を返す。start,end の既定値は、それぞれ 0 、string の
;;; 長さ。
;;;
;;; <例>
;;;         (string-downcase "ABCDEF" 2 4)
;;;                       -> "ABcdEF"
;;;         (string-downcase "ABCDEF")
;;;                       -> "abcdef"
;;;         (string-downcase "I LIKE ORANGES" 2 6)
;;;                       -> "I like ORANGES"

;;; common:string-downcase                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-downcase string &key :start :end
;;; string の、:start と :end の文字列内の大文字を小文字に変換し、
;;; その結果を返す。:start,:end の既定値は、それぞれ 0 、string の長さ。
;;;
;;; <例>
;;;         (common:string-downcase "ABCDEF" :start 2)
;;;                              -> "ABcdef"
;;;         (common:string-downcase "ABCDEF")
;;;                              -> "abcdef"
;;;         (common:string-downcase "ABCDEF" :end 4)
;;;                              -> "abcdEF"
;;;         (common:string-downcase "I LIKE APPLES" :start 2 :end 5)
;;;                              -> "I like APPLES"

;;; string-equal                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-equal string1 string2
;;; string1 と string2 を比較し、一致していれば string1 を返し、
;;; そうでなければ nil を返す。大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (string-equal "abc" "xyz") -> nil
;;;         (string-equal "abc" "abc") -> "abc"
;;;         (string-equal "はる" "はる") -> "はる"
;;;         (string-equal "さくら" "さく") -> nil

;;; common:string-equal                    関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-equal string1 string2
;;;         	     &key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、一致していれば string1 を返し、
;;; そうでなければ nil を返す。大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (!x "a")
;;;         (!y "a")
;;;         (common:string-equal x y) -> "a"
;;;         (common:string-equal "ab" "a") -> nil
;;;         (!x "あいうえお")
;;;         (!y "アイウエオ")
;;;         (common:string-equal x y) -> nil

(defun tao:string-fill (string character &optional (start 0) end)
  "string-fill                            関数[#!subr]

<説明>
  形式 : string-fill string character &opt start end
string において、start と end の範囲の全ての文字を、character に変更
し、その結果を返す。 start, end の既定値は、それぞれ 0 、string の長さ。

<例>
        (string-fill \"abcdefghij\" \"x\" 2 4)
                  -> \"abxxefghij\"
        (string-fill \"abcdefghij\" \"x\")
                  -> \"xxxxxxxxxx\"
        (string-fill \"abcdefghij\" \"x\" 12 14)
                  -> \"abcdefghij\"
        (string-fill \"さしすせそ\" \"す\" 2 4)
                   ->\"さしすすそ\""
  (let ((len1 (cl:length string)))
    (if (< len1 start) ;範囲を越えた場合(> start end)というケースがあるのかは不明
	string
	(let ((fill-string (tao:make-string len1 character)))
	  (replace string fill-string :start1 start :end1 end)))))

;;; common:string-fill                     関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-fill string character &key :start :end
;;; string において、start と end の範囲の全ての文字を、character に変更
;;; し、その結果を返す。:start,:end の既定値は、それぞれ 0 、string の長さ。
;;;
;;; <例>
;;;         (common:string-fill "abcdefg" 'c :start 1 :end 4)
;;;                          -> "acccefg"
;;;         (common:string-fill "かきくけこ" 'さ )
;;;                          -> "さささささ"
;;; ＠
;;;

(defun tao:string-fill-pointer (string)
  "string-fill-pointer                    関数[#!subr]

<説明>
  形式 : string-fill-pointer string
フィルポインタ付きのストリング string のフィルポインタの値を返す。

<例>
        (!y (make-string-with-fill-pointer 10 \"a\" 20))
            -> \"aaaaaaaaaa\"
        (string-fill-pointer y) -> 10
        (!(string-fill-pointer y) 15) -> 15
        y -> \"aaaaaaaaaaaaaaa\"
        (!(string-fill-pointer y) 30) -> エラー"
  (etypecase string
    (simple-string nil)
    (string (fill-pointer string))))



(declaim (inline tao:string-greater-or-equal))
(defun tao:string-greater-or-equal (string1 string2)
  "string-greater-or-equal                関数[#!subr]

<説明>
  形式 : string-greater-or-equal string1 string2
string1 と string2 の値を比較し、辞書的に等しいか大きければ、string2
の値を返し、そうでなければ nil を返す。大文字と小文字の相違は無視する。
<例>
        (string-greater-or-equal \"ab \" \"a\") -> \"a\"
        (string-greater-or-equal \"ab\" \"b\") -> nil
        (string-greater-or-equal \"ab\" \"abcde\") -> nil
        (string-greater-or-equal \"ab\" \"ab\") -> \"ab\"
        (string-greater-or-equal \"さしす\" \"すせそ\") -> nil"
  (or (string-greaterp string1 string2)
      (string-equal string1 string2)))

;;; string-greaterp                        関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-greaterp string1 string2
;;; string1 の値が、string2 の値より辞書順的に大きい場合、string2 の値を
;;; 返し、そうでなければ nil を返す。大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (string-greaterp "story" 'store) -> "store"
;;;         (string-greaterp "かたかな" 'ひらがな) -> nil
;;;         (string-greaterp "カタカナ" "ひらがな") -> "ひらがな"

;;; common:string-greaterp                 関数[#!macro]
;;; <説明>
;;;   形式 : common:string-greaterp string1 string2
;;;    		 &key :start1 :end1 :start2 :end2
;;; string1 が string2 より辞書順的に大きい場合、その一致しない最初の文字
;;; 位置を返し、そうでなければ nil を返す。大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (common:string-greaterp "123" "4") -> nil
;;;         (common:string-greaterp "cd" "a") -> 0
;;;         (common:string-greaterp "a" "a") -> nil
;;;         (common:string-greaterp "アイウ" "あいう") -> 0
;;;         (common:string-greaterp "aBcd" "abc") -> 3

;;; string-input-stream                    クラス
;;;
;;; <説明>
;;;   インスタンスが文字列入力ストリームであるクラス。
;;; 文字列データは、このストリームからとられる。

;;; string-left-trim                       関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-left-trim string1 string2
;;; string2 の文字列を左から右へ調べて行き、string1 の中のいずれかの文字
;;; と等しい文字を捜す。等しい文字があれば、その文字を string2 から抜き取り、
;;; さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
;;; なった文字列を返す。
;;;
;;; <例>
;;;         (string-left-trim "abc" "acbcaxyabcpqbcaba")
;;;                              -> "xyabcpqbcaba"
;;;         (string-left-trim "abc" "abcabcabcxabcyabccabca")
;;;                              -> "xabcyabccabca"
;;;         (!r "カタカナとひらがな") -> "カタカナとひらがな"
;;;         (string-left-trim "カ" r) -> "タカナとひらがな"
;;;         r -> "カタカナとひらがな"
;;;         (string-left-trim "か" "かかかかきくけこ") -> "きくけこ"

;;; common:string-left-trim                関数[#!expr]
;;;
;;; <説明>
;;;  形式 : common:string-left-trim char-bag string
;;; string の文字列を左から右へ調べて行き、char-bag の中のいずれかの文字と
;;; 等しい文字を捜す。等しい文字があれば、その文字を string から抜き取り、
;;; さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
;;; なった文字列を返す。 char-bag はストリング、文字のリスト、アトムの
;;; リストのどれかでよい。
;;;
;;; <例>
;;;         (!p "acbcaxyabcpqbcaba") -> "acbcaxyabcpqbcaba"
;;;         (common:string-left-trim "abc" p) -> "xyabcpqbcaba"
;;;         p -> "acbcaxyabcpqbcaba"
;;;         (!q "abcabcabcxabcyabccabca") -> "abcabcabcxabcyabccabca"
;;;         (common:string-left-trim "abc" q) -> "xabcyabccabca"
;;;         (!r "カタカナとひらがな") -> "カタカナとひらがな"
;;;         (common:string-left-trim "カ" r) -> "タカナとひらがな"
;;;         r -> "カタカナとひらがな"

(defun tao:string-length (string)
  "string-length                          関数[#!subr]

<説明>
  形式 : string-length string
string の文字列の長さを返す。

<例>
        (string-length \"\") -> 0
        (string-length '|abc|) -> 3
        (string-length \"abcdefghijkl\") -> 12
        (string-length \"今日は\") -> 3"
  (etypecase string
    (string (tao:slength string))
    (symbol (tao:string-length (symbol-name string)))))

(declaim (inline tao:string-less-or-equal))
(defun tao:string-less-or-equal (string1 string2)
  "string-less-or-equal                   関数[#!subr]

<説明>
  形式 : string-less-or-equal string1 string2
string1 とstring2 を比較し、string1 のほうが辞書順的に小さいか等しい
場合、string2 の値を返し、そうでなければ nil を返す。
大文字と小文字の相違は無視する。

<例>
        (string-less-or-equal \"2\" \"1\") -> nil
        (string-less-or-equal \"2\" \"2\") -> \"2\"
        (string-less-or-equal \"abc\" \"def\") -> \"def\"
        (string-less-or-equal \"あかさ\" \"はまや\") -> \"はまや\"
        (string-less-or-equal \"アカサ\"  \"あかさ\") -> nil"
  (or (string-equal string1 string2)
      (string-lessp string1 string2)))

;;; string-lessp                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-lessp string1 string2
;;; string1 と string2 を比較し、string1 のほうが、string2 の値より辞書順
;;; 的に小さい場合は、string2 の値を返し、そうでなければ nil を返す。
;;; 大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (string-lessp "abacus" "abdomen") -> "abdomen"
;;;         (string-lessp "abcdef" "abcde") -> nil
;;;         (string-lessp "abcdef" "abcdefg") -> "abcdefg"
;;;         (string-lessp "かたかな" 'ひらがな) -> "ひらがな"

;;; common:string-lessp                    関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-lessp stringa string2
;;;         	 &opt :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、string1 の方が辞書順的に小さいとき、その
;;; 一致しない最初の文字位置を返し、そうでなければ nil を返す。
;;; 大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (common:string-lessp "a" "A") -> nil
;;;         (common:string-lessp "A" "b") -> 0
;;;         (common:string-lessp "b" "a") -> nil
;;;         (common:string-lessp "ab" "da") -> 0
;;;         (common:string-lessp "かたかな" "かたみち") -> 2

;;; string-not-equal                       関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-not-equal string1 string2
;;; string1 と string2 の値を比較し、辞書順的に一致しなかった場合、
;;; string2 の値を返し、そうでなければ nil を返す。大文字と小文字の相違は
;;; 無視する。
;;;
;;; <例>
;;;         (string-not-equal "b" "B") -> nil
;;;         (string-not-equal "a" "b") -> "b"
;;;         (string-not-equal "b" "a") -> "a"
;;;         (string-not-equal "a" "a") -> nil
;;;         (string-not-equal "こんちは" "コンチハ") -> "コンチハ"

;;; common:string-not-equal                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-not-equal string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、辞書順的に一致しなかった場合、一致しない
;;; 最初の文字位置を返し、そうでなければ nil を返す。
;;; 大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (common:string-not-equal "a" "a") -> nil
;;;         (common:string-not-equal "a" "b") -> 0
;;;         (common:string-not-equal "A" "a") -> nil
;;;         (common:string-not-equal "かさ" "かみ") -> 1

;;; string-not-greaterp                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : string-not-greaterp string1 string2
;;; string1 と string2 を比較し、string1 のほうが辞書順的に大きくなかった
;;; 場合、string2 を返し、そうでなければ nil を返す。大文字と小文字の相違
;;; は無視する。
;;;
;;; <例>
;;;         (string-not-greaterp "a" "A") -> "A"
;;;         (string-not-greaterp "a" "b") -> "b"
;;;         (string-not-greaterp "c" "b") -> nil
;;;         (!x "ab")
;;;         (!y "cd")
;;;         (string-not-greaterp x y) -> "cd"
;;;         (string-not-greaterp "あい" "かき") -> "かき"

;;; common:string-not-greaterp             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-not-greaterp string1 string2
;;;         	&key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、string1 の方が辞書順的に大きくなかった
;;; 場合、一致しなかった最初の文字位置を返し、そうでなければ nil を返す。
;;; 大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (common:string-not-greaterp "a" "a") -> 0
;;;         (common:string-not-greaterp "a" "b") -> 0
;;;         (common:string-not-greaterp "b" "a") -> nil
;;;         (!x "ab")
;;;         (!y "cd")
;;;         (common:string-not-greaterp x  y) -> 0
;;;         (common:string-not-greaterp "かきくけこ"  "かきくけけ") -> 4

;;; string-not-lessp                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : string-not-lessp string1 string2
;;; string1 と string2 を比較し、string1 のほうが辞書順的に小さくなかった
;;; 場合、string2 を返し、そうでなければ nil を返す。大文字と小文字の相違
;;; は無視する。
;;;
;;; <例>
;;;         (string-lessp "a" "A") -> nil
;;;         (string-lessp "a" "a") -> nil
;;;         (string-lessp "b" "a") -> nil
;;;         (string-lessp "a" "b") -> "b"
;;;         (string-lessp "はな" "はし") -> "はし"

;;; common:string-not-lessp                関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-not-lessp string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、string1 の方が辞書順的に小さくなかった場合、
;;; 一致しなかった最初の文字位置を返し、そうでなければ nil を返す。
;;; 大文字と小文字の相違は無視する。
;;;
;;; <例>
;;;         (common:string-not-lessp "a" "b") -> nil
;;;         (common:string-not-lessp "b" "a") -> 0
;;;         (common:string-not-lessp "a" "a") -> 0
;;;         (common:string-not-lessp "a" "A") -> 0
;;;         (common:string-not-lessp "cd" "ab") -> 0
;;;         (common:string-not-lessp "あか" "あお") -> 1

;;; string-output-stream                   クラス
;;;
;;; <説明>
;;;   インスタンスが文字列出力ストリームであるクラス。
;;; 文字列データは、出力用としてこのストリームに送られる。

;(replace "abcdefghij" "foo" :start1 3 :end1 6 :start2 0)

(defun tao:string-replace (string1 string2 &optional (start 0) end)
  "string-replace                         関数[#!expr]

<説明>
  形式 : string-replace string1 string2 &opt start end sw
string1 の start 番目から (end-1) 番目までの文字を、string2 で置き換え
る。 start の既定値は 0、end の既定値は string1 の長さ。

<例>
        (!str1 \"abcdefghij\") -> \"abcdefghij\"
        (string-replace str1 \"pqrst\" 3 6) -> \"abcpqrghij\"
        str1 -> \"abcpqrghik\"
        (string-replace str1 \"klmno\" 7 14) -> \"abcpqrgklm\"
        str1 -> \"abcpqrgklm\"
        (string-replace \"あいうえお\" \"か\" 1 3) -> \"あかうえお\""
  (mapc (lambda (x) (unless (stringp x)
		      (error "value ~S is not of the expected type STRING" x)))
	`(,string1 ,string2))
  (cl:replace string1 string2 :start1 start :end1 end))

(defun tao:string-reverse (string)
  "string-reverse                         関数[#!subr]

<説明>
  形式 : string-reverse string
string を逆順に並び換え、そのコピーを返す。

<例>
        (string-reverse \"edit\") -> \"tide\"
        (!x \"今日は晴れだ\") -> \"今日は晴れだ\"
        (string-reverse x) -> \"だれ晴は日今\"
        (string-reverse x) -> \"今日は晴れだ\""
  (etypecase string
    (string (tao:sreverse string))
    (symbol (tao:string-reverse (symbol-name string)))))


(defun string-reverse-search-* (string1 string2 n test-fn)
  (let ((str1 (->string string1))
	(str2 (->string string2)))
    (let* ((len1 (cl:length str1))
	   (len2 (cl:length str2))
	   (start2 (if n (- (1- len2) n) 0)))
      (let ((res (search (reverse str1) (reverse str2) :start2 start2 :test test-fn)))
	(when res
	  (- len2 len1 res))))))

(defun tao:string-reverse-search (string1 string2 &optional n)
  "string-reverse-search                  関数[#!subr]

<説明>
  形式 : string-reverse-search string1 string2 &opt n
文字列パターン string1 が、string2 内にあるかを、 n 番目の文字位置から
前の方に逆順に走査し、含まれていた場合には、string1 の最初の文字に相当
する文字が string2 の何番目にあるかを返す。なかった場合はnil を返す。
n が省略された場合には、string2 の文字列の一番最後の文字から検索する
\(n の既定値は string2 の長さ)。文字列内の文字の位置は 0 から数える。

<例>
        (string-reverse-search \"a\" \"ABCDE\") -> 0
        (string-reverse-search \"fgh\" \"abcdefghijkl\" 9) -> 5
        (string-reverse-search \"an\" \"banana\") -> 3
        (string-reverse-search \"an\" \"banana\" 2) -> 1
        (string-reverse-search \"an\" 'foobar) -> nil
        (string-reverse-search \"さ\" \"さくらさく\") -> 3
        (string-reverse-search \"ア\" \"アイウエオ\") -> nil ? 0じゃないの？"
  (string-reverse-search-* string1 string2 n #'string-equal))

(defun tao:string-reverse-search-case (string1 string2 &optional n)
  "string-reverse-search-case             関数[#!subr]

<説明>
  形式 : string-reverse-search-case string1 string2 &opt n
文字列パターン string1 が string2 内にあるかを、n 番目の文字位置から
前の方に逆順に検索し、含まれていた場合には、発見された部分文字列の先頭
文字の string2 内の文字位置を返し、なかった場合は nil を返す。number が
省略された場合には、string2 の一番最後の文字から検索する(number の既定
値は string2 の長さ)。文字列内の文字の位置は 0 から数える。

<例>
        (string-reverse-search-case \"fGh\" \"abcdefghijkl\") -> nil
        (string-reverse-search-case \"fGh\" \"abcdefghijkl\" 9) -> nil
        (string-reverse-search-case \"a\" \"ABCDE\") -> nil
        (string-reverse-search-case \"あ\" \"あいうえお\") -> 0"
  (string-reverse-search-* string1 string2 n #'string=))

;;; string-right-trim                      関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-right-trim string1 string2
;;; string2 の文字列を右から左へ調べて行き、string1 の中のいずれかの文字
;;; と等しい文字を捜す。等しい文字があれば、その文字を string2 から抜き取り、
;;; さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
;;; なった文字列を返す。
;;;
;;; <例>
;;;         (string-right-trim "abc" "acbcaxyabcpqbcaba")
;;;                               -> "acbcaxyabcpq"
;;;         (string-right-trim "あいう" "あいうえああああいいい")
;;;                                  -> "あいうえ"
;;;         (!r "あいうえあああいいい") -> "あいうえあああいいい"
;;;         (string-right-trim "あいう" r) -> "あいうえ"
;;;         r -> "あいうえあああいいい"
;;;         (string-right-trim "abc" "abcabcabcabbb") -> ""

;;; common:string-right-trim               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:string-right-trim char-bag string
;;; string の文字列を右から左へ調べて行き、char-bag の中のいずれかの文字
;;; と等しい文字を捜す。等しい文字があれば、その文字を string から抜き取り、
;;; さらに検索を進める。なければそこで検索を終える。そして、抜き取られ短く
;;; なった文字列を返す。 char-bag はストリング、文字のリスト、アトムのリス
;;; トでよい。
;;;
;;; <例>
;;;         (common:string-right-trim "cd" "abcdefg") -> "abcdefg"
;;;         (common:string-right-trim "fg" "abcdefg") -> "abcde"
;;;         (!x "たちつたたれたたた") -> "たちつたたれたたた"
;;;         (common:string-right-trim "た" x) -> "たちつたたれ"
;;;         x -> "たちつたたれたたた"

#|(defun string-search-* (string1 string2 number elt=)
  (declare (number number)
           (function elt=))
  (let ((s1 (->string string1))
	(s2 (->string string2)))
    (let ((len1 (cl:length s1)))
      (do ((s (subseq s2 number) (subseq s 1))
	   (cnt 0 (1+ cnt)))
	  ((equal "" s))
        (let ((mm (mismatch s1 s :test elt=)))
          (when (null mm) (return 0))
          (or (zerop mm)
              (return (and (< len1 (cl:length s))
                           (+ cnt number)))))))))|#


(defun string-search-* (string1 string2 number elt=)
  (declare (number number)
           (function elt=))
  (let ((s1 (->string string1))
	(s2 (->string string2)))
    (declare (simple-string s1 s2))
    (cl:search s1 s2 :start2 number :test elt=)))


(defun ->string (obj)
  (typecase obj
    (string obj)
    (character (string obj))
    (number (princ-to-string obj))
    (atom (string-downcase (string obj)))
    (otherwise (error "~S is not of type ATOM." obj))))

(defun tao:string-search (string1 string2 &optional (number 0))
  "string-search                          関数[#!subr]

<説明>
  形式 : string-search string1 string2  &opt number
string2 を先頭から順番に、string1 のパターンが含まれているかどうかを
調べる。検索は string2 の number 番目の文字から始める。number の
既定値は 0。string1 を string2 上に重ねて、先頭から右にずらしながら
調べて行き、最初にパターンがマッチしたところで調べるのをやめ、その時の
string1 の先頭の文字が string2 の中で何文字目になっているのかを返す。
マッチしなかった場合は、nil を返す。文字列内の文字位置は 0 から数える。
大文字、小文字の相違は無視される。

<例>
        (string-search \"fgh\" \"abcdefghijkl\" 3) -> 5
        (string-search \"an\" \"BANANA\") -> 1
        (string-search \"an\" \"banana\" 2) -> 3
        (string-search \"an\" 'foobar) -> nil
        (string-search \"あか\" \"しろあかき\") -> 2
        (string-search \"あか\" \"あかしろきいろ\" 3) -> nil"
  (string-search-* string1 string2 number #'char-equal))

(defun tao:string-search-case (string1 string2 &optional (number 0))
  "string-search-case                     関数[#!subr]

<説明>
  形式 : string-search-case string1 string2 &opt number
string2 を先頭から順番に、string1 のパターンが含まれているかどうかを
調べる。検索は string2 の number 番目の文字から始め、number の既定値は
0。string1 を string2 上に重ね、先頭から右にずらしながら調べて行き、
最初にパターンがマッチしたところで調べるのをやめ、その時の string1 の
先頭の文字が string2 の中で何文字目になっているのかを返す。マッチしな
かった場合、nil を返す。文字列内の文字位置は 0 から数える。
大文字と小文字を区別する

<例>
        (string-search-case \"fgh\" \"abcdefghijkl\" 3) -> 5
        (string-search-case \"Fgh\" \"abcdefghijkl\") -> nil
        (string-search-case \"B\" \"abcABC\") -> 4
        (string-search-case \"あいう\" \"うえおいあ\") -> nil"
  (string-search-* string1 string2 number #'equal))

(defun tao:string-trim (string1 string2)
  #.(string '#:|string-trim                            関数[#!subr]

<説明>
  形式 : string-trim string1 string2
まず最初に、string2 の文字列を左から右へ調べて行き、string1 の中のい
ずれかの文字と等しい文字を捜す。等しい文字があれば、string-trim はそ
の文字を string2 から抜き取り、さらに検索を進める。なければそこで検索
を終える。次に string1 の中の文字と等しい文字が string2 にあるかどうか
を、今度は string2 の右から左へ調べて行く。この検索も最初と同じ方法で
進められる。最後にこの 2 つのステップで作られた新しい文字列を返す。

<例>
        (string-trim "abc" "acbcaxyabcpqbcaba") -> "xyabcpq"
        (!r "あういけいあうかああいあう")
         -> "あういけいあうかああいあう"
        (string-trim "あいう" r) -> "けいあうか"
        r -> "あういけいあうかああいあう"|)
  (declare (string string1 string2))
  (cl:string-trim string1 string2))

(defsynonym common:string-trim cl:string-trim
  #.(string '#:|common:string-trim                     関数[#!expr]

<説明>
  形式 : common:string-trim char-bag string
まず最初に、string の文字列を左から右へ調べて行き、char-bag の中のい
ずれかの文字と等しい文字を捜す。等しい文字があれば、その文字を string
から抜き取り、さらに検索を進める。なければそこで検索を終える。次に
char-bag の中の文字と等しい文字が string にあるかどうかを、今度は
string の右から左へ調べて行く。この検索も最初と同じ方法で進められる。
最後にこの 2 つのステップで作られた新しい文字列を返す。
char-bag はストリング、文字のリスト、アトムのリストのどれか。

<例>
        (common:string-trim "cd" "abcdefg") -> "abcdefg"
        (common:string-trim "fg" "abcdefg") -> "abcde"
        (!y "aabbccdeccbbaa") -> "aabbccdeccbbaa"
        (common:string-trim "abc" y) -> "de"
        y -> "aabbccdeccbbaa"
        (common:string-trim "あいう" "あかういういあ") -> "か"|))

;;; string-upcase                          関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string-upcase string &opt start end
;;; string において、start と end の範囲の小文字をすべて大文字に変換し、
;;; その結果を返す。start,end の既定値は、それぞれ 0 、string の長さ。
;;;
;;; <例>
;;;         (string-upcase "abcd") -> "ABCD"
;;;         (string-upcase "abcd" 2 3) -> "abCd"
;;;         (string-upcase "abcd" 2) -> "abCD"
;;;         (string-upcase "i am a programmer" 0 1) -> "I am a programmer"

;;; common:string-upcase                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string-upcase string &key :start :end
;;; :start と :end の間の string 内の小文字を大文字に変換し、その結果を返す。
;;; :start,:end の既定値は 0、string の長さ。
;;;
;;; <例>
;;;         (common:string-upcase "abcdefg" :start 3 :end 4) -> "abcDefg"
;;;         (common:string-upcase "abcdefg") -> "ABCDEFG"
;;;         (common:string-upcase "aDcdeHfg" :end 4) -> "ADCDeHfg"

;;; string/=                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string/= string1 string2
;;; string1 と string2 を辞書順的に比較し、一致しなかった場合、string2 を
;;; し、そうでなければ nilを返す。大文字と小文字の相違を区別する。
;;;
;;; <例>
;;;         (string/= "A" "a") -> "a"
;;;         (string-not-equal "A" "a") -> nil
;;;         (string-not-equal "はる" "あき") -> "あき"

;;; common:string/=                        関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string/= string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を辞書順的に比較し、一致しなかった場合、一致しない
;;; 最初の文字位置を返し、それ以外は nil を返す。
;;; 大文字と小文字の相違を区別する。
;;;
;;; <例>
;;;         (common:string/= "abc" "bcdef") -> 0
;;;         (common:string/= "abc" "abc") -> nil
;;;         (!x "ab")
;;;         (!y "ac")
;;;         (common:string/= y x) -> 1
;;;         (common:string/= x x) -> nil
;;;         (common:string/= "さくら" "さく") -> 2

;;; string<                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string< string1 string2
;;; string1 と string2 を比較し、string1 のほうが、string2 より辞書順的に
;;; 小さい場合は、string2 の値を返し、そうでなければ nil を返す。大文字と
;;; 小文字の相違を区別する。
;;;
;;; <例>
;;;         (string< "a" "B") -> nil
;;;         (string-lessp "a" "B") ->"B"
;;;         (string< "はひふ" "まみむ") -> "まみむ"

;;; common:string<                         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string< string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を辞書順的に比較し、string1 が string2 よりも小さい
;;; 場合はその一致しない最初の文字位置を返し、そうでない場合は nil を返す。
;;; 大文字と小文字の相違を区別する。
;;;
;;; <例>
;;;         (common:string< "abc" "abc") -> nil
;;;         (common:string< "abc" "defg") -> 0
;;;         (common:string< "defg" "abc") -> nil
;;;         (common:string< "abc" "abd") -> 2
;;;         (common:string< "はひふ" "まみむ") ->  0

;;; string<=                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string<= string1 string2
;;; string1 と string2 を比較し、string1 が、string2 より辞書順的に小さい
;;; か等しい場合、string2 を返し、そうでなければ nil を返す。大文字と小文
;;; 字の相違を区別する。
;;;
;;; <例>
;;;         (string<= "a" "B") -> nil
;;;         (string-not-greaterp "a" "B") -> "B"
;;;         (string<= "あいう" "アイウ") -> "アイウ"
;;;         (string<= "かきく" "さしす") -> "さしす"

;;; common:string<=                        関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string<= string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; satring1 と string2 の値を辞書順的に比較し、string1 の文字が string2
;;; の文字より小さいか等しい場合は、そのときの最初の文字位置を返し、それ
;;; 以外は nil を返す。大文字と小文字の相違を区別する。
;;;
;;; <例>
;;;         (common:string<= "ab" "ab") -> 0
;;;         (common:string<= "ab" "cde") -> 0
;;;         (common:string<= "cde" "ab") -> nil
;;;         (common:string<= "あいう" "アイウ") -> 0

;;; string=                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string= string1 string2
;;; string1 と string2 を比較し、string1 が、string2 に辞書順的に一致した
;;; 場合は、string2 を返し、そうでなければ nil を返す。大文字と小文字の
;;; 相違を区別する。
;;;
;;; <例>
;;;         (string= "a" "A") -> nil
;;;         (string-equal "a" "A") -> "A"
;;;         (string= "abc" 'abc) -> "abc"
;;;         (string= 'abc 'AbC) -> "abc"
;;;         (string= "あい" "あイ") -> nil
;;;         (string= 'あい "あい") -> "あい"

;;; common:string=                         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string= string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を辞書順的に比較し、等しい場合、string1 を返し、
;;; それ以外は nil を返す。大文字と小文字の相違は区別する。
;;;
;;; <例>
;;;         (common:string= "ab" "ab") -> "ab"
;;;         (common:string= "ab" "cd") -> nil
;;;         (common:string= "cd" "ab") -> nil
;;;         (common:string= "あい" "アイ") -> nil

;;; string>                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string> string1 string2
;;; string1 と string2 を比較し、string1 が、string2 より辞書順的に大きい
;;; 場合、string2 を返し、そうでなければ nil を返す。大文字と小文字の相違
;;; を区別する。
;;;
;;; <例>
;;;         (string> "B" "a") -> nil
;;;         (string-greaterp "B" "a") ->"a"
;;;         (string> "あ" "ぁ") -> "ぁ"
;;;         (string> "あさ" "あめ") -> nil

;;; common:string>                         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string> string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、string1 が string2 より辞書順的に大きい
;;; 場合、その一致しない最初の文字位置を返し、それ以外の場合は nil を返す。
;;; 大文字と小文字の相違は区別する。
;;;
;;; <例>
;;;         (common:string> "ab" "ab") -> nil
;;;         (common:string> "cd" "ab") -> 0
;;;         (common:string> "かき" "カキ") -> nil
;;;         (common:string> "あし" "あさ") -> 1

;;; string>=                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : string>= string1 string2
;;; string1 と string2 を比較し、string1 が、string2 より辞書順的に大きい
;;; か等しい場合、string2 を返し、そうでなければ nil を返す。大文字と小文
;;; 字の相違を区別する。
;;;
;;; <例>
;;;         (string>= "B" "a") -> nil
;;;         (string-not-lessp "B" "a") -> "a"
;;;         (string>= "さし" "かき") -> "かき"
;;;         (string>= "いつ" "ぃっ") -> "ぃっ"

;;; common:string>=                        関数[#!macro]
;;;
;;; <説明>
;;;   形式 : common:string>= string1 string2
;;;         	 &key :start1 :end1 :start2 :end2
;;; string1 と string2 を比較し、string1 の全ての文字が、string2 の文字より
;;; 辞書順的に大きいか等しい場合、一致しない最初の文字位置を返し、
;;; それ以外は nil を返す。大文字と小文字の相違は区別する。
;;;
;;; <例>
;;;         (common:string>= "ab" "ab") -> 0
;;;         (common:string>= "ab" "cd") -> nil
;;;         (common:string>= "cd" "ab") -> 0
;;;         (common:string>= "あ" "ぁ") -> 0
;;;         (common:string>= "かき" "くけ") -> nil

;;; stringp                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : stringp arg
;;; arg が文字列（文字列または１つの文字）なら、arg を返し、
;;; それ以外なら nil を返す。
;;;
;;; <例>
;;;         (stringp "") -> ""
;;;         (stringp "string") -> "string"
;;;         (stringp 'string) -> nil
;;;         (stringp 123) -> nil

;;; common:stringp                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : common:stringp &rest arg
;;; arg が文字列なら arg 、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (common:stringp "") -> nil
;;;         (common:stringp "a") -> nil
;;;         (common:stringp "string") -> "string"
;;;         (common:stringp 'string) -> nil
;;;         (common:stringp 123) -> nil

;;; sublis                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : sublis a-list tree &key :test :test-not :key
;;; 木構造 tree のオブジェクトのうち、連想リスト a-list のいずれかの car 部
;;; と一致するものを、その car 部に対応する cdr 部に変更して、変更した木を
;;; 返す (tree は破壊されない)。nsublis は破壊版。
;;;
;;; <例>
;;;         (sublis '((Kanto . Japan) (Japan . Osaka)) '(Tokyo is Kanto))
;;;         	 -> (Tokyo is Japan)

(declaim (inline tao:sublisq))
(defun tao:sublisq (a-list tree)
  "sublisq                                関数[#!subr]

<説明>
  形式 : sublisq a-list tree
tree 内のシンボルを、連想リスト a-list の cdr 部と置き換えることに
よって新しい木を作り、その結果を返す。置き換えは次のように実行される。
a-list のいずれかの要素の car 部 と eq であるようなシンボルが tree に
あったら、そのシンボルを、その car 部に対応する cdr 部で置き換える。
新しく作られる木はできるだけ多くの部分を古い木と共有するように作られる。
tree は、破壊されない。nsublisq は破壊版。

<例>
        (sublisq '((man . woman) (strong . weak))
        	 '(man is very strong))
        -> (woman is very weak)
        (sublisq '((1 . A) (11 . J) (12 . Q) (13 . K))
        	 '(1 12 13 2 12 1 13))
        -> (A Q K 2 Q A K)
        x = (a (b c) (d (e f)))  なら
          (eq (sublisq '((aa . 11) (bb . 22)) x) x) -> t"
  (sublis a-list tree :test #'eq))

(defun tao:sublisq-copy (a-list tree)
  "sublisq-copy                           関数[#!subr]

<説明>
  形式 : sublisq-copy a-list tree
tree 内のシンボルを、連想リスト a-list の cdr 部と置き換えることに
よって新しい木を作り、その結果を返す。置き換えは次のように実行される。
a-list のいずれかの要素の car 部 と eq であるようなシンボルが tree で
指定された木にあったら、そのシンボルを、その car 部に対応する cdr 部で
置き換える。返された木はいかなる部分も元の木と共有しない。

<例>
          x = (a (b c) (d (e f)))  なら
          (eq (sublisq-copy '((aa . 11) (bb . 22)) x) x) -> nil"
  (sublis a-list (copy-tree tree) :test #'eq))

(declaim (inline tao:sublisql))
(defun tao:sublisql (a-list tree)
  (sublis a-list tree :test #'eql))


(declaim (inline tao:subpackages))
(defun tao:subpackages (&optional (package *package*))
  "subpackages                            関数[#!expr]

<説明>
  形式 : subpackages &opt package
package に属するすべてのパッケージ名をリストにして返す。package の
既定値はカレントパッケージ。

<例>
        (package-name (subpackages)) -> nil
        (package-name (subpackages sys:univ-package))
                -> (\"apropos\" \"net\" \"step\" \"bas\" \"sys\" \"key\")"
  (package-used-by-list package))

;; TAOのpackage-nameはリストを取るらしい

;;; subseq                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : subseq seq start &opt end
;;; シーケンス seq の start から end までを返す。返される部分シーケンスの
;;; 型は元のシーケンスと同じ型。
;;;
;;; <例>
;;;         (subseq "abcdefg" 2) -> "cdefg"
;;;         (subseq "abcdefg" 1 4) -> "bcd"
;;;         (subseq '(a b c d e) 1 4) -> (b c d)
;;;         (subseq #(a b c d e f) 0 3)
;;;                 ->{vector}183855(common:simple-general-vector . 3)

(declaim (inline tao:subset))
(defun tao:subset (pred list)
  "subset                                 関数[#!macro]

<説明>
  形式 : subset pred list
list から、条件 pred を満足しない要素をすべて削除し、その結果の小さく
なったリストを返す ( list は破壊されない)。 pred は引数を 1 つだけとる。

<例>
        x = (1 a 2 b 3 c 4 d 5)
        (subset #'integerp x) -> (1 2 3 4 5)
        x -> (1 a 2 b 3 c 4 d 5)"
  (remove-if-not pred list))

(declaim (inline tao:subset-not))
(defun tao:subset-not (pred list)
  "subset-not                             関数[#!macro]

<説明>
  形式 : subset-not pred list
list から、条件 pred を満足しない要素をすべて削除し、その結果の小さく
なったリストを返す ( list は破壊されない)。pred は引数を 1 つだけとる。
  (subset-not x y) = (rem-if x y)

<例>
        x = (1 a 2 b 3 c 4 d 5)
        (subst-not #'integerp x) -> (a b c d)
        x -> (1 a 2 b 3 c 4 d 5)"
  (remove-if pred list))

(defsynonym tao:subsetp cl:subsetp
  "subsetp                                関数[#!macro]

<説明>
  形式 : subsetp list1 list2 &key :test :test-not :key
list1 のすべての要素が、list2 にある場合は t を返し、そうでなければ
nil を返す。

<例>
        (subset  '(a b)  '(a b c d))  ->  t
        (subset  '(a b)  '(c d))  ->  nil")

;;; subst                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : subst new old tree &key :test :test-not :key
;;; tree の全ての副木又は葉をコピーし、そのコピーした木の副木又は葉 old
;;; を new で書き換え、返す (非破壊的)。nsubst は破壊版。
;;;
;;; <例>
;;;         x = (shakespeare wrote (the hurricane))
;;;         (subst 'tempest 'harricane x) ->
;;;         	(shakespeare wrote (the tempest))
;;;         x -> (shakespeare wrote (the hurricane))

;;; subst-if                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : subst-if new test tree &key :key
;;; tree の副木又は葉をすべてコピーし、それらのうち、条件 test を満足する
;;; 副木又は葉を new で書き換え、その結果を返す。 tree は破壊されない。
;;; nsubst-if は破壊版。
;;;
;;; <例>
;;;         x = (("asd" . asd) (qwe . "qwe"))
;;;         (subst-if 'a #'stringp x ) -> ((a . asd) (qwe .a))
;;;         x -> (("asd" . asd) (qwe . "qwe"))

;;; subst-if-not                           関数[#!macro]
;;;
;;; <説明>
;;;   形式 : subst-if-not new test tree &key :key
;;; tree の副木又は葉をすべてコピーし、それらのうち、条件 test を満足しな
;;; い副木又は葉を new で書き換え (非破壊的)、その結果を返す。
;;; nsubst-if-not は破壊版。

;;; substitute                             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : substitute newitem olditem seq &key :from-end :test :test-not
;;;                                              :start :end :count :key
;;; シーケンス seq の :start から :end までの範囲で、olditem を :count 個だ
;;; け、newitem に仮に変更し、その結果を返す。nsubstitute は破壊版。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (substitute 9 4 x) -> (1 2 9 1 3 9 5)
;;;         (substitute 9 4 x :count 1 :from-end t) -> (1 2 4 1 3 9 5)
;;;         (substitute 9 3 x :test #'>) -> (9 9 4 9 3 4 5)
;;;         x -> (1 2 4 1 3 4 5)

;;; substitute-if                          関数[#!macro]
;;;
;;; <説明>
;;;   形式 : substitute-if newitem test seq &key :from-end :start :end
;;;                                             :count :key
;;; シーケンス seq の :start から :end までの範囲で、条件 test を満足する
;;; 要素を :count 個だけ、newitem に仮に変更し、その結果を返す。
;;; nsubstitute-if は破壊版。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (substitute-if 9 #'oddp x) -> (9 2 4 9 9 4 9)
;;;         (substitute-if 9 #'evenp x :count 1 :from-end t)
;;;                                    -> (1 2 4 1 3 9 5)
;;;         x -> (1 2 4 1 3 4 5)

;;; substitute-if-not                      関数[#!macro]
;;;
;;; <説明>
;;;   形式 : substitute-if-not newitem test seq &key :from-end :start :end
;;;                                                 :count :key
;;; シーケンス seq の :start から :end までの範囲で、条件 test を満足しない
;;; 要素を :count 個だけ、newitem に仮に変更し、その結果を返す。
;;; nsubstitute-if-not は破壊版。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (substitute-if-not 9 #'oddp x) -> (1 9 9 1 3 9 5)
;;;         (substitute-if-not 9 #'evenp x :count 1 :from-end t)
;;;                                        -> (1 2 4 1 3 4 9)
;;;         x -> (1 2 4 1 3 4 5)

(defun tao:substqu (new old tree)
  "substqu                                関数[#!subr]

<説明>
  形式 : substqu new old tree
tree 内の、old と equal である要素を、new と置き換え、その結果を返す。
tree をコピーして修正するので、この操作は非破壊的。nsubstqu は破壊版。

<例>
        x = (a b (bar . c) bar . bar)
        (substqu 'foo 'bar x) -> (a b (foo . c) foo . foo)
        x -> (a b (bar . c) bar . bar)"
  (subst new old tree :test #'equal))


(defun tao:substql (new old tree)
  (subst new old tree :test #'eql))


(defun string*-arg-check (string start end)
  (let ((string (typecase string
		  (string string)
		  (symbol (string string))
		  (otherwise (error "~S is not of type STRING designator." string)))))
    (let ((len (cl:length string)))
      (let ((start (etypecase start
                     ((integer 0 *) start)
                     ((integer * -1) (+ len start))))
	    (end (etypecase end
                   (null len)
                   ((integer 0 *) end)
                   ((integer * -1) (+ len end)))))
	(values string len start end)))))


(defun tao:substring (string start &optional end)
  "substring                              関数[#!subr]

<説明>
  形式 : substring string start &opt end
stirng の start 番目から (end-1) 番目までの文字からなる部分文字列を作り、
そのコピーを返す。返される文字列の長さは、end で指定された文字位置から
start で指定された文字位置をひいたもの。 end の省略時は string の終わり
までを返す。start と end は負の数でもよいが、この場合は逆インデックスと
なる。例えば \"abcd\" の中の a, b, c, d の位置を示す逆インデックスは各々
 -4, -3, -2, -1。このような処理をする場合は、nsubstring を使った方がよ
い。引数はストリングかアトムでなければならない。

<例>
        (substring \"abcd\" 2 3) -> \"c\"
        (substring \"abcd\" 2) -> \"cd\"
        (substring 'abcdefg 3 0) -> \"\"
        (substring \"私は女の子です。\" 4) -> \"子です。\"
        (substring \"私は女の子です。\" 3 6) -> \"の子で\""
  (multiple-value-bind (string len start end)
                       (string*-arg-check string start end)
    (declare (ignore len))
    (if (> start end)
	""
	(subseq string start end))))

;;; subtypep                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : subtypep type1 type2
;;; 値を 2 つ返す。最初の値は、type1 が type2 のサブタイプならば
;;; non-nil、そうでなければnil 。第 2 の値は最初に返された値の確実性を示す。
;;;
;;;         返す値
;;;         !(t t)          type1 は type2 のサブタイプ
;;;         !(t nil)        （未使用)
;;;         !(nil t)        type1 は type2 のサブタイプではない
;;;         !(nil nil)      type1 は type 2のサブタイプかどうかわからない
;;;
;;; <例>
;;;         (subtype 'fixnum 'integer) -> !(t t)
;;;         (subtype 'list 'string) -> !(nil t)
;;;         (deftype str-cha () '(or character (satisfies stringp)))
;;;         				 -> str-cha
;;;         (subtype character str-cha) -> !(t t)
;;;         (subtype string str-cha) -> !(nil nil)

;;; super                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : super 'super-message &rest args
;;; クラス A がメッセージ x を、クラス B (A のスーパクラス) が同名のメッセ
;;; ージ x を持つとする。A のメソッド内では、通常の x を送る方法によって B
;;; の x を送ることはできない。super は (super B.x) という式によって A のメ
;;; ソッド内で B のメッセージを送れるようにする。super はメソッド内でのみ使
;;; える。x を self (defmethod 参照) へ送り、これでスーパクラス B のメソッ
;;; ド x が実行される。super-message は B.x という形となる。
;;; super によって送られたメッセージでは、もしメソッド結合が B に記述されて
;;; いるならば、B やさらに B のスーパクラスのメソッドがメソッド結合により実
;;; 行される。
;;;
;;; <例>
;;;         (defclass a () ((aa 1)) () :gettable) -> a
;;;         (defclass b () ((bb 1)) (a) :gettable) -> b
;;;         (defmethod (a mult) () (!!* !aa 2)) -> mult
;;;         (defmethod (b mult) () (!!* !bb 3)) -> mult
;;;         (defmethod (b multi) () (super a.mult)) -> multi
;;;         (!ainst (make-instance 'a)) -> {udo}56789a
;;;         (!binst (make-instance 'b)) -> {udo}56819b
;;;         [ainst mult] -> 2
;;;         [ainst aa] -> 2
;;;         [binst mult] -> 3
;;;         [binst aa] -> 1
;;;         [binst bb] -> 3
;;;         [binst multi] -> 2
;;;         [binst aa] -> 2

;;; svref                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : svref vector integer
;;; 一般単純ベクタ vector の integer 番目の値を返す
;;; (要素の位置は 0 から数える)。
;;;
;;; <例>
;;;         (!v (vector 1 2 3 4 5))
;;;         	-> {vector}1844308(common:simple-general-vector . 5)
;;;         (svref v 2) -> 3
;;;         (setf (svref v 2) 6) -> 6
;;;         (show-vector v) ->
;;;         vtitle: common:simple-general-vector  vsize: 5
;;;             0 kdr: 1
;;;             1 kar: 2
;;;             2 kdr: 6
;;;             3 kar: 4
;;;             4 kdr: 5
;;;         {vector}1844308(common:simple-general-vector . 5)

;;; sxhash                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sxhash object
;;; object に対するハッシュコードを計算し、その結果を負でない固定小数点数
;;; (fixnum) として返す。
;;;
;;; <例>
;;;         (sxhash 'a) -> 97
;;;         (sxhash 'b) -> 98
;;;         (sxhash 'z) -> 122
;;;         (sxhash 'xyz) -> 856

;;; symbol-function                        関数[#!subr]
;;;
;;; <説明>
;;;   形式 : symbol-function symbol
;;; symbol のグローバル関数定義を返す。関数定義を持っていないならばエラー
;;; を示す。特に Lisp の中に埋め込まれる言語のインタプリタを作成するとき
;;; に便利。
;;;
;;; <例>
;;;         (symbol-function 'car) -> {applobj}24312(#!subr . 6)
;;;         (de ufo (x) (car x)) -> ufo
;;;         (symbol-function 'ufo) -> {applobj}39257(#!expr-simple . 6)
;;;         (defun foo (x) (cdr x)) -> foo
;;;         (symbol-function 'foo) -> {applobj}30288(#!exprdyn-simple . 6)
;;;         (symbol-function 'aho) -> nil

;;; symbol-name                            関数[#!macro]
;;;
;;; <説明>
;;;   形式 : symbol-name symbol
;;; symbol の印字名を返す。
;;; (symbol-name x) = (pname x)
;;;
;;; <例>
;;;         (symbol-name 'xyz) -> "xyz"
;;;         (symbol-name "abc") -> "abc"
;;;         (!a '(1 2 3)) -> (1 2 3)
;;;         (symbol-name a) -> "(1 2 3)"

;;; symbol-package                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : symbol-package symbol
;;; symbol が所属するパッケージ名を返す。
;;;
;;; <例>
;;;         (package-name (symbol-package 'car)) -> "bas"

;;; symbol-plist                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : symbol-plist id
;;; plist の別名関数。シンボル id の属性リストを返す。
;;;
;;; <例>
;;;         aa の属性リストを (p 1 q 2 r 3) とする
;;;         (symbol-plist 'aa) -> (p 1 q 2 r 3)
;;;         (!(symbol-plist 'aaa) '(a 1 b 2 c 3 d 4)) -> (a 1 b 2 c 3 d 4)
;;;         aaa の属性リストは (a 1 b 2 c 3 d 4)

;;; symbol-value                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : symbol-value var
;;; スペシャル変数 var の現在値を返す。
;;;
;;; <例>
;;;         (!x '(a b c))
;;;         (symbol-value 'x) -> (a b c)
;;;         (symbol-value 'y) -> (unbound-variable y)
;;;         (prog (p) (!p 3)
;;;                   (symbol-value 'p)) -> (unbound-variabl p)
;;;         (prog (p) (special-variables p)
;;;         	  (!p 23)
;;;                   (symbol-value 'p)) -> 23

;;; symbolp                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : symbolp object
;;; object がシンボルなら object を返し、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (symbolp 'asdf) -> asdf
;;;         (symbolp '_x) -> _x
;;;         (symbolp "asdf") -> nil
;;;         (symbolp #!expr) -> nil

(defsynonym tao:symeval cl:symbol-value
  "symeval                                関数[#!macro]

<説明>
  形式 : symeval symbol
symbol の最新の値を返す。値をもっていなければエラーを返す。

<例>
        (setq x '(a b c))
        (symeval 'x) -> (a b c)
        (symeval 'y) -> (unbound-variable y nil)")

;;; bas:sysmode        未インプリメント    関数[#!macro]
;;;
;;; <説明>
;;;   形式 : bas:sysmode &opt 'key 'num
;;;   key --> :bic    num --> ビット位置 (8 進数)
;;;           :bis
;;;           :xor
;;;           :and
;;;
;;; sysmode (システムモードとユーザモード)のためのビットテーブル。
;;; * のついたビットは、ユーザモード用。
;;;         0-1     gc mode (全スタックグループに対して大域的)
;;;                 0       gc 中でない
;;;                 1       マーキングフェーズ実行中
;;;                 2       コレクティングフェーズ実行中
;;;         2*      car-nil-error
;;;                 0       (car nil) は、エラーでない
;;;                 1       (car nil) は、エラーとする
;;;         3*      cdr-nil-error
;;;                 0       (cdr nil) は、エラーでない
;;;                 1       (cdr nil) は、エラーとする
;;;         4*      現在は未使用
;;;         5*      エラーモード
;;;                 0       エラーモードでない
;;;                 1       エラーモードである
;;;         6*      evalhook
;;;                 0       evalhook されてない
;;;                 1       evalhook されている
;;;         7       モニタ busy
;;;                 0       モニタは busy でない
;;;                 1       モニタ busy 状態
;;;         8*      現在のプロセスの特権
;;;                 0       現在のプロセスは特権を持つ
;;;                 1       現在のプロセスは特権を持つ
;;;         9*      １文字ストリング（one character string）
;;;                 0       １文字ストリングのデータタイプはキャラクタ
;;;                 1       １文字ストリングのデータタイプはストリング
;;;         10*     Common Lisp モード
;;;                 0       Common Lisp モードではない
;;;                 1       Common Lisp モード
;;;         11*     パッケージ使用の特権
;;;                 0       特権は on
;;;                 1       特権は off

;;; systat                                 関数[#!expr]
;;;
;;; <説明>
;;;   システムの現在の状態がプリントされる。
;;;
;;; <例>
;;;   (systat)
;;;   [Cobalt-Systat]  load-min:  20%  load-sec:   0%  28-Apr-87  9:43:44
;;; Job Line Process  Job                  Status       Time     Bottom
;;; ----------------------------------------------------------------------
;;; 3*   1   yukari  top-level            input-wait   0:01:05      15
;;; 6    0   kamio   top-level            input-wait   0:00:07      15
;;; 7    6   suzuki  z                    running      0:00:10       5
;;; 0    0   system  interrupt-character  mail-wait    0:00:08       0
;;; 1    4   system  login                input-wait   0:00:00       2
;;; ＠
