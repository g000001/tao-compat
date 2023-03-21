(tao:tao)
(in-package #:tao-internal)

;;  8:50pm Sunday,19 August 2007
(defmacro tao:! (&body forms)
  "<説明>
  形式 : ! &rest body
! は、ローカル変数を宣言することと、バックトラックをする関数を持つ点を
除いて、 or と同じ。すなわち、バックトラックにおいて、その制御は次の
ような式に入る。
 (![(&aux var ...)] B1 B2 ...Bn)
Bn では body の最後が評価される。
! は各結果が nil の間 B1,B2 ... を逐次評価する。
 (![(&aux var ...)] B1 B2 ...Bn) は論理的には \"or\" と同じ。
論理的 \"or\" の ! と特殊シンボルマクロの ! を混同しないよう注意。
少なくとも、シンボル ! の後にスペースが 1 つ入っていれば、論理的 \"or\"
の ! と考えられる。"
  (let ((aux-vars (and (consp (car forms))
                       (string-equal '&aux (string (caar forms)))
                       (prog1 (cdar forms) (pop forms)) ))
        (exit (gensym "EXIT-")) )
    (cl:loop
       :with cuts
       :and tags := (list exit)
       :and body
       :and ans := (gensym "ANS-")

       :for x :in forms
       :if (and (symbolp x) (string-equal '! x))
       :do (progn
             (push (gensym "CUT-") cuts)
             (push `(if ,(car cuts) (go ,exit) (setq ,(car cuts) t))
                   (cdr body) ))
       :else
       :do (progn
             (push (gensym "TAG-") tags)
             (push (car tags) body)
             (push `(and (setq ,ans ,x) (go ,(cadr tags))) body) )
       :finally (return `(prog* (,ans ,@aux-vars ,@cuts)
                            ,@(nreverse body)
                            ,exit
                            (return ,ans) )))))

;;; !                                      特殊シンボルマクロ
;;;
;;; <説明>
;;;   代入を行う。ただし、(! ...) となる場合だけ。
;;;
;;; <例>
;;;         (!x 123) = (setq x 123)
;;;         (!x '(a b c d e)) -> (a b c d e)
;;;         (!(car x) 123) -> 123
;;;         x -> (123 b c d e)

(defmacro tao:self (var)
  `(quote ,var) )

#|(defmacro selfass (fn &rest args)
  (let ((self (dolist (item args)
                (and (listp item)
                     (eq 'self (car item))
                     (return (cadr item)) )))
        (vars (mapcar #'(lambda (item)
                          (if (and (listp item) (eq 'self (car item)))
                              (cadr item)
                              item ))
                      args )))
    `(setf ,self (,fn ,@vars)) ))|#

;;; readtable.lisp
;;; (defmacro tao:selfass (fn &rest args)

#||
(defmacro & (&rest body)
  `((Hclauses (&+dyn ( ) ,@body !))) )
||#


(defmacro tao:& (&body forms)
  "&                                      関数[#!macro]

<説明>
  形式 : & &rest body
局所変数の宣言 (&aux  var  ...) をすることとバックトラックの機能がある
ことを除いては、関数 and と同じ。
 (& [(&aux var ...)] B1 B2 ... Bn) とすると、各々の結果が t の間は、
順番に B1, B2, ... が評価されていく。
  もし、B4 が、nil と評価されるとバックトラックがおこり、B3 が再び評価
される。B3 の評価では、前に選ばれたものの次の節が実行される。B3 が
ロジカル関数でなければ、B3 の再評価は、バックトラックでスキップされ、
B2 がロジカル関数なら B2 が再評価される。バックトラックの前に B3 で
なされるユニフィケーションは元に戻されるが、副作用は元に戻されない。
B1, B2, ... または、Bn で使われる局所変数、特に論理変数は、
 (&aux  var ...) で宣言する必要がある。

<例>
        (& (&aux _x _y) (concatenate _x _y (1 2 1 2)) (== _x _y))
        (prog (_x _y) (& (concatenate _x _y (1 2 3)) (== _x _y)))"
  #+old
  (let ((aux-vars (and (typep (car forms) '&aux-form)
                       (prog1 (cdar forms) (pop forms)) )))
    (let ((exit (gensym "exit-")))
      `(block ,exit
         (tao:let (,@aux-vars)
           ,(tao.logic::compile-body
             forms
             `(lambda () (return-from ,exit T))
             tao.logic::no-bindings)))))
  (let ((aux-vars (and (typep (car forms) '&aux-form)
                       (prog1 (cdar forms) (pop forms)) )))
    (let ((cont (gensym "cont"))
          (tao.logic::*predicate* (gensym "anonymous-pred-")))
      `(tao:let (,@aux-vars)
         (with-return-from-pred ,tao.logic::*predicate* ,cont (nil ,@aux-vars)
           ,(tao.logic::compile-body
             (mapcar #'macroexpand forms)
             `#',cont
             tao.logic::no-bindings))))))


;;; &                                      メッセージ
;;;
;;; <説明>
;;;   形式 : & &rest arg-pattern
;;; & は既に宣言されたインスタンスファクトを参照する。arg-pattern がインス
;;; タンスファクトを宣言していたら t を、そうでなければ nil を返す。
;;; &assert の例を参照。
;;; ＠


(defmacro tao:&+ (&whole whole &rest args)
  "&+                                     関数[#!subr]

<説明>
  形式 : &+ &rest 'x
\(&+ A' [(&aux  var...)] B1 B2  ...  Bn) は、U-resolver と呼ばれる名前
なしのホーン節を作る。
シンボル A は、関数 assert においてはフォーム (p ...) の形をしている。
ここで P は、主ファンクタ、つまりホーン節の名前である。
シンボル A' は、A から主ファンクタ P を除いた残りのフォーム (...) で
あり、 U-resolver のヘッダと呼ばれる。B1 B2 ... Bn は、ボディと呼ばれる。
関数 &+ は、使い方については、関数 expr とほぼ同じで、機能については、
関数 assert とほぼ同じ。スコープ境界型である。
Lisp 関数の assert は、定義のボディを調べ自動的に補助変数宣言を行ない、
主ファンクタに resolver を関連づける。
関数 assert は、resolver がタイプ C なのか U なのかを自動的に決定する。
タイプ C の resolver は C-resolver と呼ばれる。

<例>
        ((&+  ((_x  _))_x)  (1  2  3)) -> 1
        リスト (1  2  3) をヘッダ (_x  .  _) に, ユニファイすることを
        試み、x を 1 にすることによりうまくいく。
        expr の表現とほぼ同じ。
        ((expr (x) (car  x)) '(1  2  3)) -> 1"
  (declare (ignore args))
  `(tao:Hclauses ,whole))


(defmacro tao:&+dyn (pattern &body body)
  "&+dyn                                  関数[#!subr]

 <説明>
   形式 : &+dyn &rest 'x
 変数のスコープについてスコープの制限がないということ以外は、関数 &+ と
 同じ。関数 &+dyn の使い方は、関数 lambda とほぼ同じ。"
  (let ((auxvars nil)
        (body body))
    (typecase body
      ((cons &aux-form *)
       (setq auxvars (cdr (car body)))
       (setq body (cdr body))))
    (etypecase pattern
      (null
       `(tao.logic::compile-local-predicate 'plet
                                            0
                                            '(((plet _arg)
                                               ,@body))
                                            ',auxvars))
      (cons
       `(tao.logic::compile-local-predicate 'plet
                                            ,(length pattern)
                                            '(((plet _arg)
                                               (tao:== ,pattern (_arg))
                                               ,@body))
                                            ',auxvars))
      (symbol ;todo
       `(tao.logic::compile-local-predicate 'plet
                                            1
                                            '(((plet _arg)
                                               (tao:== ,pattern _arg)
                                               ,@body))
                                            ',auxvars)))))



(defmacro tao:&and (&rest body)
  "&and                                   関数[#!macro]

<説明>
  形式 : &and &rest body
body の最後に ! がないことを除いては & と同じ。
バックトラックでは その制御は body の最後から入って行くことができる。

<例>
        (&and [(&aux var ...)] B1 B2 ... Bn) =
                              ((&+dyn () B1 B2 ... Bn))"
  `(tao:query (tao:&+dyn ( ) ,@body)) )


#+old
(defmacro tao:&and (&rest forms)
  "&and                                   関数[#!macro]

<説明>
  形式 : &and &rest body
body の最後に ! がないことを除いては & と同じ。
バックトラックでは その制御は body の最後から入って行くことができる。

<例>
        (&and [(&aux var ...)] B1 B2 ... Bn) =
                              ((&+dyn () B1 B2 ... Bn))"
  (let ((aux-vars (and (consp (car forms))
                       (eq '&aux (caar forms))
                       (prog1 (cdar forms) (pop forms)) ))
        (cont (gensym "cont")))
    `(with-return-from-reval ,cont (,aux-vars)
       ,(tao.logic::compile-body forms
                                 `(function ,cont)
                                 tao.logic::no-bindings))) )


(defmacro tao:&assert (obj &rest args)
  "&assert                                メッセージ

 <説明>
   形式 : &assert &rest arg-pattern
 &assert は arg-pattern によって表されたインスタンスファクトを言明する。
 t を返す。

 <例>
         (defclass fact-class () () () :logical-class) -> fact-class
         (!x (make-instance 'fact-class)) -> {udo}76600fact-class
         [x &assert a b c] -> t
         [x &assert (p q r)] -> t
         [x & a b c] -> t
         [x & a b d] -> nil
         [x & (p q r)] -> t
         [x & (p q 5)] -> nil
         (& (&aux _u _v _w) [x & _u _v _w] (write (list _u _v _w)))
              -> t
         これを実行すると (a b c) とプリントされる。
         (& (&aux _u) [x & . _u] (write _u)) -> t
         これを実行すると (a b c) とプリントされる。
         (& (&aux _u _v) [x & _u] (== _u (_v . _))(write _v)) -> t
         これを実行すると p とプリントされる。"
  `(progn
     (tao.logic::prolog-compile 
      (tao.logic::add-clause (list (list* (tao.object::object-name ,obj)
                                          ',(tao.logic::make-anonymous args)))
                             :asserta nil))))
 
;;; &cond                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : &cond &rest body
;;; 結果節でバックトラックが起こることを除いて関数 cond と同じ。
;;; (&cond  (conditional-clause-1 result-clauses-1)
;;;         (conditional-clause-2 result-clauses-2)
;;;           ...
;;;         (conditional-clause-n result-clauses-n)
;;; または
;;; (&cond  (A1 B11 B12 ... B1n)
;;;         (A2 B21 B22 ... B2n)
;;;         ...
;;;         (Am Bm1 Bm2 ... Bmn))
;;;
;;; 上式では、 A1 が nil でない値を返すなら、B11, B12, ... B1n が実行される。
;;; A1 が nil の場合は A2 が実行され、A2 が nil でない値を返すなら、B21,
;;; B22, ... B2n が実行される。このように、条件節が最初に nil でない値を返
;;; すような結果節が実行される。
;;; B11, B12, ... B1n というような結果節においてはバックトラックが起こる。
;;; (&cond  (A1 B11 B12 ... B1n)
;;;         (A2 B21 B22 ... B2n)
;;;         ...
;;;         (Am Bm1 Bm2 ... Bmn))
;;; =
;;; ((hclauses (&+dyn () A1 ! B11 B12 ... B1n)
;;;            (&+dyn () A2 ! B21 B22 ... B2n)
;;;            ...
;;;            (&+dyn () Am ! Bm1 Bm2 ... Bmn)) )
;;; ＠
;;; &repeat                                関数[#!hclause]
;;;
;;; <説明>
;;;   形式 : &repeat
;;; 反復関数を表す。その繰り返しの中では、ロジックプログラミング用の TAO
;;; のインタプリタは末端再起は使わないが、この関数はスタックにプッシュはし
;;; ない。
;;;
;;; <例>
;;;         (& (&repeat) B1 nil)   B1 を永久に繰り返す。
;;;         (& (&repeat) B1 ! B2)  B1 が nil でない値に変わるまで B1
;;;         を繰り返す。
;;; ＠
;;; &retract                               メッセージ
;;;
;;; <説明>
;;;   形式 : &retract &rest arg-pattern
;;; 既に arg-pattern として宣言されたインスタンスファクトを消去する。既に
;;; 宣言されたインスタンスファクトを完全に消去したなら t を、そうでなければ
;;; nil を返す。
;;;
;;; <例>
;;;         (defclass fact-class () () () :logical-class) -> fact-class
;;;         (!x (make-instance 'fact-class)) -> {udo}76600fact-class
;;;         [x &assert  a b c] -> t
;;;         [x &assert (p q r)] -> t
;;;         [x & a b c] -> t
;;;         [x & (p q r)] -> t
;;;         [x &retract a b 3] -> nil
;;;         [x &retract a b c] -> t
;;;         [x &retract a b c] -> nil
;;;         [x &retract (p q r)] -> t
;;;         [x & a b c] -> nil
;;;         [x & (p q r)] -> nil

;;; *                                      変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 1 つ前のループで評価された結果の返され
;;; た値。

;;; import & export
;;; *                                      関数[#!subr]
;;;
;;; <説明>
;;;   形式 : * &rest number1 number2 ... numberN
;;; number1 number2 ... numberN の積を返す。引数が 1 つのときは、その評価値
;;; を返す。
;;; [number1 * number2 * ...]、(number1 * number2 * ...) の形式でも記述可能。
;;; [ ... ] を用いた形式がインタプリタ上で最も速い。
;;;
;;; <例>
;;;         (* 1 2 3 4 5 6 7) -> 5040
;;;         (* 12345678 11111) -> 137172828258
;;;         (* 5) -> 5
;;;         (*) -> 1
;;; ＠
;;; *                                      ロカティブオペレータ
;;;
;;; <説明>
;;;   2 つのロカティブのかけ算を行う。
;;;
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- 30) -> 10
;;;         (q <- 20) -> 20
;;;         (r <- 10) -> 10
;;;         (s <- (p * q * r)) -> 6000
;;;         s -> 6000
;;;         (p <- (p * q * -20)) -> -12000
;;;         p -> -12000
;;;         (q <- (p + 10000 * r)) -> -20000
;;;         q -> -20000

;;; TODO CLパッケージ再定義をどうするか
;;; import & export
;;; **                                     変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 2 つ前のループで評価された結果の返され
;;; た値。

(defun tao:** (number1 number2)
  "**                                     関数[#!subr]

<説明>
  形式 : ** number1 number2
number1 の値を number2 の値でべき乗した結果を返す。

<例>
        (** 3 2) -> 9
        (** 3 3) -> 27"
  (expt number1 number2))

(defmacro tao::&progn (&body body &environment env)
  (typecase (car body)
    ((cons (eql &aux) *)
     (let ((vars (cdar body)))
       `(let (,@(mapcar (lambda (v) `(,v (tao:_))) vars))
          (flet ((tao.logic::logvar-setter ()
                   ,@(mapcar (lambda (v) `(when (and (tao.logic::var-p ,v)
                                                     (tao.logic::bound-p ,v))
                                            (setq ,v (tao.logic::deref-exp ,v))))
                             vars)
                   T))
            ,@(mapcar (lambda (c)
                        (if (and (typep c '(cons symbol *))
                                 (tao.logic::get-clauses (car c)))
                            (if (macro-function (car c))
                                (append (butlast (macroexpand c env))
                                        (list '#'tao.logic::logvar-setter))
                                (let ((ari (1- (length c))))
                                  `(,(tao.logic::make-predicate (car c) ari) ,@(cdr c) #'tao.logic::logvar-setter)))
                            c))
                      (cdr body))))))
    (T `(progn ,@body))))

;;; import & export
;;; ***                                    変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 3 つ前のループで評価された結果の返された
;;; 値。

;;; *applyhook*                            変数
;;;
;;; <説明>
;;;   通常、eval は apply を使って関数呼び出しを実行するが、もしこの変数が
;;; nil 以外なら、apply のかわりにこの変数の値である関数を使う。この関数は、
;;; 呼び出される関数、引数のリスト、環境の 3 つの引数を受け取る関数で
;;; なければならない。

;;; *break-on-warnings*                    変数
;;;
;;; <説明>
;;;   この変数の値が、nil でなければ、関数 warn は、関数 break と同じように
;;; 働く。つまり、エラーメッセージをプリントし、その後、デバッガに行くか、
;;; ループをブレイクする。

(defmacro tao:*catch (tag &body body)
  "*catch                                 関数[#!macro]

<説明>
  形式 : *catch tag &rest body
*catch は *catch が多値変数を返すのを除いて catch と同じ。
複数のリターン値の最初の値は catch のリターン値と同じ。
2 番目の値は常に nil であり、それは *catch が *throw によってではなく、
正常に終了されることを示す。"
  `(cl:catch ,tag ,@body))

;;; import & export
;;; *debug-io*                             変数
;;;
;;; <説明>
;;;   この変数の値は、会話型でデバッグするために用いる双方向のストリーム。

;;; import & export
;;; *default-pathname-defaults*            変数
;;;
;;; <説明>
;;;   "full-pathname-object" を作るときに使われる。
;;; "pathname-object" のための構文要素を持つ。
;;; この変数は、現在のログインのインスタンス変数 :pathname と同じ。
;;;
;;; <例>
;;;         *default-pathname-defaults* -> {udo}1768299pathname
;;;         (namestring *default-pathname-pdefaults*)
;;;                     -> "Ho::cs:<dire>foo.tao"

;;; import & export
;;; *error-output*                         変数
;;;
;;; <説明>
;;;   この変数の値は、エラーメッセージを送るための出力ストリーム。
;;; 既定値は、*standard-output* 。

;;; *evalhook*                             変数
;;;
;;; <説明>
;;;   step などを書くために、通常の eval のかわりに用いる関数を指定する。
;;; *evalhook* の値が nil 以外なら、その値は、評価するフォームと環境の 2 つ
;;; の引数を取る関数でなければならない。この関数が eval のかわりに評価を
;;; 行う。

;;; import & export
;;; *features*                             変数
;;;
;;; <説明>
;;;   この変数の値は、機構の名前となるシンボルのリスト。
;;; ELIS システムでは、*features* -> (tao elis lsi11)

(defvar tao:*file-search-path* nil
  #.(string '#:|*file-search-path*                     変数

<説明>
  *file-search-path* ディレクトリのリストであり、その要素は、ファイル
をロードする際、そのファイルのパスをサーチするために使われる。
例えば、リストが、("bs:<tools>" "bs:<demo>" "bs:<hora>")であるなら、
ロードされるファイルは、まず現在のディレクトリ、次に "bs:<tools>" ,
3 番目に "bs:<demo>" といったようにサーチされる。|))

;;; *fn-notation*                          変数
;;;
;;; <説明>
;;;   *fn-notation* の値が t なら、プリントイメージ funct (x) は、
;;; (funct x) と等しくなる。つまり *fn-notation* が t のときは、
;;; funct (x) の形でタイプすると、それは、翻訳されて (funct x) として
;;; 受け取られる。そして、(funct x) の代わりに funct (x) が印字される。
;;; 初期値は、nil 。

;;; import & export
;;; *load-verbose*                         変数
;;;
;;; <説明>
;;;   この変数は、関数 load に対するキーワード引数 :verbose への既定値を
;;; 与えている。初期値は nil 。

;;; *logical-name-alist*                   変数
;;;
;;; <説明>
;;;   ファイルの logical-name と formal-name のペアを持つ alist 。
;;; この alist 中の logical-name は、グローバルに使われる。
;;;
;;; <例>
;;; *logical-name-alist* ->
;;; (("host-name1"
;;;     ("logical-name11" . ("device-name11" . "directory-name11"))
;;;     ("logical-name12" . ("device-name12" . "directory-name12"))
;;;     ("logical-name13" . ("device-name13" . "directory-name13")))

;;; import & export
;;; *macroexpand-hook*                     変数
;;;
;;; <説明>
;;;   この変数の値は、マクロ展開のインタフェースのフックとして
;;; 関数 macroexpand-1 で用いられる。 *macroexpand-hook* = nil

;;; import & export
;;; *modules*                              変数
;;;
;;; <説明>
;;;   システムにロードされているモジュール名のリスト。
;;; このリストは、関数 provide 及び require によって使用される。

;;; import & export
;;; *package*                              変数
;;;
;;; <説明>
;;;   現在実行中のパッケージの名称が格納されている変数。
;;; 初期値は user パッケージ名で、関数 load によってパッケージをロード
;;; する度に更新される。
;;;
;;; <例>
;;;         *package* -> {vector}45708(package . 12)

;;; import & export
;;; *print-array*                          変数
;;;
;;; <説明>
;;;   この変数の値が nil であれば、文字列以外の配列の内容はプリントされない。
;;; nil でなければ、文字列でない配列は、#(, #*, #nA, 構文によって内容を
;;; 伴ってプリントされる。初期値は nil。
;;;
;;; <例>
;;;         *print-array* -> nil
;;;         (!v (vcons "vec" 3)) -> {vector}81848("vec" . 3)
;;;         (!*print-array* t) -> t
;;;         (!w (vcons "wec" 3)) -> #3(nil nil nil)

;;; import & export
;;; *print-base*                           変数
;;;
;;; <説明>
;;;   この変数の値は、プリントする数の基数を表す。*print-base* の初期値
;;; は、10。2 から 36 の整数をとる。
;;;
;;; <例>
;;;         *print-base* -> 10
;;;         (!a 10) -> 10
;;;         (!*print-base* 16) -> 10
;;;         (!b 10) -> A

;;; *print-bigfloat-digit*                 変数
;;;
;;; <説明>
;;;   bigfloat-point number を出力する際の桁数を削除する。既定値は、15 。

;;; import & export
;;; *print-case*                           変数
;;;
;;; <説明>
;;;   大文字、小文字を制御する。初期値は nil 。
;;; ユーザはこの変数を参照できるけれども、値を変更することができない。

;;; import & export
;;; *print-circle*                         変数
;;;
;;; <説明>
;;;   循環リストを認識して出力するかどうかを制御する。
;;; この変数の値が nil であれば、プリント過程は再帰主導で実行され、
;;; 循環構造をプリントする場合、永久ループとなり終了しない。
;;; nil でなければ、プリントされる構造のサイクルが検出され、循環性を示す
;;; ために #n= および #n# が用いられる。初期値は nil 。

;;; import & export
;;; *print-escape*                         変数
;;;
;;; <説明>
;;;   特殊記号「バックスラッシュ」を出力するか否かを指定するための変数。
;;; t (初期値) の場合は出力され、t 以外の場合は、出力されない。

;;; tao:*print-float-digit*                    変数
;;;
;;; <説明>
;;;   floating point number を出力する際の桁数を制御する。既定値は、15 。

;;; cl:*print-gensym*                         変数
;;;
;;; <説明>
;;;   ホームパッケージを持たないシンボルの前に前置詞 #: がプリントされる
;;; かどうかを制御する。
;;; nil でなけば #: がプリントされ、t (初期値) の場合、プリントされない。

;;; tao:*print-internal*                       変数
;;;
;;; <説明>
;;;   初期値は nil 。

;;; cl:*print-length*                         変数
;;;
;;; <説明>
;;;   ストリームにプリントされるリスト中の要素の最大数を表す変数。
;;; この値を超えた要素に対しては、... だけをプリントする。
;;; 値が nil なら、全要素をプリントする。既定値は、 nil 。
;;; この変数の値は整数でなければならない。
;;;
;;; <例>
;;;         (!*print-length* 5) -> 5
;;;         (print '(1 2 3 4 5 6 7 8 9)) ->
;;;             (1 2 3 4 5 ...) (1 2 3 4 5 ...)

;;; cl:*print-level*                          変数
;;;
;;; <説明>
;;;   ストリームにプリントされるリストの最大深さを表す。この値を超えた
;;; リストに対しては、プリンタは、[] だけをプリントする。値が、nil なら、
;;; 深さレベルは、無限。初期値は、nil 。
;;; この変数の値は整数でなければならない。
;;;
;;; <例>
;;;         (!x '(1 2 3 (4 5 (6 7 (8 9))))) -> (1 2 3 (4 5 (6 7 (8 9))))
;;;         (!*print-level* 2) -> 2
;;;         (print x) -> (1 2 3 (4 5 (6 7 [])))
;;;         (!*print-level* 1) -> 1
;;;         (print x) -> (1 2 3 (4 5 []))
;;;         (!*print-level* 0) -> 0
;;;         (print x) -> (1 2 3 [])

;;; tao:*print-nilnum*                         変数
;;;
;;; <説明>
;;;   この変数の値が、t なら、プリンタは十進数で (タグビットを除いて)
;;; 残り 24 ビットの nilnum を、{dnil} 10 のようにプリントする。初期値は t。
;;; ＠
;;; *print-package*                        変数
;;;
;;; <説明>
;;;   初期値は t 。

;;; cl:*print-pretty*                         変数
;;;
;;; <説明>
;;;   この変数の値が t なら、プリンタは、空白や改行をいれて Lisp プログラム
;;; を読みやすくプリントする。既定値は t。
;;;
;;; <例>
;;;         *print-pretty* -> t
;;;         (!x '((a b c) e f g)) -> ((a b c)
;;;                               e f g)
;;;         (!*print-pretty* nil) -> nil
;;;         (!x '((a b c) e f g)) -> ((a b c) e f g)

;;; cl:*print-radix*                          変数
;;;
;;; <説明>
;;;   初期値は nil 。この変数の値が nil でなければ、有理数を印字している
;;; 基数を示す基数指定子を前につけて数をプリントする。
;;; *print-base* が 2 なら、基数指定子 #b を前にプリント
;;;                 8 なら、基数指定子 #o を前にプリント
;;;                16 なら、基数指定子 #x を前にプリント
;;;
;;; <例>
;;;         (!*print-base* 16) -> 10
;;;         (!*print-radix* t) -> t
;;;         (!x 12) -> #xC
;;;         23 -> #x17

;;; tao:*print-shortfloat-digit*               変数
;;;
;;; <説明>
;;;   short floating point number を出力する際の桁数を制御する。
;;; 既定値は、6 。

;;; tao:*print-string-marker*                  変数
;;;
;;; <説明>
;;;   この変数の値が、t なら  プリンタは、文字列を "" で囲んでプリントする。
;;; 既定値は、t 。
;;;
;;; <例>
;;;         *print-string-marker* -> t
;;;         (!x "qwe") -> "qwe"
;;;         x -> "qwe"
;;;         (!*print-string-marker* nil) -> nil
;;;         (!y "asd") -> asd
;;;         y -> asd

;;; tao:*print-total-chars*                    変数
;;;
;;; <説明>
;;;   プリントされる文字の最大数を表す。この数を越えた文字に対しては、
;;; プリンタは、... をプリントする。既定値は、nil。
;;; 値が、nil なら、文字数は、無限となる。
;;;
;;; <例>
;;;         (!*print-total-chars* 5) -> 5
;;;         (print '(1 2 3 4 5 6 7 8 9)) -> (1 2 3 ...)

;;; cl:*query-io*                             変数
;;;
;;; <説明>
;;;   利用者に対して質問を行う時に用いる双方向のストリーム。質問は、この
;;; ストリームへ出力され、利用者からの回答は、このストリームから得られ
;;; る。この目的のために、変数 *standard-input* 、*standard-output* の
;;; 代わりに用いられる。

;;; cl:*random-state*                         変数
;;;
;;; <説明>
;;;
;;;   この変数はデータ構造を持つ。すなわち、random-state 型のオブジェクト
;;; であり、これは既定として random 関数が用いている乱数発生機構をコード
;;; 化したものである。
;;;
;;; <例>
;;;         *random-state* -> {udo}56590random-state

;;; cl:*read-base*                            変数
;;;
;;; <説明>
;;;   整数または分数が読み込まれる基数として、2 から 36 までの任意の整数
;;; をとる。通常は 10 。
;;; この変数の値を 10 以上にセットする場合には注意が必要。通常はシンボル
;;; として解釈されるトークンが、数として解釈されるかもしれないからである。

;;; cl:*read-default-float-format*            変数
;;;
;;; <説明>
;;;   ある特定の浮動小数点形式に対する型指定子シンボル。これらには、
;;; short-float, single-float, double-float, long-float が含まれる。初期
;;; 値は sys:shortfloat。この変数は、指数マーカーを持たない或は指数
;;; マーカーに e または E を持つ浮動小数点数を読むために用いられるべき浮
;;; 動小数点形式を表す。
;;; ＠

;;; standard-readの項によると存在している変数
(defvar tao:*read-eof-value* :eof)

;;; cl:*read-suppress*                        変数
;;;
;;; <説明>
;;;   この変数の値が nil であれば、Lisp リーダは通常通りに動作する。
;;; nil でなければ、データ入力の read などは、データを単に読み飛ばすだけで、
;;; それ以外のインターンなどの通常の処理をほとんど行わない。
;;; "#+" や "#-" などのディスパッチマクロでフォームを読み飛ばすような場合に
;;; 利用する。初期値は nil 。

;;; cl:*readtable*                            変数
;;;
;;; <説明>
;;;   現在の読み込み表 (readtable) 。使われている読み込み表を一時的に変更
;;; するために、この変数を束縛することができる。
;;;
;;; <例>
;;;         *readtable* -> {vector}1282199(readtable . 128)

;;; cl:*standard-input*                       変数
;;;
;;; <説明>
;;;   クラス fundamantal-stream のインスタンス。これは標準入力の
;;; ストリームとなる。トップレベルループにおいて、入力は、このストリーム
;;; から読まれる。通常、この値はコンソールターミナル。

;;; cl:*standard-output*                      変数
;;;
;;; <説明>
;;;   クラス fundamental-stream のインスタンス。これは標準出力の
;;; ストリームとなる。トップレベルループにおいて、出力は、このストリーム
;;; へ送られる。通常、この値はコンソールターミナル。

;;; cl:*terminal-io*                          変数
;;;
;;; <説明>
;;;   利用者のコンソールへ結合される双方向のストリーム。このストリームへ
;;; 書くことは画面にその出力が表示されることで、このストリームから読むこ
;;; とはキーボードからの入力を受け入れることとなる。
;;; ＠

(defmacro tao:*throw (tag value)
  "*throw                                 関数[#!expr]

<説明>
  形式 : *throw tag form
*throw は *throw が form の値と、tag の値を示す多値を返すことを除き、
throw のように働く。"
  `(cl:throw ,tag ,value))

;;; tao:*trace-level*                          変数
;;;
;;; <説明>
;;;   現在トレースされている関数のネストレベルを表す。

;;; cl:*trace-output*                         変数
;;;
;;; <説明>
;;;   関数 trace がその出力をプリントするような出力ストリーム。

;;; tao:*traced-fns*                           変数
;;;
;;; <説明>
;;;   実行された時にトレースされるべき関数のリスト。
;;; これは、パッケージ trace の変数。

;;; tao:*untraced-fns*                         変数
;;;
;;; <説明>
;;;   初めはトレースされる予定であったが現在はトレースされるべきではない
;;; と宣言された関数のリスト。この宣言は、関数 untrace により行われる。

;;; tao:*user-packages*                        変数
;;;
;;; <説明>
;;;   現在ログインしているユーザにより作られたパッケージのリスト。

;;; cl:+                                      変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 1 つ前のループで評価されたフォーム。

;;; cl:+                                      関数[#!subr]
;;;
;;; <説明>
;;;   形式 : + &rest number1 number2 ... numberN
;;; number1 number2 ... numberN の和を返す。
;;; [number1 + number2 + ...]、(number1 +  number2  +  ...) の形式でも
;;; 記述可能。[ ... ] を用いた形式がインタプリタ上で最も速い。
;;;
;;; <例>
;;;         (+ 1 2) -> 3
;;;         (+ 1 2 3 4 5 6 7 8 9 10) -> 55
;;;         (+ 3) -> 3
;;;         (+) -> 0

(defun locative+ (x y)
  "+                                      ロカティブオペレータ

<説明>
  2 つのロカティブを加える。

<例>
        (signed-integer-locatives p q r s) -> (p q r s)
        (p <- 10) -> 10
        (q <- 20) -> 20
        (r <- -10) -> -10
        (s <- (p + q + r)) -> 20
        s -> 20
        (p <- (p + q + 20)) -> 50
        p -> 50"
  (+ (if (tao:locbitp x) (tao:deref x) x)
     (if (tao:locbitp y) (tao:deref y) y)))

;;; cl:++                                     変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 2 つ前のループで評価されたフォーム。

#+lispworks
(defmacro tao:++ (locbit)
  "++                                     メッセージ

<説明>
  メッセージ受け渡し式 (locbit ++) は、まずロックビット locbit により
指定された値にアクセスし、次に locbit のオフセットを 1 増やす。

<例>
        mem を 16 ビットメモリブロック、p を mem のロックビットとする。
        (nthm mem 11) -> #30
        (loc-offset p) -> 11
        (!@(p ++) #100) -> #100
        (loc-offset p) -> 12
        (nthm mem 11) -> #100
        ここで、@(p ++) は、(deref (p ++)) の省略形。"
  `(prog1 (fli:copy-pointer ,locbit)
     (fli:incf-pointer ,locbit)))


(defmacro ++n (locbit)
  `(fli:incf-pointer ,locbit))

;;; ++                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : ++ locbit
;;; locbit のオフセット値 (メモリブロック内の語アドレスを示す 0 から始まる
;;; 数字) を 1 増やし、その結果を locbit に代入する。@ による略記を行うか
;;; 否か現在検討中。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!16b-memblk 200)) ->
;;;                            {memblk}48462(#!16b-memblk . {dnil}200)
;;;         (!bbb (locbit aaa 100)) ->
;;;          {locbit}({memblk}48462(#!16b-memblk . {dnil}200) . {dnil}100)
;;;         (++ bbb) ->
;;;          {locbit}({memblk}48462(#!16b-memblk . {dnil}200) . {dnil}101)
;;;         (!@(++ bbb) #123) -> #123
;;;         (deref bbb) -> #123
;;;         ここで、@(++ bbb) は、(deref (++ bbb)) の省略形。

;;; cl:+++                                    変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 3 つ前のループで評価されたフォーム。

;;; cl:-                                      変数
;;;
;;; <説明>
;;;   現在、トップレベル・ループで評価されているフォーム。

;;; cl:-                                      関数[#!subr]
;;;
;;; <説明>
;;;   形式 : - number1 &rest number2 number3 ... numberN
;;; number1 から number2 number3 ... numberN の値を引いた値を返す。
;;; 引数が 1 つの場合、number1 の評価値を負にして返す。
;;; (number1 - ... - numberN)、[number1 - ... - numberN] の形式でも記述可能。
;;; [ ... ] を用いた形式がインタプリタ上で最も速い。
;;;
;;; <例>
;;;         (- 10 4) -> 6
;;;         (- 5) -> -5
;;;         (- 10 2 3 4 5) -> -4
;;;         (2 -) -> エラー
;;;         (-) -> エラー

;;; -                                      ロカティブオペレータ
;;;
;;; <説明>
;;;   2 つのロカティブの差をとる。
;;;
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- 30) -> 10
;;;         (q <- 20) -> 20
;;;         (r <- 10) -> 10
;;;         (s <- (p - q - r)) -> 0
;;;         s -> 0
;;;         (p <- (p - q - 20)) -> -10
;;;         p -> -10

;;; --                                     メッセージ
;;;
;;; <説明>
;;;   メッセージ受け渡し式 (locbit --) は、まずロックビット locbit で、
;;; 指定された値にアクセスし、次に locbit のオフセットを 1 減少させる。
;;;
;;; <例>
;;;         mem を 16 ビットメモリブロック、p を mem の locbit とする。
;;;         (nthm mem 11) -> #30
;;;         (loc-offset p) -> 11
;;;         (!@(p --) #100) -> #100
;;;         (loc-offset p) -> 10
;;;         (nthm mem 11) -> #100

;;; --                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : -- locbit
;;; locbit のオフセットの値 (メモリブロック内の語アドレスを示す 0 から
;;; 始まる数字) を 1 減少し、その結果を locbit に代入する。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!16b-memblk 200)) ->
;;;            {memblk}48462(#!16b-memblk . {dnil}200)
;;;         (!bbb (locbit aaa 100)) ->
;;;          {locbit}({memblk}48462(#!16b-memblk . {dnil}200) . {dnil}100)
;;;         (-- bbb) ->
;;;           {locbit}({memblk}48462(#!16b-memblk . {dnil}200) . {dnil}99)
;;;         (!@(-- bbb) #123) -> #123
;;;         (deref bbb) -> #123
;;;         ここで @(-- bbb) は、(deref (-- bbb)) の省略形

(defsynonym tao:|.| cl:cons
  ".                                      メッセージ

<説明>
  角カッコ記法で使用され、指定された要素で構成されるリストを返す。

<例>
        [1 . 2] -> (1 . 2)
        ['a . nil] -> (a)
        ['a . '(b c)] -> (a b c)
        ['(1 2 3) . '(4 5 6)] -> ((1 2 3) 4 5 6)")

(defsynonym tao:|..| cl:append
  "..                                     メッセージ

<説明>
  角カッコの中で使用され、リストを作成し、その結果を返す。
関数 append と同じ。

<例>
        ['(1 2 3) .. '(4 5 6)] -> (1 2 3 4 5 6)")

;; ???
(define-symbol-macro tao:/ cl:/)

;;; tao:/                                      変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 1 つ前のループで評価された結果の返された
;;; 値の多値リスト。

(defun tao:/ (number1 number2)
  "/                                      関数[#!subr]

<説明>
  形式 : / number1 number2
number1 number2 がともに整数なら、number1 の値を number2 の値で割った
結果を商と剰余に分けて返す。(整数以外の場合、商のみ返す)。
\(number1 / number2), [number1 / number2] の形式でも記述可能。
インタプリタで実行する場合、[number1 / number2]が最も速い。

<例>
        (/ 14 4) -> !(3 2)
        (/ 14.0 4) -> 3.5
        (/ 14 4.0) -> 3.5
        (/ 10 3) -> !(3 1)
        (/ 3) -> 3
        (/) -> エラー"
  (if (and (typep number1 'integer)
           (typep number2 'integer))
      (floor number1 number2)
      (cl:/ number1 number2)))

;;; /                                      ロカティブオペレータ
;;;
;;; <説明>
;;;   2 つのロカティブの割り算を行う。
;;;
;;; <例>
;;;         (signed-integer-locatives p q r s) -> (p q r s)
;;;         (p <- 30) -> 30
;;;         (q <- 2) -> 2
;;;         (r <- 10) -> 10
;;;         (s <- (p / q / r)) -> 1
;;;         s -> 1
;;;         (p <- (p / -5 / q)) -> -3
;;;         p -> -3
;;;         (q <- (p - q / r)) -> 2
;;;         q -> 2

(defsynonym common:/ cl:/
  "common:/                               関数[#!expr]

<説明>
  形式 : common:/ number1 &rest number2 number3 ... numberN
number1 を number2 number3 ... numberN で連続して割り算を行う。
number1 number2 ... numberN が整数の時、結果が整数にならなければ
分数の形式で返す。

<例>
        (common:/ 50 1 2 3 4) -> 25/12
        (common:/ 100.0 2.5 4.0 2.0) -> 5.0
        (common:/ 100.0 pi) -> 31.830988618379f0")

;;; ＠
;;; //                                     変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 2 つ前のループで評価された結果の返された
;;; 値の多値リスト。
;;; ＠

(defun tao:// (&rest numbers)
  (if (cdr numbers)
      ;;
      (let ((prev (car numbers))
            q r)
        (dolist (n (cdr numbers))
          (setf (values q r)
                (tao:/ prev n)))
        (if r
            (values q r)
            q))
      ;;
      (car numbers)))

;;; //                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : // number1 &rest number2 number3 ... numberN
;;; number1 を number2 number3 ... numberN で連続して割り算を行う。
;;; number1 number2 ... numberN  がともに整数なら、結果を商と剰余に分けて
;;; 返す。(整数以外の場合、商のみ返す)。
;;;
;;; <例>
;;;         (// 5 4) -> 1 !(1 1)では?
;;;         (// 3) -> 3
;;;         (//) -> nil
;;;         (// 10 2 3) -> !(1 2)
;;; ＠
;;; ///                                    変数
;;;
;;; <説明>
;;;   現在のトップレベル・ループの 3 つ前のループで評価された結果の返された
;;; 値の多値リスト。
;;; ＠

(defun tao:/= (x y)
  #.(string '#:|/=                                     関数[#!subr]

<説明>
 (1)
  形式 : string1 /= string2 /= ... /= stringN
文字列 string1 string2 ... stringN を順番に比較し、一致した文字列のうち、
最後の文字列の値を返す。一致しなければ nil を返す。

 (2)
  形式 : /= number1 number2
数値 number1 number2 の値を比較し、一致しなければ number2 の評価値を
返し、そうでなければ nil を返す。
\(number1 /= number2) 、[number1 /= number2] の形式でも記述可能。
infix notation では、任意個の引数が指定可能。

<例>
 (1)
        ("abc" /= "bcd" /= "cde" /= "abc") -> "abc"
        ("a" /= "b" /= "a" /= "b" /= "a" /= "b") -> "b"
        ("a" /= "b" /= "b") -> nil
        ("あいう" /= "かきく" /= "あいう") -> "あいう"
        ("あいう" /= "かきく" /= "かきく") -> nil
 (2)
        (/= 1 2) -> 2
        (/= 2 2) -> nil
        (3 /= 4) -> 4
        (1 /= 2 /= 3 /= 4) -> 4
        (/= 2 3 4) -> エラー|)
  (etypecase x
    (string (etypecase y
              (string (and (string/= x y)
                           y))))
    (number (etypecase y
              (number (and (cl:/= x y)
                           y))))))

(defsynonym common:/= cl:/=
  "common:/=                              関数[#!subr]

<説明>
  形式 : common:/= number1 &rest number2 number3 ... numberN
number1 number2 ... numberN の値 (複素数でもよい) を比較し、等しくない
なら、numberN の値を返し、等しければ nil を返す。

<例>
        (common:/= 5 4 3 2) -> 2
        (common:/= 4 4 4 3) -> 3
        (common:/= 6 6 6 6) -> nil")

;;; cl:1+                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : 1+ number
;;; number の値 (shortnum) に 1 を加え、その結果を返す。引数が shortnum
;;; という点を除けば、(+ x 1) と同じ。1+ のほうが + より動作が多少速い。
;;;
;;; <例>
;;;         (1+ 3) -> 4
;;;         (1+ -1) -> 0
;;;         (1+ 0) -> 1

;;; cl:1-                                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : 1- number
;;; number の値 (shortnum) を 1 減少させ、その結果を返す。引数が
;;; shortnum という点を除けば (- x 1) と同じ。1- のほうが - より動作が多
;;; 少速い。
;;;
;;; <例>
;;;         (1- 3) -> 2
;;;         (1- -1) -> -2
;;;         (1- 0) -> -1

;;; 64b-float 未インプリメント             関数[#!subr]
;;;
;;; <説明>
;;;   浮動小数点ロカティブについてのメモリロケイションを得る。
;;; 現在は、使えない。

;;; 64b-floatp                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : 64b-floatp object
;;; object が 64 ビット浮動小数点ロカティブならその値を返し、そうでなければ
;;; nil を返す。
;;;
;;; <例>
;;;         (float-locatives a) -> (a)
;;;         (64b-floatp a) -> 476370

;;; 64b-signed                             関数[#!subr]
;;;
;;; <説明>
;;;   符号付き整数ロカティブについてのメモリロケイションを得る。
;;;
;;; <例>
;;;         (!x (64b-signed)) -> 47876
;;;         x はロカティブ
;;;         (x <- 168) -> 168
;;;         x -> 168

;;; 64b-signedp                            関数[#!subr]
;;;
;;; <説明>
;;;   形式 : 64b-signedp object
;;; object が 64 ビット符号付き整数ロカティブならその値を返し、
;;; そうでなければ nil を返す。
;;;
;;; <例>
;;;         (signed-integer-locatives a) -> (a)
;;;         (unsigned-integer-locatives b) -> (b)
;;;         (64b-signedp a) -> 476365
;;;         (64b-signedp 'a) -> nil
;;;         (64b-signedp b) -> nil
;;;         (64b-signedp 12) -> nil

;;; 64b-unsigned                           関数[#!subr]
;;;
;;; <説明>
;;;   符号なし整数ロカティブについてのメモリロケイションを得る。
;;;
;;; <例>
;;;         (!y (64b-unsigned)) -> 123
;;;         y はロカティブ
;;;         (y <- #1234) -> #1234
;;;         y -> #1234

;;; 64b-unsignedp                          関数[#!subr]
;;;
;;; <説明>
;;;   形式 : 64b-unsignedp object
;;; object が 64 ビット符号なし整数ロカティブならその値を返し、
;;; そうでなければ nil を返す。
;;;
;;; <例>
;;;         (unsigned-integer-locatives a) -> (a)
;;;         (signed-integer-locatives b) -> (b)
;;;         (64b-unsignedp a) -> 476365
;;;         (64b-unsignedp b) -> nil
;;;         (64b-unsignedp 'a) -> nil
;;;         (64b-unsignedp 12) -> nil

;;; 64bfloc                                クラス
;;;
;;; <説明>
;;;   インスタンスは 64 ビット表現の浮動小数点数。
;;; 64bfloc は 64-bit-float-locative の意味。
;;; 64bfloc データはガーベジコレクタの対象とならない。

;;; 64bsiloc                               クラス
;;;
;;; <説明>
;;;   インスタンスは 64 ビット表現の整数。
;;; 64bsiloc は 64-bit-signed-integer-locative の意味。
;;; 64bsiloc データはガーベジコレクタの対象とならない。

;;; 64builoc                               クラス
;;;
;;; <説明>
;;;   インスタンスは 64 ビット表現の整数。
;;; 64builoc は 64-bit-unsigned-integer-locative の意味。
;;; 64builoc データはガーベジコレクタの対象とならない。

;;; :abstract-class                        キーワード
;;;
;;; <説明>
;;;   インスタンスが作られないクラスを定義するときに使用。
;;; つまり :abstract-class が指定されたとき、クラスはいかなるインスタンス
;;; (udo: user defined object) も持たない。
;;; このクラスにインスタンスを作ろうとするとエラーとなる。
;;; :abstract-class が指定されたクラスは、インヘリタンスに使われるだけ
;;; である。

;;; :and                                   キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプもしくは :and タイプの
;;; message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索は、クラスから始め、そのスーパクラスへと進む。
;;; (and message-passing-of-method1 message-pssing-of-method2 ...
;;; message-passing-of-methodN) が評価され、この and 式の値が返る。
;;; つまり、あるメソッドが nil を返すとき nil が返され、残りのメソッドは
;;; 無視される。 methodN は N 番目に見つかった :primary タイプもしくは
;;; :and タイプのメソッドを示す。

;;; :append                                キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプもしくは :append タイプの
;;; message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索は、クラスから始め、そのスーパクラスへと進む。
;;; (append message-passing-of-method1 message-passing-of-method2 ...
;;; message-passing-of-methodN) が評価され、この append 式の値が返る。
;;; methodN は N 番目に見つかった :primary タイプもしくは :append タイプの
;;; メソッドを示す。

;;; :class-constants                       キーワード
;;;
;;; <説明>
;;;   形式 : (:class-constant var1 val1 var2 val2 ...)
;;; var1,var2, ... を定数として定義し、各々の値は val1, val2, ... となる。
;;; この宣言の後ではクラス定数 var1, var2, ... にはどの様な値も代入できない。

;;; :class-methods                         キーワード
;;;
;;; <説明>
;;;   形式 : (:class-method (method args body))
;;; クラスメソッド method を定義する。
;;; :class-method は、関数 defclass-method と同じ効果をもつが、クラス名の
;;; 指定をしなくてよい点が異なる。
;;; body はメソッドのボディ。args にはメソッドで使われる引数を指定。

;;; :daemon                                キーワード
;;;
;;; <説明>
;;;   message が送られるとき、階層構造 (木構造) になっているクラスとその
;;; スーパクラスを縦型探索と横型検索の方法で捜し、各クラスで定義された
;;; :before、:primary、:after 各タイプのメソッドをすべて取り出す。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; :before タイプのメソッドと :primary タイプのメソッドについて、
;;; 探索はクラスから始まりそのスーパクラスへと進む。
;;; :after タイプのメソッドについては、この捜す順序は :before タイプの
;;; メソッドを捜したときとは逆になる。
;;; ただし以上の検索は message が最初に送られたときにだけ行われ、
;;; 2 回目からは、1 回目の探索結果をそのまま使う。
;;; まず、探索で見つけたすべての :before タイプのメソッドを、見つけた順に
;;; 実行する。次に、最初に見つけた :primary タイプのメソッドを、実行する。
;;; 3 番目に、見つけたすべての :after タイプのメソッドを、見つけた順に実行
;;; する。返される値は、:primary タイプのメソッドの最後の形式の値である。
;;;
;;; <例>
;;;         (!aaa nil) -> nil
;;;         (defclass king () () ()) -> king
;;;         (defclass queen () () (king)) -> queen
;;;         (defclass jack () () (queen) (:method-combination (:daemon)))
;;;                                                         -> jack
;;;         (defmethod (king :before chain) () (!!cons 3 !aaa)) -> chain
;;;         (defmethod (queen :after chain) () (!!cons 2 !aaa)) -> chain
;;;         (defmethod (jack :primary chain) () (!!cons 1 !aaa)) -> chain
;;;         (!jack1 (make-instance 'jack)) -> {udo}37610jack
;;;         [jack1 chain] -> (1 3)
;;;         aaa -> (2 1 3)

;;; :daemon-with-and                       キーワード
;;;
;;; <説明>
;;;   :daemon-with-or において、or 関数の代わりに and 関数を、
;;; :or タイプのメソッドの代わりに :and タイプのメソッドを使えば、
;;; それは、:daemon-with-and となる。
;;; :primary タイプのメソッドは、すべての :and タイプのメソッドの値が
;;; nil でないときにだけ実行される。
;;;
;;; <例>
;;; (!aaa nil) -> nil
;;; (defclass joker () () ()) -> joker
;;; (defclass ace () () (joker)) -> ace
;;; (defclass king () () (ace)) -> king
;;; (defclass queen () () (king)) -> queen
;;; (defclass jack () () (queen) (:method-combination (:daemon-with-and)))
;;;                                                         -> jack
;;; (defmethod (joker :and (chain)) () (!!cons 5 !aaa))    -> (chain)
;;; (defmethod (ace :before (chain)) () (!!cons 4 !aaa))   -> (chain)
;;; (defmethod (king :or (chain)) () (!!cons 3 !aaa))      -> (chain)
;;; (defmethod (queen :after (chain)) () (!!cons 2 !aaa))  -> (chain)
;;; (defmethod (jack :primary (chain)) () (!!cons 1 !aaa)) -> (chain)
;;; (!jack1 (make-instance 'jack)) -> {udo}37610jack
;;; [jack1 (chain)] -> (1 3 4)
;;; aaa -> (2 1 3 4)

;;; :daemon-with-or                        キーワード
;;;
;;; <説明>
;;;   message が送られるとき、階層構造 (木構造) になっているクラスと
;;; そのスーパクラスを縦型探索と横型検索の方法で捜し、各クラスで定義された
;;; :before、:primary、:after、:or 各タイプのメソッドをすべて取り出す。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; :before、:primary、:or 各タイプのメソッドについて、探索はクラスから
;;; 始まりそのスーパクラスへと進む。
;;; :after タイプのメソッドについては、この捜す順序は :before タイプの
;;; メソッドを捜したときとは逆になる。
;;; ただし以上の探索は message が最初に送られたときにだけ行われ、
;;; 2 回目からは、1 回目の探索結果をそのまま使う。
;;; methodN を N 番目に見つけた :or タイプのメソッドとする。
;;; また methodP を、最初に見つけた :primary タイプのメソッドとする。
;;; まず、探索で見つけた :before タイプのメソッドをすべて、見つけた順に
;;; 実行する。
;;; 次に、(or message-passing-of-method1 message-passing-of-method2...
;;; message-passing-of-methodN message-passing-of-methodP) を実行する。
;;; その次に、探索で見つけた :after タイプのメソッドをすべて、見つけた順
;;; に実行する。
;;; 返される値は、(or ... message-passing-of-methodP) の値である。
;;; つまり、:primary タイプのメソッドが 1 つだけ実行されるのではなく、
;;; すべての :or タイプのメソッドと最初に見つけた :primary タイプの
;;; メソッドが or 関数の中で実行されるのであるという点を除けば、
;;; daemon-with-or は :daemon と同じである。
;;;
;;; <例>
;;; (!aaa nil) -> nil
;;; (defclass joker () () ()) -> joker
;;; (defclass ace () () (joker)) -> ace
;;; (defclass king () () (ace)) -> king
;;; (defclass queen () () (king)) -> queen
;;; (defclass jack () () (queen) (:method-combination (:daemon-with-or)))
;;;                                                      -> jack
;;; (defmethod (joker :after (chain)) () (!!cons 5 !aaa))    -> (chain)
;;; (defmethod (ace :before (chain)) () (!!cons 4 !aaa))     -> (chain)
;;; (defmethod (king :or (chain)) () (!!cons 3 !aaa))        -> (chain)
;;; (defmethod (queen :after (chain)) () (!!cons 2 !aaa))    -> (chain)
;;; (defmethod (jack :primary (chain)) () (!!cons 1 !aaa))   -> (chain)
;;; (!jack1 (make-instance 'jack)) -> {udo}37610jack
;;; [jack1 (chain)] -> (3 4)
;;; aaa -> (2 5 3 4)

;;; :default-init-plist                    キーワード
;;;
;;; <説明>
;;;   形式 : :default-init-plist pair1 pair2 ... pairN
;;; インスタンス変数の初期値を設定する。
;;; ここで pair1 は変数名 var1 とその値 val1 のペア、pair2 は変数名 var2
;;; とその値 val2 のペア ... となっている。var1 は val1 に、var2 は、
;;; val2 に、... と初期化される。
;;; 関数 defclass の第 3 引数 inst-vars による初期化は、
;;; この :default-init-plist による初期化より優先される。
;;; クラスが定義されるとき 各初期値 (valI) は、各々 1 度ずつ評価される。
;;;
;;; <例>
;;;         (defclass ab () (a b c) () :gettable
;;;                           (:default-init-plist a 1 b 2 c 3)) -> ab
;;;         (!abab (make-instance 'ab)) -> {udo}49354ab
;;;         [abab a] -> 1
;;;         [abab b] -> 2
;;;         [abab c] -> 3

;;; :eval-when-instantiation               キーワード
;;;
;;; <説明>
;;;   形式 : :eval-when-instantiation form1 form2 ... formN
;;; 関数 make-instance を実行するとき、順に form1 form2 ... が評価される。
;;; インスタンス変数は form1 ... の中でその変数名によってアクセス可能だが、
;;; 関数 cvar と関数 super は form1 ... では使えない。form1 ... では 関数
;;; cvar の代わりに関数 class-variable が使われる。このとき作られる
;;; インスタンスは form1 ... の中では self によって参照される。
;;;
;;; <例>
;;;         (defclass abc ((count 0)) () ()
;;;           (:eval-when-instantiation
;;;              (!!1+ !(class-variable count abc))) -> abc
;;;         count -> 0
;;;         (make-instance 'abc) -> {udo}48687abc
;;;         count -> 1
;;;         (make-instance 'abc) -> {udo}48688abc
;;;         count -> 2
;;;         (make-instance 'abc) -> {udo}48689abc
;;;         count -> 3

;;; :gettable                              キーワード
;;;
;;; <説明>
;;;   :gettable を指定すると、クラスのインスタンス変数の値をクラスの
;;; メソッドの外から見ることができる。
;;; :gettable はすべてのインスタンス変数をアクセス可能にする。
;;; しかし (:gettable var1 var2...) とすると、メッセージ var1, var2, ...
;;; を作ることにより、インスタンス変数 var1, var2, ... だけをアクセス可能
;;; にする。
;;;
;;; <例>
;;;         (defclass abc () ((abc1 10)  (abc2 20)) () :gettable) -> abc
;;;         (!abcabc (make-instance 'abc)) -> {udo}45900abc
;;;         [abcabc abc1] -> 10
;;;         [abcabc abc2] -> 20

;;; :init-class-vars                       キーワード
;;;
;;; <説明>
;;;   形式 : :init-class-vars pair1 pair2 ... pairN
;;; :default-init-plist と同じ方法でクラス変数を初期化する。

;;; :inverse-list                          キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプもしくは :inverse-list
;;; タイプの message という名のメソッドを求めて、木構造をなすクラスと
;;; そのスーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索は、クラスから始め、そのスーパクラスへと進む。
;;; (list message-passing-of-methodN ... message-passing-of-method2
;;; message-pssing-of-method1) が評価され、この list 式の値が返される。
;;; methodN を N 番目に見つけた :primary タイプ、もしくは :inverse-list
;;; タイプのメソッドとする。

;;; :list                                  キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプ、もしくは :list タイプの
;;; message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索は、クラスから始め、そのスーパクラスへと進む。
;;; methodN を N 番目に見つかった :primary タイプ、もしくは :list タイプの
;;; メソッドとする。
;;; (list message-passing-of-method1 message-pssing-of-method2 ...
;;; message-passing-of-methodN) が評価され、この list 式の値が返される。

;;; :method-combination                    キーワード
;;;
;;; <説明>
;;;   形式 : :method-combination howto
;;; セルフクラスおよびスーパクラスにある同じセレクタをもつメソッドの
;;; 結合方法 howto を宣言する。
;;; TAO では、関数 defclass を用いてのクラスの定義において、1 つの結合方法
;;; だけが設定可能。すなわち howto は値として :daemon, :daemon-with-and,
;;; :daemon-with-or, :progn, :or, :and, :append, :nconc, :list,
;;; :inverse-list のどれか 1 つだけをとる。howto の既定値は :daemon。

;;; :nconc                                 キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプ、もしくは :nconc タイプ
;;; の message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索はクラスから始め、そのスーパクラスへと進む。
;;; (nconc message-passing-of-method1 message-pssing-of-method2 ...
;;; message-passing-of-methodN) が評価され、この nconc 式の値が返される。
;;; methodN を N 番目に見つかった :primary タイプもしくは :nconc タイプの
;;; メソッドとする。

;;; :no-vanilla-class                      キーワード
;;;
;;; <説明>
;;;   このオプションが指定されない場合、特別なクラス vanilla-class が
;;; クラスのスーパクラスとして自動的に定義される。
;;;
;;; <例>
;;;         (defclass ab () () ()) -> ab
;;;         (!abab (make-instance 'ab)) -> {udo}45870ab
;;;         [abab describe]
;;;              -> {udo}45870ab, an object of class ab (version 0),
;;;                     has instance variable values: nil
;;;         (defclass abc () () () :no-vanilla-class) -> abc
;;;         (!abcabc (make-instance 'abc)) -> {udo}45884abc
;;;         [abcabc describe] -> (no-method-found {udo}45884abc describe)
;;;         メッセージ describe は、vanilla-class で定義されている。

;;; :or                                    キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプ、もしくは :or タイプの
;;; message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索はクラスから始め、そのスーパクラスへと進む。
;;; (or message-passing-of-method1 message-pssing-of-method2 ...
;;; message-passing-of-methodN) が評価され、この or 式の値が返される。
;;; つまり、あるメソッドが nil でない値を返すとき、この値が返され、
;;; 残りのメソッドは無視される。methodN を N 番目に見つかった :primary
;;; タイプもしくは :or タイプのメソッドとする。

;;; :progn                                 キーワード
;;;
;;; <説明>
;;;   メッセージが送られたとき、:primary タイプ、もしくは :progn タイプ
;;; の message という名のメソッドを求めて、木構造をなすクラスとその
;;; スーパクラスを縦型探索と横型検索の方式で探索する。
;;; 木構造は関数 defclass の引数 supers によって作られる。
;;; 探索はクラスから始まり、そのスーパクラスへと進む。
;;; :primary タイプ、もしくは :progn タイプのメソッドは、見つけた順に
;;; 実行される。返されるのは最後に実行された形式の値。
;;;
;;; <例>
;;; (!aaa nil) -> nil
;;; (defclass joker () () ()) -> joker
;;; (defclass ace () () (joker)) -> ace
;;; (defclass king () () (ace)) -> king
;;; (defclass queen () () (king)) -> queen
;;; (defclass jack () () (queen) (:method-combination (:progn))) -> jack
;;; (defmethod (joker :after (chain)) () (!!cons 5 !aaa))      -> (chain)
;;; (defmethod (ace (chain)) () (!!cons 4 !aaa))               -> (chain)
;;; (defmethod (king (chain)) () (!!cons 3 !aaa))              -> (chain)
;;; (defmethod (queen :after (chain)) () (!!cons 2 !aaa))      -> (chain)
;;; (defmethod (jack :primary (chain)) () (!!cons 1 !aaa))     -> (chain)
;;; (!jack1 (make-instance 'jack)) -> {udo}37610jack
;;; [jack1 (chain)] -> (4 3 1)
;;; aaa -> (4 3 1)
;;; メソッドのタイプの既定値は、:primary。

;;; :settable                              キーワード
;;;
;;; <説明>
;;;   :settable を指定するとクラスのインスタンス変数をクラスのメソッドの
;;; 外から設定可能となる。
;;; (:settable var1 var2...) とすると、インスタンス変数 var1, var2,... に
;;; 対するメッセージ set-var1, set-var2, ... が自動的に作られる。
;;; また単に (:settable) とすれば、すべてのインスタンス変数に対して同様の
;;; メッセージが作られ、すべてのインスタンス変数に値が設定可能となる。
;;;
;;; <例>
;;;         (defclass abc () ((abc1 10)  (abc2 20) abc3) ()
;;;                          :gettable (:settable abc2 abc3)) -> abc
;;;         (!abcabc (make-instance 'abc)) -> {udo}45900abc
;;;         [abcabc abc1] -> 10
;;;         [abcabc abc2] -> 20
;;;         [abcabc abc3] -> nil
;;;         [abcabc set-abc3 30] -> 30
;;;         [abcabc abc3] -> 30
;;;         [abcabc set-abc2 200] -> 200
;;;         [abcabc abc2] -> 200
;;;         [abcabc set-abc1 100]   エラーを起こす。


(setf (documentation :unify-next-element T)
      ":unify-next-element                    メッセージ

<説明>
  形式 : :unify-next-element
ユーザは目的を満たすためにメッセージ :unify-next-elemnt を定義しなけれ
ばならない。インスタンス P があるリスト Q とユニファイされるとき、
:unify-next-element は P に送られ、返値 R は Q に再びユニファイされる。
もとのユニフィケーションの評価結果は R と Q とのユニフィケーションの結
果。インスタンスとリストとの間でユニフィケーションが起こったとき、
:unify-next-element が定義されていないとエラーとなることに注意。また、
2 番目の引数は関数 == を用いたインスタンスでなければならない。

<例>
        (defclass abc () ()) -> abc
        (!x (make-instance 'abc)) -> {udo}81699abc
        (== (a b c) ,x)
        (defmethod (abc :unify-next-element) () (cons 'a '(b c)))
        -> :unify-next-element
        (== (a b c) ,x) -> t
上記の例で、インスタンス x とリスト (a b c) との間にユニフィケーション
が起こると、:unify-next-element は x に送られる。メッセージ送りの結果
リスト (a b c) となり、これは == の最初の引数にユニファイされる。")

(defun tao:< (x y)
  #.(string '#:|<                                      関数[!#subr]

<説明>
 (1)
  形式 : string1 < string2 < ... < stringN
string1 string2 ... stringN を辞書順に比較し、完全に昇順に並んでいる
場合は、stringN の値を返す。そうでなければnil を返す。

 (2)
  形式 : < number1 number2
number1 の値が number2 の値より小さければ number2 の評価値を返し、
それ以外なら nil を返す。引数が 1 つのときは、その評価値を返す。
infix notation では、任意個の引数が指定可能。

<例>
 (1)
        ("abacus" < "abdomen") -> "abdomen"
        ("a" < "b" < "c" < "d" < "e") -> "e"
        ("a" < "b" < "b" < "d" < "e") -> nil
        ("あ" < "か" < "さ" < "た") -> "た"
        ("あえお" < "あいう") -> nil
 (2)
        (< 4 5) -> 5
        (< 5 4) -> nil
        (< 0) -> 0|)
  (etypecase x
    (string (etypecase y
              (string (and (string< x y)
                           y))))
    (number (etypecase y
              (number (and (cl:< x y)
                           y))))))

(defsynonym common:< cl:<
  "common:<                               関数[#!subr]

<説明>
  形式 : common:< number1 &rest number2 number3 ... numberN
number1 number2 ... numberN の値 (複素数も可) を左から右に順に比較し、
完全に単調増加している (等しい値もない) なら、numberN の値を返し、
それ以外なら nil を返す。

<例>
        (common:< 0 1 2 3 4 5) -> 5
        (common:< 0 1 2 4 4 5) -> nil")

(defmacro tao:<- (loc object)
  "<-                                     メッセージ

<説明>
  形式 : <- object
メッセージ受け渡し式 (locative <- object) を記述するために使用され、
object の値を locative に代入し、代入した値を返す。
object では、Fortran 型、Lisp 型の両方の式が許される。ただし object が
ロカティブなら、指定された値が locative に代入される。

<例>
        (signed-integer-locatives x y) -> (x y)
        (x <- ((2 + 3) * 4)) -> 20
        (y <- 30) -> 30
        x -> 20
        y -> 30
        (x <- y) -> 30
        x -> 30
        (x <- 20) -> 20
        x -> 20
        y -> 30
        (!y x) -> 20
        y -> 20
        (x <- 30) -> 30
        x -> 30
        y -> 30"
  #+lispworks
  (lw:rebinding (object)
    `(if (fli:pointerp ,object)
         (setf (fli:dereference ,loc) (fli:dereference ,object))
         (setf (fli:dereference ,loc) ,object))))


(defun tao:<= (x y)
  (string '#:|<=                                     関数[#!subr]

<説明>
 (1)
  形式 : string1 <= string2 <= ... <= stringN
string1 string2 ... stringN の値を辞書順に比較し、昇順に並んでいる場合
は、stringN の値を返す。そうでなければnil を返す。

 (2)
  形式 : <= number1 number2
number1 の値が number2 の値より小さいか等しければ、number2 の評価値を
返し、それ以外なら nil を返す。  引数が 1 つのときは、その評価値を返す。
infix notation では、任意個の引数が指定可能。

<例>
 (1)
        ("a" <= "b" <= "c" <= "d" <= "e") -> "e"
        ("a" <= "b" <= "b" <= "d" <= "e") -> "e"
        ("a" <= "b" <= "d" <= "c" <= "e") -> nil
        ("あ" <= "か" <= "さ" <= "た") -> "た"
        ("あえお" <= "あいう") -> nil
 (2)
        (<= 4 5) -> 5
        (<= 5 4) -> nil
        (<= 0) -> 0|)
  (etypecase x
    (string (etypecase y
              (string (and (string<= x y)
                           y))))
    (number (etypecase y
              (number (and (cl:<= x y)
                           y))))))

(defsynonym common:<= cl:<=
  "common:<=                              関数[#!subr]

<説明>
  形式 : common:<= number1 &rest number2 number3 ... numberN
number1 number2 ... numberN の値 (複素数でもよい) を左から右に順に比較
し、単調増加している (等しい場合も可) なら、numberN の値を返し、
それ以外なら nil を返す。

<例>
        (common:<= 0 1 2 3 4 5) -> 5
        (common:<= 0 1 2 4 4 5) -> 5")

(defun tao:= (x y)
  #.(string '#:|=                                      関数[!#subr]

<説明>
 (1)
  形式 : string1 = string2 = ... = stringN
string1 string2 ... stringN の値を比較し、全て一致している場合は、
stringN の値を返す。そうでなければnil を返す。

 (2)
  形式 : = number1 number2
number1 の値と number2 の値を比較し、一致していれば、number1 の評価値を
返し、そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
\(number1 = number2)、[number1 = number2] の形式でも記述可能。

<例>
 (1)
        ("abc" = "abc" = "abc") -> "abc"
        ("xyz" = "xyz" = 'xyz) -> "xyz"
        ("あえお" = "あいう") -> nil
        ("あいう" = "あいう") -> "あいう"
 (2)
        (= 4 4) -> 4
        (= 3 4) -> nil
        (= 4) -> 4
        (= "string" "string") -> エラー
        (= nil nil) -> nil|)
  (etypecase x
    (string (etypecase y
              (string (and (string= x y)
                           y))))
    (number (etypecase y
              (number (and (cl:= x y)
                           y))))))

(defsynonym common:= cl:=
  "common:=                               関数[#!subr]

<説明>
  形式 : common:= number1 &rest number2 number3 ... numberN
number1 number2 ... numberN の値 (複素数でも可) を左から右に順に比較し、
全ての値が等しい場合は numberN の値を返し、それ以外なら nil を返す。

<例>
        (common:= 3 3 3 3) -> 3
        (!a '#c(2 3)) -> #c(2 3)
        (common:= a #c(2 3)) -> #c(2 3)
        (common:= 2 2 3) -> nil")


(setf (tao.logic::get-clauses 'tao:==) T)
(defmacro tao:== (arg1 arg2)
  "==                                     関数[#!&+]
<説明>

形式 : == _arg1 _arg2
_arg1 と ＿arg2 がユニファイされる。
ユニファイが成功すると、t を返し、そうでなければ nil を返す。
_arg1 _arg2 は、論理変数であれば、ユニフィケイションの前に評価されるが、
そうでなければ、評価されない。コンマの次に来る Lisp 変数や Lisp 関数は、
ユニフィケイションの前に評価される。

<例>
        (prog (_x _y) (== (_x . _y) (1 2 3)) -> t.
        現在 _x は (1 2 3) の car になり、同時に _y は (1 2 3) の cdr
        となっている。"
  (let ((cont (gensym "cont")))
    `(with-return-from-reval ,cont (nil ,arg1 ,arg2)
       (tao.logic::==/2 ,(unquotify arg1) ,(unquotify arg2)
                        #',cont))))


(defun tao:> (x y)
  #.(string '#:|>                                      関数[#!subr]

<説明>
 (1)
  形式 : string1 > string2 > ... > stringN
string1 string2 ... stringN の値を辞書順に比較し、完全に降順に並んで
いる場合は、stringN の値を返す。そうでなければ nil を返す。

 (2)
  形式 : > number1 number2
number1 の値が number2 の値より大きいなら、number2 の評価値を返し、
そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
\(number1 > number2)、[number1 > number2] の形式でも記述可能。
infix notation では、任意個の引数が指定可能。

<例>
 (1)
        ("story" > 'store) -> "store"
        ("z" > "y" > "x" > "w" > "v" > "u") -> "u"
        ("z" > "y" > "y" > "w" > "v" > "u") -> "nil"
        ("あいう" > "あえお") -> "nil"
        ("あえお" > "あいう") -> "あえお"
 (2)
        (> 5 4) -> 4
        (> 4 5) -> nil
        (> 5) -> 5|)
  (etypecase x
    (string (etypecase y
              (string (and (string> x y)
                           y))))
    (number (etypecase y
              (number (and (cl:> x y)
                           y))))))

(defsynonym common:> cl:>
  "common:>                               関数[#!subr]

<説明>
  形式 : common:> number1 &rest number2 number3 ... numberN
number1 number2 ... numberN の値 (複素数も可) を左から右に順に比較し、
完全に単純減少している場合は、numberN の値を返し、そうでなければ nil
を返す。

<例>
        (common:> 5 4 3 2 1 0) -> 0
        (common:> 5 4 4 2 1 0) -> nil")

(defun tao:>= (x y)
  #.(string '#:|>=                                     関数[#!subr]

\<説明>
 (1)
  形式 : string1 >= string2 >= ... >= stringN
string1 string2 ... stringN の値を辞書順に比較し、降順に並んでいる
\(等しい場合を含む) 場合は、stringN の値を返す。
そうでなければ nil を返す。

 (2)
  形式 : >= number1 number2
number1 の値が number2 の値より大きいか等しいなら、number2 の評価値を
返し、そうでなければ nil を返す。引数が 1 つのときは、その評価値を返す。
\(number1 >= number2)、[number1 >= number2] の形式でも記述可能。
infix notation では、任意個の引数が指定可能。

\<例>
\(1)
\("z" >= "y" >= "x" >= "w" >= "v" >= "u") -> "u"
\("z" >= "y" >= "y" >= "w" >= "v" >= "v") -> "v"
\("z" >= "y" >= "y" >= "u" >= "v" >= "v") -> nil
\("た" >= "さ" "" >= "か" >= "あ") -> "あ"
\("させそ" >= "さそせ") -> nil
\(2)
\(>= 5 4) -> 4
\(>= 4 5) -> nil
\(>= 5) -> 5|)
  (etypecase x
    (string (etypecase y
              (string (and (string>= x y)
                           y))))
    (number (etypecase y
              (number (and (cl:>= x y)
                           y))))))

(defsynonym common:>= cl:>=
  "common:>=                              関数[#!subr]

<説明>
  形式 : common:>= number1 &rest number2 number2 ... numberN
number1 number2 ... numberN の値 (複素数も可) を左から右に順に比較し、
単純減少 (等しいものがあってもよい) している場合は、numberN の値を返し、
そうでなければ nil を返す。

<例>
        (common:>= 5 4 3 2 1 0) -> 0
        (common:>= 5 4 4 2 1 0) -> 0")


(defsynonym tao:\\ cl:mod
  "\                                      関数[#!macro]

 <説明>
   形式 : \ number1 number2
 number1 の値を number2 の値で割り、その剰余を返す。
 (\ x y) = (mod x y)
 演算符号 \ がエスケープコードとして使用されている場合は \\ を使う。
 (\ 14 4) は ELIS ではエラーを起こすので注意。

\\                                     関数[#!macro]

<説明>
  形式 : \\ number1 number2
number1 の値を number2 の値で割り、その剰余を返す。
\(\\ x y) = (mod x y)

<例>
        (\\ 5 3) -> 2
        (\\ 0 3) -> 0
        (\\ -4 2) -> 0
        (\\ 4 0) -> エラー")

(define-symbol-macro tao:_ (tao:undef))
(setf (documentation 'tao:_ 'function)
      "_                                      特殊変数名

<説明>
  1 つの下線 _ は、あらゆるものをユニファイする名前なしの論理変数を表す。")

;;; ~                                      関数[#!macro]
;;; <説明>
;;;   形式 : ~   &rest body
;;; (~ [(&aux var ...)] B1) は論理 "not" である。
;;; B1 においてすべてのバックトラックが試されたあと、B1 が nil と評価される
;;; と (~B1) は t となる。B1 が t と評価されると (~B1) は nil となる。
