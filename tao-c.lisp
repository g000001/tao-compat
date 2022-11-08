(tao:tao)
(in-package #:tao-internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ＠
;;; caaaar                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caaaar list
;;; (caaaar list) = (car (car (car (car list))))。
;;;
;;; <例>
;;;         (caaaar '((((a))))) -> a
;;;         (caaaar '(((((a b)))))) -> (a b)
;;;         (caaaar '((((nil))))) -> nil
;;;         (caaaar '((((()))))) -> ()
;;; ＠
;;; caaadr                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caaadr list
;;; (caaadr list) = (car (car (car (cdr list))))。
;;;
;;; <例>
;;;         (caaadr '(a ((b (c (d (e))))))) -> b
;;; ＠
;;; caaar                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caaar list
;;; (caaar list) = (car (car (car list)))。
;;;
;;; <例>
;;;         (caaar '(((b)))) -> b
;;; ＠
;;; caadar                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caadar list
;;; (caadar list) = (car (car (cdr (car list))))
;;;
;;; <例>
;;;         (caadar '((a b c d e))) -> c
;;; ＠
;;; caaddr                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caaddr list
;;; (caaddr list) = (car (car (cdr (cdr list))))
;;;
;;; <例>
;;;         (caaddr '((a) (b) (c) (d) (e))) -> c
;;; ＠
;;; caadr                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caadr list
;;; (caadr list) = (car (car (cdr list)))
;;;
;;; <例>
;;;         (caadr '((a) (b) (c) (d) (e))) -> (c)
;;; ＠
;;; caar                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caar list
;;; (caar list) = (car (car list))。
;;;
;;; <例>
;;;         (caar '(((a b)))) -> (a b)
;;; ＠
;;; cadaar                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cadaar list
;;; (cadaar list) = (car (cdr (car (car list))))。
;;;
;;; <例>
;;;         (cadaar '((((a) (b) (c))))) -> (b)
;;; ＠
;;; cadadr                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cadadr list
;;; (cadadr list) = (car (cdr (car (cdr list))))。
;;;
;;; <例>
;;;         (cadadr '((a) ((b) c) (d) (e) (f))) -> (c)
;;; ＠
;;; cadar                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cadar list
;;; (cadar list) = (car (cdr (car list)))。
;;;
;;; <例>
;;;         (cadar '((a (b) (c)))) -> (b)

(defun tao:cadblep (object)
  "cadblep                                関数[#!subr]

<説明>
  形式 : cadblep object
object が car 関数でも cdr 関数でもエラーにならないならば、評価値を返し、
それ以外なら nil を返す。

<例>
        (cadblep '(a b c)) -> (a b c)
        (cadblep \"ultra-q\") -> nil
        (cadblep 'seven) -> nil
        (cadblep nil) -> nil"
  (and (typep object 'list)
       object))

;;; ＠
;;; caddar                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caddar list
;;; (caddar list) = (car (cdr (cdr (car list))))。
;;;
;;; <例>
;;;         (caddar '(a (b) (c))) -> (c)
;;; ＠
;;; cadddr                                 関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cadddr list
;;; (cadddr list) = (car (cdr (cdr (cdr list))))。
;;;
;;; <例>
;;;         (cadddr '(a b c (d e))) -> (d e)
;;; ＠
;;; caddr                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : caddr list
;;; (caddr list) = (car (cdr (cdr list)))。
;;;
;;; <例>
;;;         (caddr '(a b c)) -> c
;;; ＠
;;; cadr                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cadr list
;;; (cadr list) = (car (cdr list))。
;;;
;;; <例>
;;;         (cadr '(a (b))) -> (b)
;;; ＠
;;; call-arguments-limit                   定数
;;;
;;; <説明>
;;;   関数に渡し得る引数の上限を示す正の整数で、128。
;;; ＠
;;; car                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : car list
;;; list の第 1 要素 (car 部) を返す。list は cadble (carおよび cdr をとる
;;; ことができる) でなければならない。nil に適用することはできない。
;;; 予め、フォーム (set-sysmode :car-nil-error nil) を実行しておけばエラー
;;; を返さないで nil を返す。
;;;   関数 cdr、car と cdr の合成関数 (car や cdr 関数処理を 4 回まで続け
;;; て行う場合には、そのすべての組み合わせについて個々に関数が定義されて
;;; おり、その名前は c で始まり r で終わる。 c と r の間は car
;;; オペレーションに対して a、cdrオペレーションに対して d を指定する)。
;;;
;;; <例>
;;;         (car '(a b c)) -> a
;;;         (car nil)   エラーが警告される
;;; ＠
;;; case                                   関数[#!macro]
;;;
;;; <説明>
;;;   形式 : case key-form (key-list1 form11 form12 ...)
;;;                    (key-list2 form21 form22 ...)
;;;                     ...
;;;                        (key-listM formM1 formM2 ... formMN)
;;; key-list1 key-list2 ... を逐次検索し、key-form の評価された値と eql な
;;; 要素をリスト key-listI の中に見つけると、対応するフォーム formI1
;;; formI2 ... を評価する。見つからないときは、nil を返す。最後のリスト
;;; key-listM として t または otherwise を指定し、それまでの全てが、eql で
;;; なかった時は、formM1 formM2 ... formMN を評価し、formMN の評価結果を
;;; 返す。実行する節の選択方法以外は、cond 関数とほぼ同じ。
;;;
;;; <例>
;;;         (defun foo (x)
;;;          (case x
;;;           ((a b) 1)
;;;           (c 2)
;;;           ((otherwise) 3)
;;;           ((t) 4)
;;;           (nil 5)
;;;           ((nil) 6)
;;;           (otherwise 7)))
;;;         (foo 'a) -> 1
;;;         (foo 'b) -> 1
;;;         (foo 'c) -> 2
;;;         (foo 'otherwise) -> 3
;;;         (foo t) -> 4
;;;         (foo nil) -> 6
;;;         (foo 'bar) -> 7
;;; ＠

(defmacro tao:caseq (key &body cases)
  "caseq                                  関数[#!macro]

<説明>
  形式 : caseq object (case1 form11 form12 ...)
                      (case2 form21 form22 ...)
                      ...
                      (caseM formM1 formM2 ... formMN)
順に case1 case2 ... が object と eq かどうか調べていき、eq になると、
その後にくるフォームを順に評価し、その最後のフォームの評価結果を返す。
1 番最後の caseM を t または otherwise に指定した場合、それまでの全てが、
eq でなかった時は、formM1 formM2 ... formMN を評価する。caseI がリスト
のときには関数 eq のかわりに関数 memq でテストを行なう。
つまり (memq object caseI) の評価結果が nil でない値なら、その後にくる
フォームを評価する。selectq 関数 (同一機能)

<例>
        (caseq 3 ((3 4) 'abc) (t 'xyz)) -> abc
        (caseq 'a ((b c d) 'ng) (otherwise 'ok)) -> ok
        (caseq 'd ((a) \"a\") ((b) \"b\") ((c) \"c\")) -> nil"
  (let ((k (gensym)))
    `(let ((,k ,key))
       (declare (ignorable ,k))
       (cond ,@(mapcar (lambda (xx)
                         (if (member (car xx) '(otherwise t) :test #'eq)
                             `('T ,@(cdr xx))
                             (if (consp xx)
                                 `((member ,k ',(car xx) :test #'eq) ,@(cdr xx))
                                 `((eq ,k ',(car xx)) ,@(cdr xx)))))
                       cases)))))
#|
 (case 'foo
  (foo 'foo)
  (otherwise 'bar))

 (caseq 'foo
  ((foo bar baz) 'bar)
  (otherwise 'zot))

 (caseq 'foo
  (foo 'bar)
  (t 'zot))

 (caseq 'foo
  (foo 'bar)
  ('t 'zot))

 (caseq 'foo
  (foo 'bar)
  (otherwise 'zot))
|#

;;; ＠
;;; catch                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : catch  tag &rest form1 form2 ... formN
;;; form1 form2 ... formN を評価し、formN の値を返す。評価中に tag の値と
;;; eq なキャッチタグを指定する throw 式が現れたら、評価を終了し、throw 式
;;; の指定する値を返す。
;;;
;;; <例>
;;;         (catch 'a (throw 'a 3)) -> 3
;;;         y = 2    a = (b c)
;;;         (catch a (seq (!x 1) (throw a y) (!z 3))) -> 2
;;;         x = 1   y = 2  z = unbound  しかし
;;;         (catch a (seq (!x 1) (throw '(b c) y) (!z 3))) は エラー 。
;;; ＠

(defmacro catcher (tag form &body receiver-forms)
  "catcher                                関数[#!subr]

<説明>
  形式 : catcher tag form receiver1 receiver2 ... recieverN
 (throw  tag  val) が form を評価してゆく途中で現れたら、val に束縛され
た変数 tag を用いてレシーバ receiver1 receiver2 ... recieverN を逐次
評価し、recieverN の評価値を返す。レシーバが省略されると val を返す。
throw が現れなかった場合、form を単純に評価するのと同じ。

<例>
        もし z = (p q r), ならば
        (catcher 'a (seq (!x 1) (throw 'a z)) (list 1 2 3)) -> (1 2 3)
        (catcher 'a (seq (!a '(e f g)) (throw 'a z)) (car a)) -> p
        (catcher 'a (seq (!x 1) (throw 'a z)) ) -> (p q r)
        (catcher 'a (seq (!x 1) (throw 'a)) (!y 3)) -> 3
        (catcher 'a (seq (!x 1) (throw 'a))) -> {undef}0
        (catcher 'exception
           (do-work)
           (case (car exception)
                 (unexpected-response ...)
                 (non-number-argument ...)
                 (table-overflow ...)
                 (not-implemented-yet ...) ))
        この例ではレシーバ (case (car ...) ...) がある。
        (do-work) にある throw は
        (throw 'exception (list 'unexpected-response some-value))
        とする。throw が (do-work) の評価中に起こった場合
        (case (car...)...) が評価される。"
  (let ((ft (find-throw form)))
    (cond ((and ft receiver-forms)
           `(let ((,(cadr tag) (catch ,tag ,form)))
              ,@receiver-forms))
          ('T `(catch ,tag ,form)))))

;; helper

#| Ver. 1
 (defun find-throw (form)
  (if (eq 'throw (car form))
      (values t (cadr form))
      (dolist (c (remove-if-not #'consp form) nil)
        (multiple-value-bind (p tag) (find-throw c)
          (and p (return (values t tag)))))))
|#

;; Ver. 3
(defun find-throw (form &optional taglist)
  (if (eq 'throw (car form))
      (push (cadr form) taglist)
      (dolist (c (remove-if-not #'consp form) taglist)
        (let ((tem (find-throw c taglist)))
          (when tem
            (setq taglist tem))))))

#|
 (catcher 'a (throw 'a "f"))

 (catcher 'a
    (progn
      (princ "Hello,")
      (princ "World! ")
      (throw 'a 'foo))
  (princ a)
  'b)
|#

#|
 (find-throw '((foo)  (((((((throw 'a))))))) throw ))
 (find-throw '(foo (((((((throw 'a)))))))  ))

 (find-throw '(x (((throw 'a)))))
|#


#|
 (catch 'a
  (princ "Hello")
  (princ "World!")
  (throw 'a '("Again" foo bar baz))
  )

 (catcher 'a
    (progn
      (princ "Hello,")
      (princ "World! ")
)
  (princ a))
|#


;; レシーバー部がない場合はnilを返すことにしてみた。
(defmacro catcher-case (form &rest receiver-cases)
  "catcher-case                           関数[#!subr]

<説明>
  形式 : (catcher-case form (tag1 receiver11 receiver12 ...)
                            (tag2 receiver21 receiver22 ...) ...)
catcher と selectq が結合した関数。
tagI が form の評価中に (throw tagI val) という形で出現した場合、
 (catcher tagI form receiverI1 receiverI2 ...) と等価な動作となる。
最後の tagN が t のとき、tag1 tag2 ... tag(N-1) がマッチしない他の
throw はすべて tagN が受け取り、tagN に結び付けられたレシーバ中の
変数 caught-value は throw の value に束縛される。caught-value 参照。

<例>
        (catcher-case
           (do-work)
           (unexpected-response ...)
           (non-number-argument ...)
           (table-overflow ...)
           (not-implemented-yet ...)
           (t ...) )"
  (let ((ft (find-throw form))
        (tag (gensym)))
    (cond ((and ft receiver-cases)
           `(multiple-value-bind (caught-value ,tag) (catch-x ,form)
              (case ,tag
                ,@receiver-cases)))
          ('T `,form))))

;; Helper catcher-case
(defmacro catch-x (&body form)
  (let ((tags (find-throw form))
        (ctag (gensym)))
    (if tags
        (let ((result `(prog1 (catch ,(car tags) ,@form) (setq ,ctag ,(car tags)))))
          (dolist (tg (cdr tags) `(let (,ctag) (values ,result ,ctag)))
            (setq result `(prog1 (catch ,tg ,result) (setq ,ctag ,tg)))))
        `(values (progn ,@form) nil))))

;;; caught-value                           変数
;;;
;;; <説明>
;;;   関数 catcher と catcher-case の中で使う。
;;;
;;; <例>
;;;         (catcher-case ((!x ((a * (x ** 2)) + (b * x) + c))
;;;                        (cond ((x > 100) (throw 'p 100))
;;;                              ((100 >= x > 50) (throw 'q 75))
;;;                              ((50 >= x >=0) (throw 'r 25))
;;;                              (t (throw 's 0))))
;;;                       ('q (!!+1 !z75) (!!+ !sum75 q))
;;;                       ('r (!!+1 !z25) (!!+ !sum25 r))
;;;                       (t (!!+1 !z0) (!!+ !sum0 caught-value)))


;; ＠
;; ccase                                  関数[#!macro]

;; <説明>
;;   形式 : ccase keyplace
;;                (keylist-1 form11 form12 ... form1N)
;;                (keylist-2 form21 ...)
;;              ...
;;                (keylist-M formM1 ...)
;; 形式 keyplace を評価し、それが、keylist の要素と eql な節を選択し、その
;; 節のフォームを順に評価し、最後のフォームのリターン値を返す。
;; 満足されるような節がなければ、継続可能なエラーを警告する。エラーから
;; 継続するために、新しい値を受け入れ、それを keyplace に置く。そして、
;; テストを再び行う。keyplace の副形式は何回も評価され得る。keyplace は、
;; 関数 setf に受け入れ可能な汎変数参照でなければならない。関数 case に
;; 似ているけれども、陽な otherwise 句あるいは t 句は許されない。

;; <例>
;;         (defun test (x)
;;              (ccase x (a 10)
;;                       (b 20))) -> test
;;         (test 'a) -> 10
;;         (test 'b) -> 20
;;         (test 'c) -> (not-implemnted-yet ccase x ((a 10) (b 20))))
;; ＠
;; cdaaar                                 関数[#!subr]

;; <説明>
;;   形式 : cdaaar list
;; (cdaaar list) = (cdr (car (car (car list))))。

;; <例>
;;         (cdaaar '((((a (b c)))))) -> ((b c))
;; ＠
;; cdaadr                                 関数[#!subr]

;; <説明>
;;   形式 : cdaadr list
;; (cdaadr list) = (cdr (car (car (cdr list))))。

;; <例>
;;         (cdaadr '(a ((b c) d))) -> (c)
;; ＠
;; cdaar                                  関数[#!subr]

;; <説明>
;;   形式 : cdaar list
;; (cdaar list) = (cdr (car (car list)))。

;; <例>
;;         (cdaar '(((a b c)))) -> (b c)
;; ＠
;; cdadar                                 関数[#!subr]

;; <説明>
;;   形式 : cdadar list
;; (cdadar list) = (cdr (car (cdr (car list))))。

;; <例>
;;         (cdadar '((a (b c) d))) -> (c)
;; ＠
;; cdaddr                                 関数[#!subr]

;; <説明>
;;   形式 : cdaddr list
;; (cdaddr list) = (cdr (car (cdr (cdr list))))。

;; <例>
;;         (cdaddr '(a b (c d) e)) -> (d)
;; ＠
;; cdadr                                  関数[#!subr]

;; <説明>
;;   形式 : cdadr list
;; (cdadr list) = (cdr (car (cdr list)))。

;; <例>
;;         (cdadr '(a (b c (d)))) -> (c (d))
;; ＠
;; cdar                                   関数[#!subr]

;; <説明>
;;   形式 : cdar list
;; (cdar list) = (cdr (car list))。

;; <例>
;;         (cdar '((a b))) -> (b)
;; ＠
;; cddaar                                 関数[#!subr]

;; <説明>
;;   形式 : cddaar list
;; (cddaar list) = (cdr (cdr (car (car list))))。

;; <例>
;;         (cddaar '(((a b c) d)))) -> c
;; ＠
;; cddadr                                 関数[#!subr]

;; <説明>
;;   形式 : cddadr list
;; (cddadr list) = (cdr (cdr (car (cdr list))))。

;; <例>
;;         (cddadr '(a (b c d e))) -> (d e)
;; ＠
;; cddar                                  関数[#!subr]

;; <説明>
;;   形式 : cddar list
;; (cddar list) = (cdr (cdr (car list)))。

;; <例>
;;         (cddar '((a b c))) -> (c)
;; ＠
;; cdddar                                 関数[#!subr]

;; <説明>
;;   形式 : cdddar list
;; (cdddar list) = (cdr (cdr (cdr (car list))))。

;; <例>
;;         (cdddar '((a b c d)))) -> (c d)
;; ＠
;; cddddr                                 関数[#!subr]

;; <説明>
;;   形式 : cddddr list
;; (cddddr list) = (cdr (cdr (cdr (cdr list))))。

;; <例>
;;         (cddddr '(a b c d e)) -> (e)
;; ＠
;; cdddr                                  関数[#!subr]

;; <説明>
;;   形式 : cdddr list
;; (cdddr list) = (cdr (cdr (cdr list)))。

;; <例>
;;         (cdddr '(a b c d)) -> (d)
;; ＠
;; cddr                                   関数[#!subr]

;; <説明>
;;   形式 : cddr list
;; (cddr list) = (cdr (cdr list))。

;; <例>
;;         (cddr '(a b c)) -> (c)
;; ＠
;; cdr                                    関数[#!subr]

;; <説明>
;;   形式 : cdr list
;; list からその第 1 要素を除いてできるリスト (リストの cdr 部) を返す。
;; list は cadble (car および cdr をとることができる) でなければならない。
;;   関数 car、car と cdr の合成関数 (car や cdr 関数処理を 4 回まで続けて
;; 行う場合には、そのすべての組み合わせについて個々に関数が定義されており、
;; その名前は c で始まり r で終わる。 c と r の間は car オペレーションに
;; 対して a、cdrオペレーションに対して d を指定する)。
;; nil に適用できる。

;; <例>
;;         (cdr '(a b c)) -> (b c)
;;         (cdr '(a (bc d) ((e) (f g) h))) -> ((bc d) ((e) (f g) h)))
;;         (cdr nil) -> ()
;;         (cdr '(nil)) -> ()
;; ＠

;; cdr!                                   関数[#!subr]
(defmacro tao:cdr! (list)
  "<説明>
  形式 : cdr! list
\(cdr! list) = (!list (cdr list)) 。
ただし左辺では list は 1 度しか評価されない。
<例>
        x が (a b c) のとき
        (cdr! x) -> (b c)"
  `(setq ,list (cdr ,list)))

;;; ＠
;;; ceiling                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : ceiling number1 &opt number2
;;; number1 / number2 以上の最小の整数を第 1 の値として、
;;; (- number1 (* "第 1 の値" number2)) を第 2 の値として 返す。引数が 1 つ
;;; の場合、その値に対して同様の処理をする(割り算は行わない)。
;;;
;;; <例>
;;;         (ceiling 2.5) -> !(3 -0.50)
;;;         (ceiling -2.6) -> !(-2 -0.600007)
;;;         (ceiling 3.9 4.2) -> !(1 -0.300019)
;;; ＠
;;; cell                                   クラス
;;;
;;; <説明>
;;;   インスタンスは (a b c), (1 . 2) のようなコンス。
;;; cell (32ビットワードのペア) には、最初のワードにデータが格納され (car)、
;;; 2 番目のワードに次の cell へのポインタ (cdr) がある。
;;;         第 1 cell = 第 2 cell への p ポインタ
;;;         第 2 cell = 第 3 cell への q ポインタ
;;;         第 3 cell = r  nil
;;; ＠

(defun tao:cellp (object)
  "cellp                                  関数[#!subr]

<説明>
  形式 : cellp object
object がセル又は名前付きセルならば、評価値を返し、それ以外なら nil を
返す。

<例>
        (cellp nil) -> nil
        (cellp ()) -> nil
        (cellp '(a b c)) -> (a b c)
        (cellp 'fn(x y)) -> fn(x y) if *fn-notation* is non-nil.
        (cellp ''(a b c)) -> nil
        (cellp '^(a b c)) -> nil
        (cellp '[a b c]) -> nil"
  ;;--- TODO
  (consp object))

;;; ＠
;;; cerror                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : cerror string1 string2 &rest string3
;;; エラーを報告し、デバッガに入るが、エラーを解決した後に、デバッガから
;;; 再び継続することを認める。継続する時、nil を返し、その後、この関数の
;;; 呼び出し以下のプログラムコードを実行する。string1 と string2 は
;;; string3 とともに、メッセージ文字列を出力するための制御文字列として、
;;; 関数 format に対して与えられる。string2 でエラーメッセージ文字列が指定
;;; され、string1 で継続メッセージ文字列が指定される。
;;;
;;; <例>
;;;         (defun test (x)
;;;             (cond ((= x 1)
;;;                    (cerror "continued message" "error message")))
;;;         -> test
;;;         (test 1) -> (not-implemented-yet cerror
;;;                     ("continued mesage" "error message" ())
;;; ＠
;;; change-font                            関数[#!subr]
;;;
;;; <説明>
;;;   形式 : change-font fatstring font op &opt integer1 integer2
;;; ファットストリング文字列 fatstring に、処理 op に従い、フォント情報
;;; font を加える。integer1 integer2 が指定されると、fatstring のうち
;;; integer1 から (integer2 - 1) 番目までの文字にフォント情報が加えられる。
;;; (1 番目の文字を示す番号は 0 )。font は、数で与えられ、ビットパターンは、
;;; 次のようになる。
;;;   全ビットがオフ  (引数 font = 0)  通常のフォント
;;;   bit-0  がオン   (引数 font = 1)  反転のフォント
;;;   bit-1  がオン   (引数 font = 2)  ブリンクのフォント
;;;   bit-2  がオン   (引数 font = 4)  アンダーラインが引かれる
;;;   bit-3  がオン   (引数 font = 8)  強調 (明るくなる) フォント
;;; これらのフォントすべての組み合わせも可能。例えば、font が 7 なら、
;;; そのキャラクタは、反転し、ブリンクし、アンダーラインが引かれる。処理
;;; op が :and :or :xor :mask で指定されたときは、font と fatstring の
;;; フォント情報の間の論理処理。:to は、古いフォント情報をクリアし、font を
;;; セットする。
;;;
;;; <例>
;;;         (!aa (make-fatstring "abcdefghij" 1)) -> "abcdefghij"
;;;         すべてのキャラクタは、反転
;;;         (change-font aa 2 :to 0 3) -> "abcdefghij"
;;;         "a" "b" "c" は、反転しないでブリンク
;;;         その他は、反転
;;;         (change-font aa 2 :or 3 6) -> "abcdefghij"
;;;         "a" "b" "c" は、ブリンク
;;;         "d" "e" "f" は、反転してブリンク
;;;         (change-font aa 2 :and 6 9) -> "abcdefghij"
;;;         "a" "b" "c" は、ブリンク
;;;         "d" "e" "f" は、反転してブリンク
;;;         "g" "h" "i" は、通常
;;; ＠

(defun tao:char (string index)
  "char                                   関数[#!subr]

<説明>
  形式 : char string index
文字列 string の文字位置 index (0 から始まる) の文字を返す。index は、
string の長さより小さくなければならない。

<例>
        (char \"abcdefghij\" 0) -> \"a\"
        (char \"abcdefghij\" 3) -> \"d\"
        (char \"おはようざいます\" 2) -> \"よ\"
        (char \"asd\" 5) -> \"\""
  (if (< index (length string))
      (cl:char string index)
      ""))


;;; ＠
;;; char-bit                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : char-bit char name
;;; 文字 char のビット属性のうち、ビット名 name が示すビットがセットされて
;;; いれば nil でない値を返し、そうでなければ nil を返す。
;;; ＠
;;; char-bits                              関数[#!subst]
;;;
;;; <説明>
;;;   形式 : char-bits char
;;; 文字 char のビット属性を示す数を返す。
;;;
;;; <例>
;;;         (char-bits #¥a) -> 0
;;; ＠
;;; char-bits-limit                        定数
;;;
;;; <説明>
;;;   char-bits 関数によって生成される値の上限値 (この値を含まない)。
;;; TAO システムの場合は 1 。
;;; ＠
;;; char-code                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-code char
;;; 文字 char のコード属性を示す数を返す。
;;;
;;; <例>
;;;         (char-code #¥a) -> 97
;;;         (char-code #¥A) -> 65
;;; ＠
;;; char-code-limit                        定数
;;;
;;; <説明>
;;;   システムで定められた文字の属性を表す数の上限値であり、TAO システムの
;;; 場合は 65536 。char-code 関数で返される値はこの値を越えてはいけない。
;;; ＠
;;; char-code-p                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : char-code-p code
;;; code がキャラクタコードなら、code を返し、それ以外なら、nil を返す。
;;;
;;; <例>
;;;         (char-code-p 10) -> 10
;;;         (char-code-p 127) -> 127
;;;         (char-code-p 128) -> nil
;;;         (char-code-p 97) -> 97
;;;         (char-code-p 52455) -> 52455
;;; ＠
;;; char-control-bit   未インプリメント    定数
;;;
;;; <説明>
;;;   char-control-bit = 0
;;; ＠
;;; char-downcase                          関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-downcase char
;;; 文字 char が大文字を表す文字データなら、対応する小文字を表す文字データ
;;; を返す。そうでなければ char を返す。
;;;
;;; <例>
;;;         (char-downcase "A") -> "a"
;;;         (char-downcase "a") -> "a"
;;; ＠
;;; char-equal                             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-equal char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に等しければ charN、等しくなければ nil を返す。
;;;
;;; <例>
;;;         (char-equal #¥a #¥control-a) -> nil.
;;;         (char-equal #¥a #¥A) -> "A"
;;;         (char-equal #¥A #¥a) -> "a"
;;; ＠
;;; char-font                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-font char
;;; 文字 char のフォント属性を示す数を返す。
;;;
;;; <例>
;;;         (!aa (make-fatstring "a" 2) -> "a" (点滅)
;;;         (char-font aa) -> 2
;;; ＠
;;; char-font-limit                        定数
;;;
;;; <説明>
;;;   関数 char-font によって生成される値の上限値を示す非負の整数。
;;; TAO システムの場合は 128 。
;;; ＠
;;; char-greaterp                          関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-greaterp char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コ-ド的、ビット的、及びフォント的
;;; に char1 が大きければ charN、それ以外なら、nil を返す。
;;;
;;; <例>
;;;         (char-greaterp #¥a #¥A) -> nil
;;;         (char-greaterp #¥a #¥b) -> nil
;;;         (char-greaterp #¥b #¥a) -> "a"
;;; ＠
;;; char-hyper-bit     未インプリメント    定数
;;;
;;; <説明>
;;;    char-hyper-bit = 0
;;; ＠
;;; char-int                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-int char
;;; 文字 char を符号化した非負の整数を返す (char のフォント属性と、ビット
;;; 属性が 0 の場合、char-code 関数と同じ整数を返す)。文字のハッシングの
;;; ために使用する。
;;;
;;; <例>
;;;         (char-int "a") -> 97
;;;         (char-int "A") -> 65
;;; ＠
;;; char-lessp                             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-lessp char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コ-ド的、ビット的、及びフォント的
;;; に char1 が小さければ charN、それ以外なら、nil を返す。
;;;
;;; <例>
;;;         (char-lessp #¥b #¥a) -> nil
;;;         (char-lessp #¥a #¥b) -> "b"
;;; ＠
;;; char-meta-bit      未インプリメント    定数
;;;
;;; <説明>
;;;   char-meta-bit = 0
;;; ＠
;;; char-name                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : char-name char
;;; 文字 char が名前をもっていればその名前を、持っていなければ nil を返す。
;;; 標準の改行と空白文字はそれぞれ Newline 及び Space という名前を持つ。
;;; また、準標準文字も Tab、Page、Rubout、Linefeed、Return、Backspace 等の
;;; 名前を持つ。
;;;
;;; <例>
;;;         (char-name #¥a) -> nil
;;;         (char-name #¥Space) -> "Space"
;;;         (char-name #¥Backspace) -> "Backspace"
;;; ＠
;;; char-not-equal                         関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-not-equal char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に等しくなければ charN、等しければ nil を返す。
;;;
;;; <例>
;;;         (char-not-equal #¥a #¥A) -> nil
;;;         (char-not-equal #¥a #¥a) -> nil
;;;         (char-not-equal #¥a #¥b) -> "b"
;;; ＠
;;; char-not-greaterp  未インプリメント    関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-not-greaterp char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に char1 の値が大きくなければ t 、大きいならば nil を返す。
;;; ＠
;;; char-not-lessp     未インプリメント    関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char-not-lessp char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に char1 の値が小さくなければ t 、小さいならば nil を返す。
;;; ＠
;;; char-super-bit     未インプリメント    定数
;;;
;;; <説明>
;;;   char-super-bit = 0
;;; ＠
;;; char-to-strh                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-to-strh object
;;; object を文字列に変換し、その値を返す。
;;;
;;; <例>
;;;         (char-to-strh "a") -> "a"
;;;         (char-to-strh 'ab) -> "ab"
;;; ＠
;;; char-upcase                            関数[#!subr]
;;;
;;; <説明>
;;;   形式 : char-upcase char
;;; 文字 char が小文字を表す文字データなら、対応する大文字を表す文字データ
;;; を返す。そうでなければ、char を返す。
;;;
;;; <例>
;;;         (char-upcase "a") -> "A"
;;;         (char-upcase "A") -> "A"
;;; ＠
;;; char/=                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char/= char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に等しくなければ charN、等しければ nil を返す。
;;;
;;; <例>
;;;         (char/= #¥d #¥d) -> nil
;;;         (char/= #¥d #¥x) -> "x"
;;;         (char/= #¥d #¥D) -> "D"
;;;         (char/= #¥d #¥d #¥d #¥d) -> nil
;;; ＠
;;; char<                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char< char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に char1 の値が小さければ charN、小さくなければ nil を返す。
;;;
;;; <例>
;;;         (char< #¥d #¥x) -> "x"
;;;         (char< #¥d #¥d) -> nil
;;;         (char< #¥a #¥e #¥y #¥z) -> "z"
;;; ＠
;;; char<=                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char<= char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に char1 の値が小さいか等しいなら charN、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (char<= #¥d #¥a) -> nil
;;;         (char<= #¥d #¥x) -> "x"
;;;         (char<= #¥d #¥d) -> "d"
;;;         (char<= #¥a #¥e #¥y #¥z) -> "z"
;;; ＠
;;; char=                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char= char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、及びフォント的
;;; に等しければ charN、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (char= #¥d #¥d) -> "d"
;;;         (char= #¥d #¥x) -> nil
;;;         (char= #¥d #¥D) -> nil
;;;         (char= #¥d #¥d #¥x #¥d) -> nil
;;; ＠
;;; char>                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char> char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、フォント的に
;;; char1 が大きければ charN、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (char> #¥e #¥d) -> "d"
;;;         (char> #¥d #¥c #¥b #¥a) -> "a"
;;;         (char> #¥z #¥A) -> "A"
;;;         (char> #¥Z #¥z) -> nil
;;;         (char> #¥z #¥Z) -> "Z"
;;; ＠
;;; char>=                                 関数[#!macro]
;;;
;;; <説明>
;;;   形式 : char>= char1 &rest char2 ... charN
;;; 文字 char1 char2 ... charN を比較し、コード的、ビット的、フォント的に
;;; char1 が大きいか等しければ charN、そうでなければ nil を返す。
;;;
;;; <例>
;;;         (char>= #¥e #¥d) -> "d"
;;;         (char>= #¥e #¥e) -> "e"
;;;         (char>= #¥d #¥d #¥c #¥a) -> "a"
;;; ＠
;;; character                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : character object
;;; 可能であれば、object を強制的に文字にする。
;;; (character object) = (coerce object 'character)
;;;
;;; <例>
;;;         (character 'a) -> "a"
;;;         (character nil) -> ""
;;;         (character '(a b c)) -> "a"
;;; ＠
;;; characterp                             関数[#!subr]
;;;
;;; <説明>
;;;   形式 : characterp object
;;; object が文字であれば、評価値を返し、それ以外なら nil を返す。
;;; characterp = charp
;;;
;;; <例>
;;;         (characterp #¥a) -> "a"
;;;         (characterp 'a) -> nil
;;;         (characterp "a") -> "a"
;;; ＠
;;; charp                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : charp object
;;; object が長さ 1 の文字列ならば評価値を返し、それ以外なら nil を返す。
;;; charp = characterp
;;; <例>
;;;         (charp "a") -> "a"
;;;         (charp "") -> nil
;;;         (charp 'a) -> nil
;;;         (charp "ab") -> nil
;;;         (charp 123) -> nil
;;; ＠
;;; check-type                             関数[#!macro]
;;;
;;; <説明>
;;;   形式 : check-type place type &rest string
;;; place の内容が、型 type でなければ、エラーを警告する。エラーから継続
;;; するために、新しい値を受け入れ、それを place に置く。そして、新しい値の
;;; 型をチェックし、それが、なおも type でなければ別のエラーを警告する。
;;; place の副形式は、何回も評価され得る。place は、関数 setf に受け入れ
;;; 可能な汎変数参照でなければならない。type は、型指定子でなければならない。
;;; string は、この型の説明を指定し、評価される。string が省略されると、
;;; type から自動的に計算される。この関数は、nil を返す。
;;;
;;; <例>
;;;         (!a '(a b c)) -> (a b c)
;;;         (check-type a list) -> nil
;;;         (!b 10) -> 10
;;;         (check-type b integer) -> (not-implemented-yet check-type
;;;                                     (b integer nil))
;;; ＠

(defun tao:circular-list (&rest objects)
  "circular-list                          関数[#!expr]

<説明>
  形式 : circular-list &rest object1 object2 ... objectN
object1 object2 ... objectN を要素とする巡回リストを作成し返す。

<例>
        (circular-list 'a 'b) -> (a b a b a b a b ...)
        (circular-list '(a b) '(c d)) -> ((a b) (c d) (a b) (c d) ...)"
  (let ((lst (copy-list objects)))
    (rplacd (last lst) lst)
    lst))

;;; cis                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cis number
;;; 自然対数の底 e を number の値でべき乗した結果を複素数形式で返す。
;;;
;;; <例>
;;;         (cis 2) -> #c(-0.41614683654719290f0 0.909297426825681f0)
;;;         (cis -2) -> #c(-0.416146836547192f0 -0.909297426825681f0)
;;; cit101e-terminal                       クラス
;;;
;;; <説明>
;;;   インスタンスが cit101e ターミナルであるクラス。
;;; ＠
;;; cit600-terminal                        クラス
;;;
;;; <説明>
;;;   インスタンスが cit600 ターミナルであるクラス。
;;; ＠
;;; class-name-of                          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : class-name-of object
;;; object が所属しているクラス名を返す。
;;; TAO では基本的なデータタイプもすべてクラスとして見なされる。
;;;
;;; <例>
;;;         (class-name-of 12) -> integer
;;;         (class-name-of '(a . b)) -> cell
;;;         (!abc '(a . b)) -> (a . b)
;;;         (class-name-of abc) -> cell
;;;         (class-name-of 'abc) -> id
;;;         aa をクラス a3 の udo とする。
;;;         (class-name-of aa) -> a3
;;; ＠
;;; class-of                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : class-of class
;;; TAO のクラスは内部ではクラスベクタによって表されており、クラスベクタは
;;; クラス名を表す識別子の属性リストに格納されている。(defclass を参照)。
;;; クラス名 class の属性リストに格納されているクラスベクタを返す。
;;;
;;; <例>
;;;         (defclass a () ((x 5) (y 6)) (b) :gettable) -> a
;;;         (class-of 'a) -> {vector}40750(class . 10)
;;; ＠
;;; class-variable                         関数[#!subr]
;;;
;;; <説明>
;;;   形式 : class-variable class-var class-vector
;;; クラスベクタ class-vector のクラスに属する、クラス変数 class-var の値を
;;; 返す。指定されたクラス変数がなければエラーとなる。class-var への代入は
;;; 関数 cvar と同じ。
;;;
;;; <例>
;;;             関数 defclass-method の例を参照。
;;;         (defclass abc (p 10) () ()) -> abc
;;;         (class-variable 'p (class-of 'a)) -> 10
;;; ＠
;;; clause                                 関数[#!&+]
;;;
;;; <説明>
;;;   形式 : clause head body
;;; ヘッド部が head とマッチし、ボディ部が body とマッチする言明された節を
;;; サーチする。ヘッドとボディは、TAO では "(assert head . body)" となるが、
;;; DEC10-Prolog では "head :- body" となる。
;;; body は論理変数となりえる。 head も主ファンクタを除いて論理変数となりえ
;;; る。つまり、clause は既に言明された節の中のマッチされた節を抜き出す。
;;;
;;; <例>
;;;         (asserta (concat () _x _x)) -> concat
;;;         (asserta (concat (_a . _x) _y (_a . _z)) (concat _x _y _z))
;;;             -> concat
;;;         (clause (concat (_a . _x) _y (_a . _z)) _p) -> t
;;;         head は (concat (_a . _x) _y (_a . _z)) で body は _p。
;;;         clause 内の head は 2 番目の asserta のヘッドにマッチする。
;;;         body _p（論理変数）は 2 番目の asserta 内のボディに統合される。
;;;         つまり、 _p = ((concat _x _y _z)) となる。
;;;         (assert (watashi omae) (boku) (ore) (atai)) -> watashi
;;;         (clause (watashi omae) _p) -> t
;;;         _p -> ((boku) (ore) (atai))
;;;         (clause (watashi _u) _v) -> t
;;;         _u -> omae
;;;         _v -> ((boku) (ore) (atai))
;;; ＠
;;; clear-input                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : clear-input &opt stream
;;; stream と結び付いたバッファにある入力を読み込ませないようにする。
;;; nil を返す。
;;;
;;; <例>
;;;         (!aa (open "asd.tao")) -> {udo}1163487file-stream
;;;         (read aa) -> kyouwaiitenki
;;;         (clear-input aa) -> nil
;;;         (read aa) -> :eof
;;; ＠
;;; clear-memblk                           関数[#!subr]
;;;
;;; <説明>
;;;   形式 : clear-memblk memblk
;;; 1 ビット、2 ビット、4 ビット、8 ビット、16 ビット、32 ビット、64 ビット
;;; の記憶領域であるメモリブロック memblk の内容をクリアする。
;;;
;;; <例>
;;;         (!a (get-memblk #!8b-memblk 16)) ->
;;;             {memblk}491908(#!8b-memblk . {dnil}16)
;;;         (clear-memblk a) -> {memblk}491908(#!8b-memblk . {dnil}16)
;;; ＠
;;; clear-output 未インプリメント          関数[#!expr]
;;;
;;; <説明>
;;;   形式 : clear-output &opt stream
;;; stream に関する出力処理を中断させる。
;;; ＠
;;; sys:clear-tage                         関数[#!subr]
;;;
;;; <説明>
;;;   形式 : sys:clear-tage arg
;;; tage ビットがオフにされ、arg が返る。
;;;
;;; <例>
;;;         (sys:clear-tage ()) -> nil
;;;         (sys:clear-tage #144) -> 100 [10 進の shortnum にする]
;;; ＠
;;; close                                  関数[#!expr]
;;;
;;; <説明>
;;;   形式 : close stream &key :abort
;;; stream を閉じる。閉じた後の入出力操作は、stream に対して実行されない。
;;; :abort が、nil でないならば、stream の使用の異常な終了を表す。
;;;
;;; <例>
;;;         (!aa (open "tokio.tao")) -> {udo}51802file-stream
;;;         (close aa) => ok
;;; ＠


(defmacro tao:closure (var-list func)
  "closure                                関数[#!subr]

<説明>
  形式 : closure var-list func
関数 func が同じ環境で頻繁に用いられるとき便利である。
\(!f2 (closure '(x y) 'f1)) = (de f1 (x y) (!p (+ 1 2 3 x y)))
グローバル 変数と関数 f1 の引数は closure の var-list の中に入れること
はできない。

<例>
        (de a1 (x) (lambda (u) (x + u))) -> a1
        ((a1 3) 4) signals an error (x is undefined).
        (de a2 (x) (closure '(x) (lambda (u) (x + u)))) -> a2
        ((a2 3) 4) -> 7.
        (dye f (x) (inc a x) (list a b))
        (dye g (x) (inc b x) (list a b))
        (prog (a b) (!a 1) (!b 100)
              (!cf (closure '(a b) 'f)) (!b 200)
              (!cg (closure '(a b) 'g)))
        (cf 5) -> (6 200)
        (cg 10) -> (6 210)
        (cf 5) -> (11 210)
        (cg 1000) -> (11 1210)"
  `(let (,@(mapcar (lambda (x) (list x x)) (eval var-list))) ;--- TODO
     ,func))

;; (setf (symbol-function 'a1)
;;      (closure '(x) (lambda (u) (+ x u))))
;; (funcall (a1 3) 4)
;;; closurep                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : closurep func
;;; func が closure 関数なら、t を返し、そうでない場合は nil を返す。
;;;
;;; <例>
;;;         (defun func (x y) (!p (+ u v w x y))) -> func
;;;         (!fn (closure '(x y) 'func))
;;;                      -> {applobj}1206528(#!closure . 8)
;;;         (closurep fn) -> t
;;;         (closurep 'func) -> nil
;;; ＠
;;; clrhash                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : clrhash table
;;; ハッシュ表 table から、全ての要素を削除し、その結果 (ハッシュ表の内容)
;;; を返す。
;;;
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}1804146(hash-table . 8)
;;;         (clrhash a) -> {vector}1804146(hash-table . 8)
;;; ＠
;;; code-char                              関数[#!subr]
;;;
;;; <説明>
;;;   形式 : code-char code-attr &opt bit-attr font-attr
;;; コード属性 code-attr、ビット属性 bit-attr、フォント属性 font-attr に
;;; 対応する文字が生成できればその文字を返し、できなければ nil を返す。
;;;
;;; <例>
;;;         (code-char 97) -> "a"
;;;         (code-char 65) -> "A"
;;; ＠
;;; codnum                                 クラス
;;;
;;; <説明>
;;;   codnum はコード化整数 (256 以下の負でない小さな整数) を表す。
;;; 以下は同じカテゴリーに属するシステム定義のコード化整数。
;;;
;;; #!expr                      #0   s 式で定義されたスコープ限定関数
;;; #!exprdyn           #1   s 式で定義されたスコープ限定関数
;;; #!macro                     #2   マクロ
;;; #!subst             #3   subst
;;; #!closure           #4   クロージャ
;;; #!array                     #5   配列
;;; #!&+                        #6   スコープ限定 U-レゾルバ
;;; #!hclauses          #7   C-レゾルバ
;;; #!&+dyn                     #10  スコープ透過 U-レゾルバ
;;; #!subr                      #11  マイクロコード化関数 (スコープ透過)
;;; #!expr-simple               #12  スコープ限定単純関数
;;; #!exprdyn-simple    #13  スコープ透過単純関数
;;; #!subr-simple               #14  マイクロコード化単純関数 (スコープ透過)
;;; #!unit-clauses              #15  単位節におけるスペシャル C レゾルバ
;;; #!:ob                       #40  必須変数
;;; #!:qob                      #41  クォートつき必須変数
;;; #!:opt                      #42  オプショナル変数
;;; #!:qopt                     #43  クォートつきオプショナル変数
;;; #!:rest                     #44  残余変数
;;; #!:qrest            #45  クォートつき残余変数
;;; #!:aux                      #46  補助変数
;;; #!:logic            #47  論理変数
;;; #!:closed           #50  閉じた変数 (クロージャ用)
;;; #!:optn                     #51  オプショナル変数 (既定値 nil)
;;; #!:qoptn            #52  クォート付きオプショナル変数 (既定値 nil)
;;; #!1b-memblk         #100  1 ビットメモリブロック
;;; #!2b-memblk         #101  2 ビットメモリブロック
;;; #!4b-memblk         #102  4 ビットメモリブロック
;;; #!8b-memblk         #103  8 ビットメモリブロック
;;; #!16b-memblk                #104  16 ビットメモリブロック
;;; #!32b-memblk                #105  32 ビットメモリブロック
;;; #!64b-memblk                #106  64 ビットメモリブロック
;;; #!sys:prestk-memblk #107  スタック領域仮想用メモリブロック
;;; #!sys:id-hash-memblk    #110  識別子ハッシング用メモリブロック
;;; #!sys:64bloc-memblk #111  64 ビットロカティブメモリブロック
;;; #!sys:strhead-memblk    #113  ストリングヘッダメモリブロック
;;; #!sys:locbit-memblk #114  ロックビットメモリブロック
;;; #!sys:cell-memblk   #115  セルメモリブロック
;;; #!sys:vector-memblk #116  ベクタメモリブロック
;;; #!sys:id-memblk             #117  識別子メモリブロック
;;; #!sys:str-memblk    #120  ストリングメモリブロック
;;; #!sys:bad-memblk    #121  使用不可能なメモリブロック
;;; #!sys:free-memblk   #122  自由メモリブロック
;;;
;;; <例>
;;;         #!sunday, !monday, #!tuesday, #!wednesday が 0,1,2,3 を表すと
;;;         すると、以下のようなシンボル計算ができる。
;;;         ((#!monday + 16) mod 7) →  #!wednesday
;;; ＠
;;; codnump                                関数[#!subr]
;;;
;;; <説明>
;;;   形式 : codnump number
;;; number がコード化整数 (codnum) ならば number を返し、それ以外なら nil
;;; を返す。
;;;
;;; <例>
;;;         (codnump '#!expr) -> #!expr
;;;         (codnump #!expr) -> #!expr
;;;         (codnump #123) -> nil
;;; ＠
;;; coerce                                 関数[#!expr]
;;;
;;; <説明>
;;;   形式 : coerce object result-type
;;; object を型指定子 result-type の型に変換。
;;;
;;; <例>
;;;         (coerce "a" 'character) -> #¥a
;;;         (coerce 0 'short-float) -> 0.0
;;;         (coerce 3.5l0 'float) -> 3.5f0
;;;         (coerce 7/2 'float) -> 3.5f0
;;;         (coerce 7/2 'comlex) -> #c(7/2 0)
;;; ＠

(defmacro tao:comment (&body objects)
  "comment                                関数[#!expr]

<説明>
  形式 : comment &rest object1 onject2 ...  objectN
コメントを作る。object1 ... objectN の値にかかわりなくシンボル comment
を返す。

<例>
        (comment (de abc (x) (car x)) ) -> comment"
  (declare (ignore objects))
  ''comment)

;; 謎 コメントならtを返すんじゃないのか？
(defmacro tao:commentp (object)
  "commentp                               関数[#!subr]

<説明>
  形式 : commentp object
object がコメントなら t を返し、それ以外なら nil を返す。

<例>
        (commentp (ucdr '(a ;this is a comment
                          b c d ))) -> t"
  (and (consp object)
       (eq 'comment (car object))
       t))

;;; sys:common-package                     定数
;;;
;;; <説明>
;;;   パッケージ "common" へのポインタ。"common" は、パッケージ "univ" の
;;; サブパッケージ。"common" には、パッケージ "bas" 内と同じ名前の
;;; Common Lisp 関数が登録されている。
;;;
;;; <例>
;;;         sys:common-package -> {vector}32249(package . 12)
;;; ＠
;;; commonp                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : commonp object
;;; object のデータタイプが標準の Common Lisp のデータタイプならば t 、
;;; それ以外なら nil を返す。
;;;
;;; <例>
;;;         (commonp '(1 2 3)) -> t
;;;         (commonp #(1 2 3)) -> t
;;;         (commonp #¥a) -> "a"
;;; ＠
;;; compile                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : compile func1 &opt func2
;;; 関数 func1 をコンパイルし、結果のコンパイルされたコードは、その関数の
;;; 定義として、func1 に与えられる。func2 は、ラムダ式で与えられる、
;;; コンパイルされるべき、インタプリトされた関数。func2 が与えられなければ、
;;; func1 は、ラムダ式である関数定義でなければならない。func1 が nil でない
;;; シンボルならばコンパイルされた関数のオブジェクトはシンボルの広域的な
;;; 関数として組み込まれ、そのシンボルが返される。func1 が nil なら、
;;; コンパイルされた関数のオブジェクト自身が返される。
;;;
;;; <例>
;;;         (defun foo ...) -> foo   関数を定義
;;;         (compile 'foo) -> foo    コンパイルする
;;;             ここで foo はより高速になる
;;;         (compile nil '(lambda (a b c) (-(* b b) (* 4 a c)))
;;;         (de test1 ()
;;;           (write "help")) ->test1
;;;         (compile 'test1) -> For Function test1
;;;         (test1) -> "help"
;;; ＠
;;; compile-file                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : compile-file file &key :output-file
;;; file 中のソースプログラムをコンパイルし、バイナリオブジェクトファイルを
;;; 生成する。file は、妥当なファイル指定子でなければならない (例えばパス名
;;; など)。file は、Lisp ソースファイル。:output-file は、出力される
;;; オブジェクトファイルのパス名を指定する。:output-file に対する既定値は
;;; 変数 *default-pathname-defaults*から取られ、拡張子は lap。
;;;
;;; <例>
;;;         (de test1 ()
;;;             (write "help"))
;;;         以上の様なファイル "test.tao" があるとする
;;;         (compile-file 'test) -> *** compile start ***
;;;         Co::bs:<tak>test.lap
;;;         *** compile normal end ***
;;;         (load-lap 'test) -> (test1)
;;;         (test1) -> "help"
;;; ＠
;;; compiled-function-p                    関数[#!expr]
;;;
;;; <説明>
;;;   形式 : compiled-function-p object
;;; object が、コンパイルされたコードオブジェクトであれば object を返し、
;;; そうでなければ偽を返す。
;;; (compiled-function-p x) = (typep x 'compiled-function)
;;;
;;; <例>
;;;         (de tameshi ()
;;;             (write "bcd")) -> tameshi
;;;         (compiled-function-p 'tameshi) -> nil
;;;         (compile 'tameshi) -> For Function tameshi
;;;         (compiled-function-p 'tameshi) -> tameshi
;;; ＠
;;; compiler-let                           関数[#!macro]
;;;
;;; <説明>
;;;   形式 : compiler-let var-decl &rest body
;;; インタプリタでは、compiler-let は let と全く同じに働く。
;;; compiler-let 内で宣言された全てのローカル変数はスペシャル変数として
;;; 扱われる。コンパイラでは、compiler-let は下記のように働く。
;;; まず、var-decl 内のすべてのローカル変数はコンパイラが動いている環境では
;;; 与えられた値に拘束され、次に、普通のコンパイルプロセスが完了する。
;;; コンパイルされたコードが実行されたときローカル変数束縛は起こらない。
;;; ＠
;;; complex                                関数[#!expr]
;;;
;;; <説明>
;;;   形式 : complex realpart &opt imgpart
;;; realpart の値が実部で、imgpart の値が虚部の複素数を返す。
;;;
;;; <例>
;;;         (complex 3) -> 3
;;;         (complex 3.4) -> #c(3.4 0.0)
;;;         (complex -3.2) -> #c(-3.2 0.0)
;;;         (complex 3 2) -> #c(3 2)
;;; ＠
;;; complexp                               関数[#!subr]
;;;
;;; <説明>
;;;   形式 : complexp number
;;; number が複素数ならば t 、それ以外なら nil を返す。
;;;
;;; <例>
;;;         (complexp (sqrt -9)) -> #c(0.0f0 3.0f0)
;;;         (complexp (sqrt 4)) -> nil
;;; ＠
;;; concatenate                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : concatenate type &rest seq1 seq2 ... seqN
;;; シーケンス seq2 ... seqN をつないだ、新たなシーケンスを生成し、
;;; その結果を返す。
;;;
;;; <例>
;;;         (concatenate 'list '(1 2 3) #(a b c))
;;;             -> (1 2 3 a b c)
;;;         (!v (concatenate 'vector '(1 2 3) #(a b c) 1)
;;;             ->{vector}1839008(bas:simple-general-vector . 6)
;;;         (show-vector v)
;;;                 -> vtitle: bas:simple-general-vector vsize: 6
;;;                    0  kdr  1
;;;                    1  kar  2
;;;                    2  kdr  3
;;;                    3  kar  a
;;;                    4  kdr  b
;;;                    5  kar  c
;;;               {vector}1839047(bus:simple-general-vector .6)
;;; ＠
;;; concatenated-stream                    クラス
;;;
;;; <説明>
;;;   インスタンスが concatenated-stream であるクラス。
;;; concatenated-stream は、メンバーが、作成された入力ストリームの集合体。
;;; 第 1 のメンバーであるストリームから最後のメンバーであるストリームまで、
;;; 入力が繰り返される。
;;; ＠
;;; cond                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cond (test1 form11 form12 ...)
;;;               (test2 form21 form22 ...)
;;;               ...
;;; テストフォーム test1 test2 ... を順に調べていき、その値が nil にならな
;;; かった最初のテストフォームの右側のフォームを順に実行する。
;;; (右側にフォームが全くなければテストフォームの値を返す)。そして、その
;;; 最後のフォームの値を返す。最後のテストフォームが t で、それ以前のテスト
;;; フォームの値が全部 nil のとき、t の右側のフォームが実行される。
;;;
;;; <例>
;;;         (de fact (n)
;;;           (cond ((n = 0) 1) ; If n = 0, cond returns 1.
;;;                 (t (n * (fact (n - 1)))) )) ; If n <> 0, cond returns
;;;                                             ; (n * (fact (n - 1)))
;;;         (cond ((atom x)) ; (atom x)'s value will be cond's value since
;;;                          ; there is no form after test
;;;               ((consp x) (do-some-work (car x))
;;;                          (do-some-work (cdr x)) ))
;;;         (atom x) の評価が t のとき、cond は (atom  x) の値 t を返す。
;;;         (atom x) の値が nil で (consp x) の値が t のとき、
;;;         (do-some-work  (car x)) と (do-some-work (cdr x)) を両方評価
;;;         した後に (do-some-work (cdr x)) の値を返す。
;;;         (atom x) も (consp x) も nil のとき cond は何もしない。
;;; ＠
;;; conjugate                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : conjugate number
;;; number の共役複素数 number (number が複素数でない場合は number) を返す。
;;;
;;; <例>
;;;         (conjugate #c(3/5 4/5)) -> #c(3/5 -4/5)
;;;         (conjugate 3.7) -> #c(3.7 0.0)
;;;         (conjugate 3) -> 3
;;;         (conjugate #c(0.0d0 -1.0d0)) -> #c(0.0f0 1.0f0)
;;; ＠
;;; connect-dir                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : connect-dir &opt dir
;;; カレントディレクトリを dir に変更。
;;; dir の既定値は、ログインディレクトリ。
;;;
;;; <例>
;;;       (connect-dir "<dir2>") カレントディレクトリを <dir2> にする。
;;;       (connect-dir "as:<dir3>") カレントディレクトリを as:<dir3> にする。
;;; ＠
;;; cons                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cons object1 object2
;;; object1 が car 部で、object2 が cdr 部であるような新しいセルを作り、
;;; それを返す。(cons  x  y) = [x  .  y]
;;;
;;; <例>
;;;         (cons 'a 'b) -> (a . b)
;;;         (cons 'a (cons 'b (cons 'c '()))) -> (a b c)
;;;         (cons 'a '(b c d)) -> (a b c d)
;;;         (cons 1 2) -> (1 . 2)
;;;         (cons 'a nil) -> (a)
;;;         (cons '(a b) '(c d)) -> ((a b) c d)

(defmacro tao:cons! (object1 object2)
  "cons!                                  関数[#!macro]

<説明>
  形式 : cons! object1 object2
\(cons! object1 object2) = (!object1 (cons object2 object1)) 。
ただし左辺では object1 は 1 度だけ評価される。

<例>
        x を (a b)、y を (c d)とする
        (cons! x y) -> ((c d) a b)"
  `(setq ,object1 (nconc (list ,object2) ,object1)))

;;; consp                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : consp object
;;; object がコンスデータ (セル又は名前付きセル)、角カッコ (bracket)
;;; 若しくは名前付き角カッコならば、評価値を返し、それ以外は nil を返す。
;;;
;;; <例>
;;;         (consp x) = (or (cellp x) (bracketp x))
;;;         (consp '(a . b)) -> (a . b)
;;;         (consp 'a) -> nil
;;;         (consp '()) -> nil
;;;         (consp '(() . ())) -> (())
;;; ＠
;;; constantp                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : constantp name
;;; name が定数値を持つなら、そのオブジェクトを返し、それ以外なら nil を
;;; 返す。以下のものは、定数値を持つ。nil、t、number、character、string、
;;; bit-vector、キーワード引数、関数 defconstant で宣言されたシンボル等。
;;;
;;; <例>
;;;         (defconstant aa 25) -> aa
;;;         a -> 25
;;;         (constant 'aa) -> t
;;;         (!aa 25) -> 25
;;;         (constantp 'aa) -> nil
;;; ＠
;;; convert-to-jstring                     関数[#!subr]
;;;
;;; <説明>
;;;   形式 : convert-to-jstring string
;;; string の文字列コードを ASCII コードから JIS コードに変換し、string の
;;; 内容を書き換え、その値を返す。
;;;
;;; <例>
;;;         (!foo (sconc (as-char 164) (as-char 162))) -> "あ"
;;;         (slength foo) -> 2
;;;         (jstringp foo) -> nil
;;;         (convert-to-jstring foo) -> "あ"
;;;         (slength foo) -> 1
;;;         (jstringp foo) -> {dnil}1
;;; ＠

(defun tao:copy (object)
  "copy                                   関数[#!subr]

<説明>
  形式 : copy object
object がコンスの場合、それをコピーして返す。コンスでなければコピー
しないで object を返す。
copy-tree 参照。

<例>
        (copy '(a b ; this is a sample list
                c d) )
        -> (a b ; this is a sample list
             c d)
        (eq (!x 123) (copy x)) -> t"
  (if (consp object)
      (copy-tree object)
      object))

;;; ＠
;;; copy-alist                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-alist a-list
;;; 連想リスト a-list のトップレベルとセカンドレベルをコピーし、その結果を
;;; 返す。
;;;
;;; <例>
;;;         (!e '((this . i) (that . r) (the . y))) ->
;;;             ((this . i) (that . r) (the . y))
;;;         (!s (copy-alist e)) -> ((this . i) (that . r) (the . y))
;;;         s -> ((this . i) (that . r) (the . y))
;;; ＠
;;; copy-file                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-file file1 file2
;;; file1 を file2 へコピーする。
;;;
;;; <例>
;;;         (copy-file "<dir>abc.def" "ttt.kkk") -> ok
;;; ＠
;;; copy-from-floppy                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-from-floppy dir1 dev &opt dir2
;;; フロッピーディスク dev から、ディレクトリ dir2 にダンプされたファイルを、
;;; ディレクトリ dir1 にコピーする。dir2 の既定値は、dir1 の値。
;;;
;;; <例>
;;;         (copy-from-floppy "cs:<work>" "dy:" "<gonbe>")
;;; ＠
;;; copy-list                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-list list
;;; list をコピーして返す。コピーされたリストは元のリストと eq ではないが
;;; equal 。list のリスト構造のトップレベルにあるコンスはコピーされるが、
;;; それ以外にあるコンスはコピーされず、返されるリストと元のリストで共有
;;; される。
;;;
;;; <例>
;;;         (copy-list '(a . b)) -> (a . b)
;;; ＠
;;; copy-memblk                            関数[#!subr]
;;;
;;; <説明>
;;;   形式: copy-memblk memblk1 memblk2
;;; メモリブロック memblk1 の内容を、memblk2 にコピーし、memblk2 の内容を
;;; 返す。memblk1 のサイズが memblk2 のサイズより小さい場合、memblk2 の残り
;;; の部分は影響を受けない。2 つのメモリブロックは、同じ型のものでなければ
;;; ならない。
;;;
;;; <例>
;;;         (!aaa (get-memblk #!16b-memblk 20)) ->
;;;                        {memblk}480797(#!16b-memblk . {dnil}20)
;;;         (!bbb (get-memblk #!16b-memblk 30)) ->
;;;                       {memblk}480804(#!16b-memblk . {dnil}30)
;;;         (!ccc (get-memblk #!8b-memblk 40)) ->
;;;                        {memblk}480814(#!8b-memblk . {dnil}40)
;;;         (!(nthm aaa 5) #100) -> #100
;;;         (!(nthm bbb 5) #200) -> #200
;;;         (!(nthm bbb 25) #300) -> #300
;;;         (copy-memblk aaa bbb) ->
;;;              {memblk}480804(#!16b-memblk . {dnil}30)
;;;         (nthm bbb 5) -> #100
;;;         (nthm bbb 25) -> #300
;;;         (copy-memblk aaa ccc) はエラー
;;; ＠
;;; copy-readtable                         関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-readtable &opt table1 table2
;;; 読み込み表 table1 を table2 にコピーする。table1 の既定値は、現在の
;;; 読み込み表 (変数@*readtable* の値) 。table2 が省略されるか nil であれば
;;; 新しいコピーが作られる。そうでなければ、table2 は、読み込み表でなければ
;;; ならず、そこに破壊的にコピーが行われる。
;;;
;;; <例>
;;;         (equal (copy-readtable) *readtable*) -> t
;;; ＠
;;; copy-seq                               関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-seq seq
;;; シーケンス seq のコピーを作る。
;;;
;;; <例>
;;;         (!x '(a b c)) -> (a b c)
;;;         (!y (copy-seq x)) -> (a b c)
;;;         (equal x y) -> t
;;;         (eq x y) -> nil
;;;         (setq x '(1 2 3) y #(a b c))
;;;                      -> {vector}1838302(simple-vector . 3)
;;;         (setq x1 (copy-seq x)) -> (1 2 3)
;;;         (setq y1 (copy-seq y))
;;;              -> {vector}1838302(simple-vector . 3)
;;;         y,y1 ともに
;;;            vtitle: simple-vector  vsize: 3
;;;                0 kdr: a
;;;                1 kar: b
;;;                2 kdr: c
;;;            {vector}1838302(simple-vector . 3)
;;; ＠
;;; copy-symbol                            関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-symbol symbol &opt flag
;;; symbol は、すでに存在するインターンされたシンボル。新しいインターンされ
;;; ないシンボル #:symbol を作る。（#: は新たに作られたシンボルがインターン
;;; されないことを意味する）。新たに作られたシンボルの値と属性リストは、
;;; flag が nilでない場合、 symbol のコピーである。 flag が nil (既定値) の
;;; とき、新たに作られたシンボルは未定義 (unbound) であり、その属性リストは
;;; nil 。
;;;
;;; <例>
;;;         (!aa (gentemp 'ex)) -> ex1000
;;;         (!bb (copy-symbol 'aa)) -> #:ex
;;;         (symbol-plist bb) -> ()
;;;         (symbol-name bb) -> "aa"
;;;         (symbol-name aa) -> "ex1000"
;;; ＠
;;; copy-tree                              関数[#!expr]
;;;
;;; <説明>
;;;   形式 : copy-tree object
;;; object がコンスの場合は、object をコピーし、返す。コンスでない場合は、
;;; object を返す。トップレベルだけでなく、すべてのレベルにあるオブジェクト
;;; をコピーする。
;;;
;;; <例>
;;;         (copy-tree 'abc) -> abc
;;;         x が (abc) のとき
;;;         (copy-tree x)-> (abc)
;;; ＠
;;; cos                                    関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cos number
;;; number の余弦 ( cosine ) を返す。
;;; number はラジアン単位であり、number (ラジアン単位) の余弦 (cosine) を
;;; 返す。
;;;
;;; <例>
;;;         (cos [pi / 6]) -> 0.866025403784439f0
;;;         (cos [pi / 4]) -> 0.707106781186548f0
;;;         (cos #c(2 3)) -> #c(-4.1896256909688f0 -9.10922789875533f0)
;;; ＠
;;; cosh                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cosh number
;;; number の双曲的余弦 (hyperbolic cosine) を返す。
;;;
;;; <例>
;;;         (cosh 1.0f0) -> 1.54308063481524f0
;;;         (cosh 0.5f0) -> 1.12762596520639f0
;;;         (cosh -1.0f0) -> 1.54308063481524f0
;;; ＠
;;; count                                  関数[#!macro]
;;;
;;; <説明>
;;;   形式 : count item seq
;;;                &key :from-end :test :test-not :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、item の数をカウントし、
;;; 結果を返す。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (count 1 x) -> 2
;;;         (count 1 x :start 2 :end 4) -> 1
;;;         (count 1 x :test #'<) -> 5
;;;         (count 1 x :test-not #'<) -> 2
;;; ＠
;;; count-if                               関数[#!macro]
;;;
;;; <説明>
;;;   形式 : count-if test seq &key :from-end :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、test を満足する数を
;;; カウントし、結果を返す。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5) -> (1 2 4 1 3 4 5)
;;;         (count-if #'oddp x) -> 4
;;;         (count-if #'evenp x :start 2) -> 2
;;;         (count-if #'evenp x :start 2 :end 4) -> 1
;;; ＠
;;; count-if-not                           関数[#!macro]
;;;
;;; <説明>
;;;   形式 : count-if-not test seq &key :from-end :start :end :key
;;; シーケンス seq の :start から :end までの範囲で、 test を満足しない数を
;;; カウントし、結果を返す。
;;;
;;; <例>
;;;         (!x '(1 2 4 1 3 4 5)) -> (1 2 4 1 3 4 5)
;;;         (count-if-not #'oddp x)) -> 3
;;;         (count-if-not #'evenp x :start 2 :end 5) -> 2
;;; ＠
;;; create-dir                             関数[#!expr]
;;;
;;; <説明>
;;;   形式 : create-dir dir1 dir2 &opt number
;;; ディレクトリを作る。dir1 で、ELIS システムに対するディレクトリ名が指定
;;; される。dir2 で、fep (front-end-processor) システムに対する固有の
;;; ディレクトリ名 (最大 6 文字) が指定される。number で、ファイルの世代
;;; カウントが指定される。最新バージョンより number 世代以上古いバージョン
;;; は削除される。
;;;
;;; <例>
;;;         (create-dir "cs:<gonbe>" "ngonbe")
;;;         (create-dir "elis::as:<nonbe>" "snonbe")
;;; ＠
;;; critical-section                       関数[#!macro]
;;;
;;; <説明>
;;;   形式 : critical-section semaphore &rest form1 form2 ... formN
;;; 他のプロセスからの中断なしに、form1 .. formN を実行する。このフォームは、
;;; semaphore を必要とする。
;;; ＠

(defun tao:crlf (&optional stream)
  "crlf                                   関数[#!subr]

<説明>
  形式 : crlf &opt stream
stream に復帰文字と改行文字を出力し、t を返す。stream が省略された場合、
変数 *standard-input* の値 (通常コンソールターミナル) が指定されたもの
と見なす。関数 terpri と同じ働きをする。"
  (write-char #\Return stream)
  (write-char #\Linefeed stream)
  t)

;;; ＠
;;; ctypecase                              関数[#!macro]
;;;
;;; <説明>
;;;   形式 : ctypecase keyplace
;;;             (type-1 form11 ...form1N)
;;;             (type-i formI1 ...formIN)
;;;             (type-M formM1 ...formMN)
;;; 形式 keyplace を評価し、その型が、type-i であるフォーム
;;; formi1 ... formiN を順に評価し、最後のフォームのリターン値を返す。
;;; 満足する節がなければ、継続可能なエラーを警告する。エラーから継続する
;;; ために、この関数は、新しい値を受け入れ、それを keyplace に置く。そして、
;;; テストを再び行う。keyplace の副形式は何回も評価される。keyplace は、
;;; 関数 setf に受け入れ可能な汎変数参照でなければならない。関数 typecase
;;; に似ているけれども、otherwise 句あるいは t 句は許されない。
;;; type-i は、型指定子であり、評価されない。
;;;
;;; <例>
;;;         (defun test (x)
;;;             (ctypecase (list 'list)
;;;                        (integer 'integer))) -> test
;;;         (test '(a b c)) -> (not-implemented-yet ctypecase
;;;                             ((list 'list)
;;;                                  (integer 'integer)))
;;; ＠

(defun tao:current-dir ()
  "current-dir                            関数[#!expr]

<説明>
  カレントディレクトリを文字列で返す。

<例>
            (current-dir) -> \"bs:<dire>\""
  #+sbcl
  (sb-unix:posix-getcwd))

;;; ＠
;;; current-package                        関数[#!expr]
;;;
;;; <説明>
;;;   カレントパッケージポインタを返す。
;;;
;;; <例>
;;;         (package-name (current-package)) -> "gonta"
;;; ＠
;;; sys:current-process                    変数
;;;
;;; <説明>
;;;   sys:current-process の値は、現在処理中のプロセス。現在処理中の
;;; プロセスがなければ値は nil 。
;;;
;;; <例>
;;;         (sys:current-process) -> {udo}1137949process
;;; ＠

(setf (documentation 'cvar 'function)
      "cvar                                   関数[#!subr]

<説明>
  形式 : cvar var-name
クラス変数 var-name を捜し、あればその値を返す。なければエラー。
クラス変数の検索は、cvar が使われているメソッドを受け取るクラスから
始まり、そのスーパクラスへ進む。関数 defmethod や defclass-method に
おいて指定されるメソッドボディでのみ使用可能。結果として返される値への
代入は可能。defclass-method 参照。")

;;; ＠
;;; cycle                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : cycle &opt exit-id
;;; exit-id である loop 中の最初の文に強制的に戻す。exit-id が省略されて
;;; いたら最も内側の loop が指定される。loop 中の条件式に不必要に深くネスト
;;; するのを避けるのに使われる。
;;;
;;; <例>
;;;         (loop (&aux x y)
;;;               (:init (!x nil))
;;;               (!y (read))
;;;               (:until (eq y 99) (print x))
;;;               (if (evenp y) (cycle))
;;;               (!!cons y !x))
;;;         1
;;;         2
;;;         3
;;;         4
;;;         5
;;;         99 -> (5 3 1)
;;; ＠
