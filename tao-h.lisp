(tao:common-lisp)


(in-package #:tao-internal)


(define
 "hash"
 (subr nil)
 :documentation
 "形式 : hash object
object がシンボルかストリングなら、そのハッシュ値を返し、そうでなければ
エラーを返す。"
 :example
 "(hash 'a) -> 97
        (hash 'b) -> 98")


(define
 "hash-table-count"
 #'hash-table-count
 :documentation
 "形式 : hash-table-count table
ハッシュ表 table のエントリの個数を返す。
ハッシュ表が生成された直後、及び clrhash 関数でクリアされた直後の
エントリ数は 0 。"
 :example
 "(!a (make-hash-table)) -> {vector}81749(hash-table . 8)
        (hash-table-count a)) -> 0
        (!(gethash 'color a) 'red) -> red
        (hash-table-count a) -> 1")


(define
 "hash-table-p"
 #'cl:hash-table-p
 :documentation
 "形式 : hash-table-p object
object がハッシュ表なら t 、それ以外の場合は nil を返す。"
 :example
 "(!a (make-hash-table)) -> {vector}81749(hash-table . 8)
        (hash-table-p a) -> t
        (hash-table-p b) -> エラー")


(defmacro tao::*Hclauses (&body body)
  (tao.logic::make-anonymous-predicate-expr
   (length (elt (elt body 0) 1))
   body))


(define
 "Hclauses"
 (macro (&body body)  
     `(tao.logic::compile-anonymous-predicate
       ,(length (elt (elt body 0) 1))
       ',body))
 :documentation
 "形式 : hclauses (&+ A1' [(&aux var ...)] B11 ... B1n1) ...
         	  (&+ Am' [(&aux var ...)] Bm1 ... Bmnm) 
名前なしの C-resolver を作る。
シンボル A1' とシンボル A1 の違いについては、関数 &+ を参照。"
 :example
 "(&(&aux _a) ((hclauses (&+ (_x _y 3) (list _x _y))
        		 (&+ (_x _y _x) (list _x _y)))
        	 	1 (_a 2) _a ))
        最初に、第 1  U-resolver (&+ (_x _y 3) (list _x _y) を選び、
        a を 3 にしてリスト (1 (3 2)) を返す。
        バックトラックが起これば、
        第 2 U-resolver (&+ (_x _y)) (list _x _y))
        を選び x を 1 にして (1 (1  2)) を返す。
        もう一度、バックトラックが起こればフォーム & は nil を返す。
        つまり選択は、完全に失敗となる。")


(define
 "hidar"
 (macro (var)
     `(and ,var
           (if (boundp ',var)
               (if (eq (symbol-value ',var) ,var)
                   (cons ',var (symbol-value ',var))
                   (cons ',var ,var))
               (cons ',var ,var))))
 :documentation
 "形式 : hidar var
var の束縛を返す。その束縛は変数名とその値のペアによって表される。
var は値をもつ変数でなくてはならない。"
 :example
 "(!x 12) -> 12
        (hidar x) -> (12 . {vector}1830428(package . 12))
        (prog (a) (!a 123) (hidar a)) -> (123 . a)")


(define
 "host-fullname"
 (subr ()
   (long-site-name))
 :documentation
 "使っているホストコンピュータの full-namestring を格納。"
 :example
 "host-fullname -> \"Titanium\"")


(define-symbol-macro host-fullname (tao:host-fullname))


(define
 "host-name"
 (subr ()
   (short-site-name))
 :documentation
 "使っているホストコンピュータのニックネームを namestring で格納。"
 :example
 "host-name -> \"Ti\"")


(define-symbol-macro host-name (tao:short-site-name))


(define
 "host-namestring"
 #'host-namestring
 :documentation
 "形式 : host-namestring pathname
ファイル pathname のホスト名を文字列表現で返す。"
 :example
 "(host-namestring \"Ti::bs*<anata>konata.sonata.5\") -> \"Ti::\"")


;;; *EOF*
