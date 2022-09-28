(in-package #:tao-internal)
(in-readtable :tao)

;;; ＠
;;; hash                                   関数[#!subr]
;;;
;;; <説明>
;;;   形式 : hash object
;;; object がシンボルかストリングなら、そのハッシュ値を返し、そうでなければ
;;; エラーを返す。
;;;
;;; <例>
;;;         (hash 'a) -> 97
;;;         (hash 'b) -> 98
;;; ＠
;;; hash-table-count                       関数[#!expr]
;;;
;;; <説明>
;;;   形式 : hash-table-count table
;;; ハッシュ表 table のエントリの個数を返す。
;;; ハッシュ表が生成された直後、及び clrhash 関数でクリアされた直後の
;;; エントリ数は 0 。
;;;
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}81749(hash-table . 8)
;;;         (hash-table-count a)) -> 0
;;;         (!(gethash 'color a) 'red) -> red
;;;         (hash-table-count a) -> 1
;;; ＠
;;; hash-table-p                           関数[#!expr]
;;;
;;; <説明>
;;;   形式 : hash-table-p object
;;; object がハッシュ表なら t 、それ以外の場合は nil を返す。
;;;
;;; <例>
;;;         (!a (make-hash-table)) -> {vector}81749(hash-table . 8)
;;;         (hash-table-p a) -> t
;;;         (hash-table-p b) -> エラー
;;; ＠

(defmacro tao:Hclauses (&body body)
  "hclauses                               関数[#!subr]

<説明>
  形式 : hclauses (&+ A1' [(&aux var ...)] B11 ... B1n1) ...
         	  (&+ Am' [(&aux var ...)] Bm1 ... Bmnm)
名前なしの C-resolver を作る。
シンボル A1' とシンボル A1 の違いについては、関数 &+ を参照。

<例>
        (&(&aux _a) ((hclauses (&+ (_x _y 3) (list _x _y))
        		 (&+ (_x _y _x) (list _x _y)))
        	 	1 (_a 2) _a ))
        最初に、第 1  U-resolver (&+ (_x _y 3) (list _x _y) を選び、
        a を 3 にしてリスト (1 (3 2)) を返す。
        バックトラックが起これば、
        第 2 U-resolver (&+ (_x _y)) (list _x _y))
        を選び x を 1 にして (1 (1  2)) を返す。
        もう一度、バックトラックが起こればフォーム & は nil を返す。
        つまり選択は、完全に失敗となる。"  
  `(tao.logic::compile-anonymous-predicate ,(length (elt (elt body 0) 1))
                                           ',body))

;;; ＠
;;; hidar                                  関数[#!subr]
;;;
;;; <説明>
;;;   形式 : hidar var
;;; var の束縛を返す。その束縛は変数名とその値のペアによって表される。
;;; var は値をもつ変数でなくてはならない。
;;;
;;; <例>
;;;         (!x 12) -> 12
;;;         (hidar x) -> (12 . {vector}1830428(package . 12))
;;;         (prog (a) (!a 123) (hidar a)) -> (123 . a)

(defmacro tao:hidar (var)
  `(and ,var
	(if (boundp ',var)
	    (if (eq (symbol-value ',var) ,var)
		(cons ',var (symbol-value ',var))
		(cons ',var ,var))
	    (cons ',var ,var))))
#|
 (setq x 1.0)

 (let ((y 0))
  (hidar x))
|#

;;; ＠
;;; host-fullname                          変数
;;;
;;; <説明>
;;;   使っているホストコンピュータの full-namestring を格納。
;;;
;;; <例>
;;;         host-fullname -> "Titanium"

(defun tao:host-fullname ()
  (long-site-name))

;;; ＠
;;; host-name                              変数
;;;
;;; <説明>
;;;   使っているホストコンピュータのニックネームを namestring で格納。
;;;
;;; <例>
;;;         host-name -> "Ti"
;;; ＠
;;; host-namestring                        関数[#!expr]
;;;
;;; <説明>
;;;   形式 : host-namestring pathname
;;; ファイル pathname のホスト名を文字列表現で返す。
;;;
;;; <例>
;;;         (host-namestring "Ti::bs*<anata>konata.sonata.5") -> "Ti::"
;;; ＠
