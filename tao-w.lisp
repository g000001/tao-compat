;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)


(in-package #:tao-internal)


(defmacro tao:within-class (class &body body)
  "within-class                           関数[#!macro]

<説明>
  形式 : within-class 'class-name forms
クラス class-name 内の論理メソッドを宣言、消去する。
forms は (assert ...) または (retract ...) から構成される。 

<例>
        (defclass aclass () ()) -> aclass
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
        deflogic-method の例を参照。"
  `(macrolet ((tao:assert ((pred &rest args) &rest clauses)
                `(tao:deflogic-method (,',class ,pred) (,@args)
                   ,@clauses))
              (tao:retract ((pred &rest args))
                `(tao:undeflogic-method (,',class ,pred) (,@args))))
     ,@body))


(defun tao:write (object &optional (stream *standard-output*) (crlf T))
  "write                                  関数[#!exprdyn]

<説明>
  形式 : write object &opt stream crlf
stream に object 及び改行文字をプリントした後、object を返す。stream が
省略されると *standard-output* の値が使われる。crlf の指定がないと、
改行を行なう。

<例>
        (write 'abc) -> abc
                        abc
        (write 'abc *standard-output* 'xyz) -> abcabc
        (write 'abc *standard-output* nil) -> abcabc"
  (cl:write object :stream stream)
  (when crlf (cl:write-char #\Newline stream)))



"common:write                           関数[#!macro]

<説明>
  形式 : common:write object &key :stream :escape :radix :base :circle
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
:array   (配列を出力するか)          〃       *print-array*      〃

<例>
        (common:write \"asdf\") -> \"asdf\"
        			 \"asdf\""

;;; *EOF*
