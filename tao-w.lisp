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


;;; *EOF*
