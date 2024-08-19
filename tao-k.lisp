;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(cl:in-package #:tao-internal)


(define
 "tao.sys:key-package"
 (constant (load-time-value (find-package :keyword)))
 :documentation
 "この定数の値は、パッケージ \"key\" へのポインタ。
\"key\" は、パッケージ \"univ\" のサブパッケージで、キーワードが登録されて
いる。"
 :example
 "")


(define
 "keywordp"
 #'cl:keywordp
 :documentation
 "形式 : keywordp symbol
symbol がキーワードパッケージに属するシンボルなら t を返し、それ以外
なら nil を返す。 キーワードとは \":init\", \":while\" など \":\" が先頭に
書いてあるシンボルのこと。"
 :example
 "(keywordp :until) -> t
        (keywordp :init) -> t")


(define
 "tao.sys:kill-job"
 (expr nil)
 :documentation
 "形式 : sys:kill-job job-number
job-number のプロセスをキルする。
ログインにより直接生成されたプロセスに対して、この関数を使用しては
いけない。そのようなプロセスをキルするには関数 sys:logout-him を使う。"
 :example
 "sys:kill-job 2 -> ok (job 2 があるとき)
        sys:kill-job 2 -> nil (job 2 が無いとき)")


;;; *EOF*
