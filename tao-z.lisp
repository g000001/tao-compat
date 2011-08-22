;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(in-package #:tao-internal)
(in-readtable :tao)

(defun tao:zerop (number)
  "zerop                                  関数[#!subr]

<説明>
  形式 : zerop number
number が 0 ならば 0 を返し、それ以外なら nil を返す。
引数が数値でなければならないことを除けば、(= x 0) と同じ。

<例>
        (zerop 1) -> nil
        (zerop 0) -> 0
        (zerop -2) -> nil
        (zerop nil) エラー
        (zerop \"0\") エラー"
  (and (cl:zerop number) 0))
