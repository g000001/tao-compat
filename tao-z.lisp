;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)


(in-package #:tao-internal)


(define
 "zerop"
 (subr (number)
   (and (cl:zerop number) 0))
 :documentation
 "形式 : zerop number
number が 0 ならば 0 を返し、それ以外なら nil を返す。
引数が数値でなければならないことを除けば、(= x 0) と同じ。"
 :example
 "(zerop 1) -> nil
        (zerop 0) -> 0
        (zerop -2) -> nil
        (zerop nil) エラー
        (zerop \"0\") エラー")


;;; *EOF*
