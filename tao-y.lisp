;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)

(cl:in-package #:tao-internal)


(define
 "y-or-n-p"
 #'y-or-n-p
 :documentation
 "形式 : y-or-n-p &opt string &rest arg1 arg2 ... argN
答えが yes (または Y) か no (または N) になるような質問をユーザーに尋
ねる。yes なら t、no なら nil となる。string は質問を表すフォーマット
文字列で、arg1 arg2 ... argN はその引数。"
 :example
 "(y-or-n-p \"Are you ready?\") -> Are you ready?
        yes -> t
        no -> nil
        (y-or-n-p \"Cannot connect to network host ~S. Retry?\" 
        	host-name)
        -> Cannot connect to network host \\\"Ho\\\". Retry?
        yes -> t
        no -> nil
        yes,no 以外  \" ...Please respond with yes or no\" により
        再度 yes,no を要求してくる。")


(define
 "yes-or-no-p"
 #'yes-or-no-p
 :documentation
 "形式 : yes-or-no-p &opt string &rest arg1 arg2 ... argN
答えが yes (または Y) か no (または N) になるような質問をユーザーに尋
ね、yes ならば t、no ならば nil となる。yes, no 以外を入力すると、警告
を表示してベルを鳴らす。string は質問を表すフォーマット文字列で、arg1
arg2 ... argN はその引数。"
 :example
 "(yes-or-no-p \"Are you ready?\") -> {bell} Are you ready?
        yes -> t
        no -> nil
        (yes-or-no-p \"Cannot connect to network host ~S. Retry?\" 
        	host-name)
        -> {bell} Cannot connect to network host \\\"Ho\\\". Retry?
        yes -> t
        no -> nil
        yes,no 以外  \" ...Please respond with yes or no\" {bell} により
        再度 yes,no を要求してくる。
        {bell} はベルが鳴る。")


;;; *EOF*
