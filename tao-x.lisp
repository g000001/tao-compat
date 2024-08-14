;; -*- Mode: LISP; Syntax: COMMON-LISP; Coding:utf-8; -*-
(tao:common-lisp)


(in-package #:tao-internal)


(define
 "xcons"
 (subr (arg1 arg2)
   (cons arg2 arg1))
 :documentation
 "形式 : xcons arg1 arg2
arg2 が car 部であり、arg1 が cdr 部であるコンスを作り、それを返す。"
 :example
 "x = '(a b c)  y = '(d e f) のとき
        (xcons x y) -> ((d e f) a b c)
        (xcons 'a 'b) -> (b . a)")


(define
 "xor#"
 ;; (locative-operator nil)
 (subr (loc1 loc2)
   (logxor loc1 loc2))
 :documentation
 "形式 : loc1 xor# loc2
2 つの引数のビット xor (exclusive or) 操作を行う。"
 :example
 "(signed-integer-locatives p q r) -> (p q r)
        (p <- #5252) -> 2730
        (q <- #7070) -> 3640
        (r <- (p xor# q )) -> 1170 (#2222)")


;;; *EOF*
