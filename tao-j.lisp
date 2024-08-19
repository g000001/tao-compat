;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(cl:in-package #:tao-internal)


(define
 "jcharp"
 (subr (char)
   (typecase char
     (character (if (typep (char-code char) '(integer 256 *))
                    char
                    nil))
     (T nil)))
 :documentation
 "形式 : jcharp object
object が 2 バイト文字 (日本語コード文字) なら、評価値を返し、
それ以外なら nil を返す。"
 :example
 "(jcharp (code-char 41377)) -> \"  \"  (全角スペース)
        (jcharp (code-char 41934)) -> \"Ｎ\"
        (jcharp 'aa) -> nil
        (jcharp \"aa\") -> nil")


(define
 "jstringp"
 (subr (string)
   (typecase string
     (string (if (some #'tao:jcharp string)
                 (count-if #'tao:jcharp string)
                 nil))
     (T nil)))
 :documentation
 "形式 : jstringp string
string に 2 バイト文字 (日本語コード文字) が含まれているなら、その
2 バイト文字の個数を返し、それ以外なら nil を返す。"
 :example
 "(jstringp \"ＮＴＴNTT電気通信研究所\") -> 10
        (jstringp \"NTT\") -> nil")


;;; *EOF*
