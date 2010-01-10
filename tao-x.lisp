(in-package #:tao-compat)

(defun xcons (arg1 arg2)
  "xcons                                  関数[#!subr]

<説明>
  形式 : xcons arg1 arg2
arg2 が car 部であり、arg1 が cdr 部であるコンスを作り、それを返す。

<例>
        x = '(a b c)  y = '(d e f) のとき
        (xcons x y) -> ((d e f) a b c)
        (xcons 'a 'b) -> (b . a)"
  (cons arg2 arg1))

;;; xor#                                   ロカティブオペレータ
;;; 
;;; <説明>
;;;   形式 : loc1 xor# loc2
;;; 2 つの引数のビット xor (exclusive or) 操作を行う。
;;; 
;;; <例>
;;;         (signed-integer-locatives p q r) -> (p q r)
;;;         (p <- #5252) -> 2730
;;;         (q <- #7070) -> 3640
;;;         (r <- (p xor# q )) -> 1170 (#2222)
;;; ＠
