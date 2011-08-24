(in-package #:tao-internal)
(in-readtable :tao)

#|
day-of-week-string                     関数[#!expr]

<説明>
  形式 : day-of-week-string number
number に対応する曜日名を文字列で返す。number が 0〜6 以外の時は、nil
を返す。

<例>
        (day-of-week-string 0) -> "Sun"
        (day-of-week-string 1) -> "Mon"
        (day-of-week-string 2) -> "Tue"
|#


(defun tao:day-of-week-string (number)
  (case number
    (0 "Sun")
    (1 "Mon")
    (2 "Tue")
    (3 "Wed")
    (4 "Thu")
    (5 "Fri")
    (6 "Sut")
    (otherwise nil)))

(defmacro tao:dec (var &optional (val 1))
  "dec                                    関数[#!macro]

<説明>
  形式 : dec var &opt val
var の値から val の値を引き、その結果を返す。 val の既定値は 1 。
\(!!- !x n) と同じ。

<例>
        (!x 10)
        (dec x) -> 9
        x -> 9
        (dec x -2) -> 11
        x -> 11
        (dec 3 2) -> エラー
        (dec 3) -> エラー"
  `(setq ,var (- ,var ,val)))

(defmacro tao:define (symbol applobj)
  "define                                 関数[#!expr]

<説明>
  形式 : define symbol applobj
symbol を関数オブジェクト applobj に結び付ける。

<例>
        (define fn (lambda (x y) (list x y)))
                 = (dye fn (x y) (list x y))
        (define fn (expr (x y) (list x y)))
                 = (de fn (x y) (list x y))
        (define aa (array 10)) -> aa"
  `(progn
     (setf (symbol-function ',symbol) #'values)
     (setf (symbol-function ',symbol)) ,applobj))

;; deは、define exprの略
;; defunとの差異が不明 拡張されたlambdaリストが取れるのが、
;; defunかもしれない。
(defmacro tao:de (fn var-list &body body)
  "de                                     関数[#!expr]

<説明>
  形式 : de 'fn 'var-list &rest 'body
fn が関数名、var-list が引数リストである expr 型関数、すなわち
スコープ限定型関数を body で定義。

<例>
        (de fact (n)
          (cond ((n = 0) 1)
                (t (n * (fact (n - 1)))) )) -> fact
        fact は、階乗の計算をする関数。
        (de cell-count (x)
          (cond ((consp x)
                 (+ 1 (cell-count (car x)) (cell-count (cdr x))))
                (t 0) ))  ->  cell-count"
  ;; evalは、null lexical environmentにするために利用
  `(eval
    '(defun ,fn ,(substitute '&optional '&opt var-list)
      ,@body)))

(defmacro tao:do-forever (&body body)
  "do-forever                             関数[#!macro]

<説明>
  形式 : do-forever &rest body
body の中のどこかで return を実行するまで body の中の式の実行を繰り
返す。 body の中に return がなければ、繰り返しをいつまでも続ける。

<例>
        (do-forever
        (!aa (read))
          (if (equal aa 'ok) (retrun nil) (prins  \"input ok? : \")))
        \"ok\" と入力しないかぎり、do-foreverから抜け出ることはできない。"
  (let ((=> (gensym)))
    `(block nil
       (tagbody
          ,=>
          ,@body
          (go ,=>)))))

;; コンパイラが自動でスペシャルにするのを期待するという手抜き
;; 本格的にはCodewalkerを利用しないと駄目
(defmacro tao:dye (fn var-list &body body)
  "dye                                    関数[#!expr]

<説明>
  形式 : dye 'fn 'var-list &rest 'body
fn を名前、var-list を引数リストとする exprdyn 型関数 (スコープ透過
関数) を body で定義する。

<例>
        (dye some-routine ()
             (!state (transition state))
             (cond ((state = 4) (!d-flag t))
                   ((state = 10) (!d-flag nil)) )
             (!result (fn state result)) )  ->  some-routine"
  `(defun ,fn ,var-list
     (declare (special ,@var-list))
     ,@body))
