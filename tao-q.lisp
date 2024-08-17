;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(cl:in-package tao-internal)


(define
 "qd"
 (expr nil)
 :documentation
 "形式 : qd &opt pathname output-stream
関数 vdir の様に動作する。pathname にマッチする全ての削除された
ファイルについての情報がアルファベット順にプリントされる。"
 :example
 "")


(define
 "qdt"
 (expr nil)
 :documentation
 "形式 : qdt &opt pathname output-stream
関数 qdt は、関数 tdir が関数 vdir に対するのと同じ関係を関数 qd に
対して持つ。pathname にマッチする全ての削除されたファイルについての
情報が、削除された時間順にプリントされる。"
 :example
 "")


(define
 "tao.sys:quantum-remaining"
 (variable 0)
 :documentation
 "カレントのプロセス処理のために残っている単位クロック（時間）を表す数。
単位クロックが 1 つ進むごとに 1 つずつ減っていく。"
 :example
 "tao.sys:quantum-remaining -> 3")


(define
 "quit-spy"
 (expr nil)
 :documentation
 "スパイを中止する。"
 :example "")


(define
 "quote"
 (macro (x) `(quote ,x))
 :documentation
 "形式 : quote object
単に object を返す。object は、アトム、リスト、関数など、どんな
オブジェクトでもよい。省略して記述することも可能。
  (quote object) = 'object
  TAO では、'object は (quote object) のように解釈されるのではなく、
タグで表される。
オブジェクトのクオート用タグが ON のとき、クオートが付けられる。
オブジェクトのクオート用タグが OFF のとき、クオートが付いてない。"
 :example
 "(quote x) = 'x -> x")


(define
 "quotedp"
 (subr nil)
 :documentation
 "形式 : quotedp form
form がクォートのついた式ならば、それを返し、そうでなければ nil を返す。"
 :example
 "(quotedp ''a) -> 'a
        (quotedp 'a) -> nil
        (quotedp '(quote a)) -> nil
        ただし、'a は quote と a から成るリストではないので要注意。")


(define
 "quotient"
 (subr (&rest numbers)
   (typecase numbers
     (null nil)
     ((cons number null) (car numbers))
     (otherwise
      (multiple-value-bind (q r)
                           (floor (car numbers) (apply #'* (cdr numbers)))
        (if (zerop r)
            (coerce q (type-of r))
            (values (coerce q (type-of r)) r))))))
 :documentation
 "形式 : quotient number1 &rest number2
number1 の値を number2 の値で割り、その結果を返す。
number2 で複数の値が指定されたら連続して割り算を行なう。
結果が整数にならなければ商と余りを返し、余りが無ければ商だけを返す。
(// x1 x2 ...) と同じ。"
 :example
 "(quotient 5 4) -> !(1 1)
        (quotient 3) -> 3
        (quotient 100.0 4 5.0) -> 5.0
        (quotient) -> nil")


(define
 "quotify"
 (expr (x)
   (list 'tao:quote x))
 :documentation
 "形式 : quotify object
object にクォートを付けた値を返す。"
 :example
 "(quotify (list 1 2 3)) -> '(1 2 3)")


(defun var-name-p (expr)
  (and (symbolp expr)
       (eql 0 (position #\_ (string expr)))))


(defun unquotify (expr)
  (typecase expr
    (null '())
    ((and vector (not string))
     (cons 'vector
           (map 'list
                (lambda (elt)
                  (unquotify elt))
                expr)))
    (atom (if (var-name-p expr) ;TODO
              expr
              `',expr))
    ((cons (eql tao:unquote)) (cadr expr))
    (cons (list 'cons
                (unquotify (car expr))
                (unquotify (cdr expr))))))


(defun logvar-setter-exist-p (env)
  (multiple-value-bind (ftype localp)
                       (#+lispworks hcl:function-information
                        #+sbcl sb-cltl2:function-information
                        'tao.logic::logvar-setter env)
    (and ftype localp)))


(defmacro tao::query (log &rest args)
  (let ((cont (gensym "cont")))
    `(with-return-from-reval ,cont (,log ,args)
       (funcall ,log
                ,@(mapcar #'unquotify args)
                #',cont))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun qq-expand-list (x depth)
  (if (consp x)
      (case (car x)
        ((tao::quasiquote)
         (list (quote list)
               (list 'cons (list (quote quote) (car x))
                     (qq-expand (cdr x) (+ depth 1)))))
        ((tao::unquote tao::unquote-splicing)
         (cond ((> depth 0)
                (list (quote list)
                      (list (quote cons)
                            (list (quote quote)
                                  (car x))
                            (qq-expand (cdr x) (- depth 1)))))
               ((eq (quote tao::unquote) (car x))
                (list* (quote list)
                       (cdr x)))
               (:else
                (list* (quote append)
                       (cdr x)))))
        (otherwise
         (list (quote list)
               (list (quote append)
                     (qq-expand-list (car x) depth)
                     (qq-expand (cdr x) depth)))))
      (list (quote quote)
            (list x) )))

(defun qq-expand (x depth)
  (typecase x
    (cons (case (car x)
            ((tao::quasiquote)
             (list (quote cons)
                   (list (quote quote)
                         (car x) )
                   (qq-expand (cdr x) (+ depth 1)) ))
            ((tao::unquote tao::unquote-splicing)
             (cond ((> depth 0)
                    (list (quote cons)
                          (list (quote quote)
                                (car x) )
                          (qq-expand (cdr x) (- depth 1)) ))
                   ((and (eq (quote tao::unquote) (car x))
                         (not (null (cdr x)))
                         (null (cddr x)) )
                    (cadr x) )
                   (:else
                    (error "Illegal") )))
            (otherwise
             (list (quote append)
                   (qq-expand-list (car x) depth)
                   (qq-expand (cdr x) depth) ))))
    (simple-vector 
     (coerce (eval (qq-expand (coerce x 'list) depth)) ;FIXME
             'simple-vector))
    (T (list (quote quote) x)))))


(defmacro tao::quasiquote (&whole form expr)
  (if (eq (quote tao::quasiquote) (car form))
      (qq-expand expr 0)
      form ))


;;; *EOF*
