;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(defpackage tao.object 
  (:use c2cl))


(cl:in-package tao.object)


(defclass tao::tao-class (standard-class) ())


(defclass tao::tao-object (standard-object)
  ()
  (:metaclass tao::tao-class))


(defmethod print-object ((obj tao::tao-object) stream)
  (let ((adr #+lispworks (sys:object-address obj)
             #-lispworks ""))
    (format stream "{udo}~(~X~)" adr)))


(defmethod validate-superclass ((subclass tao::tao-class) (superclass standard-class))
  T)


(defclass tao::logical-class (tao::tao-class) ())


(defvar *instance-facts* (make-hash-table))


(defmethod make-instance :around ((class logical-class)
                                  &rest initargs
                                  &key (name (gentemp (string (class-name class)))))
  (let ((ins (call-next-method)))
    (setf (gethash ins *instance-facts*)
          name)
    ins))


(defun object-name (obj)
  (gethash obj *instance-facts*))


(defun make-setter-definition (slotd)
  (let ((slot-name (slot-definition-name slotd))
        (setter-fn (car (slot-definition-writers slotd))))
    `(setf (fdefinition ',(intern (concatenate 'string
                                               (string '#:set-)
                                               (string slot-name))
                                  (symbol-package slot-name)))
           (lambda (obj val) (funcall #',setter-fn val obj)))))


(defun compute-getter-setter (slot gettable settable)
  (let* ((getter (etypecase gettable
                   (null '())
                   (symbol slot)
                   (cons (and (member slot gettable) slot))))
         (setter (and (null getter)
                      (etypecase settable
                        (null '())
                        (symbol slot)
                        (cons (and (member slot settable)
                                   (make-symbol (concatenate 'string
                                                             (string '#:set-)
                                                             (string slot)))))))))
    `(,slot
      ,@(and setter (list :writer setter))
      ,@(and getter (list :accessor getter)))))


(defun tao-slot-form->cl-slot-from (cvs ivs &key gettable settable)
  (append (mapcar (lambda (slot)
                    (etypecase slot
                      (symbol
                       (append (compute-getter-setter slot gettable settable)
                               (list :allocation :class :initarg slot)))
                      ((cons symbol *)
                       (append (compute-getter-setter (elt slot 0) gettable settable)
                               (list :initform (elt slot 1) :allocation :class :initarg (elt slot 1))))))
                  cvs)
          (mapcar (lambda (slot)
                    (etypecase slot
                      (symbol
                       (append (compute-getter-setter slot gettable settable)
                               (list :initarg slot)))
                      ((cons symbol *)
                       (append (compute-getter-setter (elt slot 0) gettable settable)
                               (list :initform (elt slot 1) :initarg (elt slot 1))))))
                  ivs)))


(defun find-gettable-option (options)
  (some (lambda (x)
          (typecase x
            ((or (eql :gettable)
                 (cons (eql :gettable) *))
             x)
            (T nil)))
        options))


(defun find-settable-option (options)
  (some (lambda (x)
          (typecase x
            ((or (eql :settable)
                 (cons (eql :settable) *))
             x)
            (T nil)))
        options))


(defmethod ensure-class-using-class :after ((class tao::tao-class) name &rest initargs)
  (dolist (slotd (class-direct-slots class))
    (eval (make-setter-definition slotd))))


(defmacro tao:defclass (class-name (&rest class-vars) (&rest inst-vars) &optional supers &rest options)
  "defclass                               関数[#!macro]

<説明>
  形式 : defclass 'class-name 'class-vars 'inst-vars
                   &opt 'supers &rest 'options
クラスベクタと呼ばれるベクタを作成し、class-name で指定された識別子
の属性リストに格納する。
class-name が、この作成されたクラスの名前を表し、作成したクラスの名前を
返す。
class-vars はクラス変数を宣言する部分であり、その要素が cv のよう
な識別子または (cv cvini) のようなリストであるリストとして宣言する。
class-vars の要素が例えば (cv cvini) というリストである場合、car 部で
ある cv はクラス変数を宣言し、cdr 部である cvini はクラス変数cv の初期
値となる。この cvini 式はそのクラスが定義される時に各々一回評価される。
inst-vars はインスタンス変数を宣言する部分で、宣言の仕方はクラス変数
の場合と同様である。 inst-vars においても、 class-vars と同様に、インス
タンス変数の初期値設定ができる。
supers は、作成されるクラスのスーパクラス名である識別子から成るリストで
ある。 supers の既定値は nil 。
options で種々のオプションを指定する。もし、そのオプションの 1 つ
:no-vanilla-class が options として与えられなければ、vanilla-class が作
成されるクラスのスーパクラスになる。
;;
クラスはスーパクラスやサブクラスで示されるように階層構成をとる。
クラスの作成はこの階層構成の上にあるクラスから順に行なう方が望ましい。
ただし、関連のあるクラスがすべて作成された後に、それらのクラスの
インスタンスの最初の 1 つが関数 make-instance によって作られる場合には、
この関数の適用の順序は階層構成の順である必要はない。
;;
クラスの構造を示すクラスベクタはそのジェネレーションによって管理される。
つまり、あるクラスが同じクラス名によって再定義された場合、このクラスの
バージョンナンバーが 1 増して、このクラスのジェネレーションが変わって
いることを示す。さらにこのクラスのサブクラスのバージョンナンバーも
すべて最新のものになる。
;;
クラスは、そのすべてのスーパクラスの全インスタンス変数と全メソッドを
継承するが、クラス変数は継承しない。
;;
<例>
        (defclass a1 () ((b11 1) (b12 2)) () :gettable) -> a1
        (defclass a2 () ((b21 3) (b22 4)) (a1) :gettable) -> a2
        (defclass a3 () ((b31 5) (b32 6)) (a2) :gettable) -> a3
        (!aa (make-instance 'a3)) -> {udo}40766a3
        [aa b11] -> 1
        [aa b22] -> 4"
  (let ((gettable (find-gettable-option options))
        (settable (find-settable-option options)))
    `(progn
       (cl:defclass ,class-name (,@supers tao::tao-object)
         ,(tao-slot-form->cl-slot-from class-vars
                                       inst-vars
                                       :gettable gettable
                                       :settable settable)
         (:metaclass tao::tao-class))
       ',class-name)))


;;; *EOF*
