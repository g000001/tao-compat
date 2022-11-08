;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:common-lisp)


(defpackage tao.object 
  (:use c2cl))


(cl:in-package tao.object)


(defclass tao::tao-class (standard-class) ())


(defmethod validate-superclass ((subclass tao::tao-class) (superclass standard-class))
  T)


(defun mappend (function &rest lists)
  (loop :for results :in (apply #'mapcar function lists)
        :append results))


(defun depth-first-preorder-superclasses* (class)
  (if (eq class (find-class 'standard-object))
      '()
      (cons class (mappend (lambda (c)
                             (depth-first-preorder-superclasses* c))
                           (class-direct-superclasses class)))))


(defmethod compute-class-precedence-list ((class tao::tao-class))
  (append (remove-duplicates
           (depth-first-preorder-superclasses* class)
           :from-end T)
          (list (find-class 'standard-object)
                (find-class T))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Dan Weinreb                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Utilities that make use of the meta-object protocol.

;;; Abstract classes are classes that must never be
;;; instantiated.  They are only used as superclasses.
;;; In general, they perform two useful function:

;;; First, they can be used to define a "protocol",
;;; namely a set of generic functions that some
;;; group of classes should implement.

;;; Second, they can provide some utility methods
;;; that any of the subclasses is free to take
;;; advantage of, so that the subclasses can share
;;; code in a modular way.

;;; This implementation was provided to us by MOPmeister
;;; Pascal Costanza, of the Vrije Universiteit Brussel,
;;; Programming Technology Lab.

(define-condition instantiate-abstract (error)
  ((class-name :type symbol
               :initarg :class-name
               :reader instantiate-abstract-class-name))
  (:documentation "There was an attempt to instantiate an abstract class")
  (:report (lambda (c stream)
             (format stream "There was an attempt to make an instance of abstract class ~S"
                     (instantiate-abstract-class-name c)))))


(defclass tao::abstract-class (tao::tao-class)
  ()
  (:documentation
   "This is a metaclass.  Any class that has this as its metaclass
    is an abstract class.  An attempt to instantiate an abstract
    class signals a INSTANTIATE-ABSTRACT condition."))


(defmethod validate-superclass ((class tao::abstract-class) (superclass standard-class)) t)


(defmethod validate-superclass ((class standard-class) (superclass tao::abstract-class)) t)


(defvar *outside-abstract-class* nil)


(defmethod allocate-instance ((class tao::abstract-class) &key &allow-other-keys)
  (unless *outside-abstract-class*
    (error 'instantiate-abstract :class-name (class-name class))))


(defmethod class-prototype :around ((class tao::abstract-class))
  (let ((*outside-abstract-class* t))
    (call-next-method)))


;;; Abstract classes ends here.


(defclass tao:vanilla-class () 
  ()
  (:metaclass tao::abstract-class))


(defclass tao::logical-class (tao::tao-class)
  ())


(defmethod print-object ((obj tao:vanilla-class) stream)
  (let ((adr #+lispworks (sys:object-address obj)
             #-lispworks ""))
    (format stream "{udo}~(~X~)[~A]" adr (type-of obj))))


(defvar *instance-facts* (make-hash-table))


(defmethod make-instance :around ((class tao::logical-class)
                                  &rest initargs
                                  &key (name (gentemp (string (class-name class))))
                                  &allow-other-keys)
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


(defun compute-accessor (slot gettable)
  (let ((gettable (mapcar (lambda (g) 
                            (etypecase g
                              ((and symbol (not null)) (list g g))
                              (cons g)))
                          gettable)))
    (cadr (find slot gettable :key #'car))))


(defun compute-getter-setter (slot gettable settable)
  (let* ((getter (etypecase gettable
                   (null '())
                   (symbol slot)
                   (cons (compute-accessor slot gettable))))
         (setter (etypecase settable
                   (null '())
                   (symbol (and (not getter) slot))
                   (cons (and (member slot settable)
                              (make-symbol (concatenate 'string
                                                        (string '#:set-)
                                                        (string slot))))))))
    `(,slot
      ,@(and setter (list :writer setter))
      ,@(and getter (list :accessor getter)))))


(defun tao-slot-form->cl-slot-from (cvs ivs &key gettable settable)
  (append 
   ;; class vars
   (mapcar (lambda (slot)
             (etypecase slot
               (symbol
                (append (compute-getter-setter slot gettable settable)
                        (list :allocation :class :initarg slot)))
               ((cons symbol *)
                (let ((var (elt slot 0))
                      (val (elt slot 1)))
                  (append (compute-getter-setter (elt slot 0) gettable settable)
                          (list :initform val :allocation :class :initarg var))))))
           cvs)
   ;; instance vars
   (mapcar (lambda (slot)
             (etypecase slot
               (symbol
                (append (compute-getter-setter slot gettable settable)
                        (list :initarg slot)))
               ((cons symbol *)
                (let ((var (elt slot 0))
                      (val (elt slot 1)))
                  (append (compute-getter-setter var gettable settable)
                          (list :initform val :initarg var))))))
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


(defun find-abstract-class-option (options)
  (some (lambda (x)
          (typecase x
            ((eql :abstract-class) 'tao::abstract-class)
            ((eql :logical-class) 'tao::logical-class)
            (T nil)))
        options))


(defmethod ensure-class-using-class :after ((class tao::tao-class) name &rest initargs)
  (dolist (slotd (class-direct-slots class))
    (eval (make-setter-definition slotd))))


(defun parse-defclass-options (class-name options)
  (let ((defclass-options nil)
        (methods nil))
    (setq defclass-options
          (mapcan (lambda (opt)
                    (typecase opt
                      ((cons (eql :default-init-plist))
                       (list `(:default-initargs ,@(cdr opt))))
                      ((cons (eql :eval-when-instantiation))
                       (prog1 '()
                         (push 
                          `(defmethod initialize-instance :after ((tao:self ,class-name) &rest initargs)
                             ,@(cdr opt))
                          methods)))
                      (T '())))
                  options))
    (values defclass-options methods)))


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
        (settable (find-settable-option options))
        (metaclass (or (find-abstract-class-option options)
                       'tao::tao-class)))
    (multiple-value-bind (options methods)
                         (parse-defclass-options class-name options)
      `(progn
         (cl:defclass ,class-name (,@supers tao:vanilla-class)
           ,(tao-slot-form->cl-slot-from class-vars
                                         inst-vars
                                         :gettable gettable
                                         :settable settable
                                         )
           (:metaclass ,metaclass)
           ,@options)
         (progn ,@methods)
         ',class-name))))


(define-method-combination tao:assert ()
  ((assert-clauses (tao:assert . *)))
  (let ((cont (gensym "cont"))
        (tao.logic::*predicate* (gensym "anonymous-pred-")))
    `(tao-internal::with-return-from-pred ,tao.logic::*predicate* ,cont ()
       ,(tao.logic::compile-body
         (let ((firsts '())
               (lasts '()))
           (dolist (m assert-clauses)
             (case (second (method-qualifiers m))
               ((:first nil)
                (setq firsts `(,m ,@firsts)))
               (:last
                (setq lasts `(,m ,@lasts)))
               (:cut 
                (setq firsts `(,m ,@firsts))
                (return))))
           (destructuring-bind (first . tail)
                               (append (nreverse firsts) lasts)
             `((call-method ,first ,tail))))
         `#',cont
         tao.logic::no-bindings))))


;;; *EOF*
