;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package tao-user)
(named-readtables:in-readtable :tao)


(&progn (&aux _a _b)
  (== _a 3)
  (== _b _a)
  (list _a _b))
;(3 3)

(let ((x '(0 1 2 (:kmacrou (@) nil)))
      (j (list '!)))
  (&progn (&aux _f)
    (!(cdr j)
      (if (== (_ _ _ (:kmacrou _f nil)) ,x)
          _f
          (cdr x)))
    j))
;(! . (@)) 


(define &conc
  (Hclauses
    (&+ (( ) _y _y))
    (&+ ((_a . _x) _y (_a . _z))
        (&conc _x _y _z))))

(&progn (&aux _x _y _z)
  (== _x (0 1 2))
  (== _y (3 4 5))
  (&conc _x _y _z)
  (list _x _y _z))
;((0 1 2) (3 4 5) (0 1 2 3 4 5)) 


(&progn (&aux _x _y _z)
  (== _x '(0 1 2))
  (== _y '(3 4 5))
  (funcall (tao.logic::compile-anonymous-predicate 3
                                                   '(((&+ ( ) _y _y))
                                                     ((&+ (_a . _x) _y (_a . _z))
                                                      (append _x _y _z) )))
           _x _y _z
           #'tao.logic::logvar-setter)
  _z)
;(0 1 2 3 4 5) 


(define append
  (Hclauses
    (&+ (( ) _y _y))
    (&+ ((_a . _x) _y (_a . _z))
        (append _x _y _z))))

(&progn (&aux _x _y _z)
  (== _x (0 1 2))
  (== _y (3 4 5))
  (query (Hclauses
           (&+ (( ) _y _y))
           (&+ ((_a . _x) _y (_a . _z))
               (append _x _y _z)))
         _x _y _z)
  _z)
;(0 1 2 3 4 5) 

(define &conc
  (Hclauses
    (&+ (( ) _y _y))
    (&+ ((_a . _x) _y (_a . _z))
        (&conc _x _y _z))))

(define &append
  (Hclauses
    (&+ (( ) _y _y))
    (&+ ((_a . _x) _y (_a . _z))
        (&append _x _y _z))))


(define &reverse
  (Hclauses
    (&+ ((_x . _l0) _l)
        (&reverse _l0 _l1)
        (&append _l1 (_x) _l))
    (&+ (() ()))))

(&progn (&aux _x _y _z _r)
  (== _x (0 1 3))
  (== _y (0 1 3))
  (&append _x _y _z)
  (&reverse _z _r)
  _r)
;(3 1 0 3 1 0) 

(&progn (&aux _z)
  (query (&+ ((_a . _x) _y (_a . _z))
             (append _x _y _z))
         (0 1 2)
         (3 4 5)
         _z)
  _z)
;(0 1 2 3 4 5)

(&progn (&aux _x)
  (query (&+ ((_ _x . _)) (write _x))
         (alpha beta gamma delta) ))
;▻ BETA
;→ T

(&progn (&aux _x _y)
  (query (&+ ((kkk kk k)))
         (_x . _y))
  (write _x)
  (write _y) )
;▻ KKK(KK K)
;→ (KK K)


(&progn (&aux _ans _u)
  (== _u (1 2 3 4))
  (== _ans _u)
  (&reverse _u _ans)
  _ans)
;→ (1 2 3 4)

(&progn (&aux _ans _u)
  (== _u (1 2 3 4))
  (== _ans _u)
  (setq _ans (undef))
  (&reverse _u _ans)
  _ans)
;→ (4 3 2 1)

(&progn (&aux _a _z)
  (query (&+ (_x _x))
         (((_a)) kk . _z)
         ((_z) _a _a))
  (list _a _z))
;→ (KK (KK))

(assert (append ( ) _y _y))
(assert (append (_a . _x) _y (_a . _z))
        (append _x _y _z) )


(&progn (&aux _x _y _z _r)
  (== _x (0 1 2))
  (== _y (3 4 5))
  (append _x _y _z)
  _z)
;→ (0 1 2 3 4 5)

(setf (tao.logic::get-clauses 'lisp) T)
(defmacro lisp (&rest args)
  `(tao.logic::lisp/2 ,@(mapcar #'tao-internal::unquotify args) (constantly T)))

(let ((a 42))
  (&progn (&aux _x _y)
    (== _x 3)
    (lisp _y [_x + ,a])
    _y))
;→ 45

(&progn (&aux _x _y)
  (== _x 3)
  (== _y ,[_x + 10])
  (list _x _y))
;→ (3 13)




(progn
;;; original
(assert (queen _n _l)
        (generate _n _l1)
        (try _n _l1 () _l () ()) )
(assert (generate 0 ()))
(assert (generate _n (_n . _l))
        (> _n 0)             ; この > は lisp 関数
        (== _n1 ,(1- _n))
        (generate _n1 _l) )
(assert (try _ () _l _l _ _))
(assert (try _m _s _l1 _l _c _d)
        (choose _s _a _s1)
        (== _c1 ,(_m + _a))
        (notmem _c1 _c)
        (== _d1 ,(_m - _a))
        (notmem _d1 _d)
        (try ,(_m - 1) _s1 (_a . _l1)
             _l (_c1 . _c) (_d1 . _d) ) )
(assert (choose (_a . _l) _a _l))
(assert (choose (_a . _l) _x (_a . _l1))
        (choose _l _x _l1) )
(assert (notmem _ ()))
(assert (notmem _a (_b . _l))
        (not (== _a _b))        ; not は lisp 関数
        (notmem _a _l) ))


(progn
;;; WIP
(assert (queen _n _l)
        (generate _n _l1)
        (try _n _l1 () _l () ()) )
(assert (generate 0 ()))
(assert (generate _n (_n . _l))
        (> _n 0)             ; この > は lisp 関数
        (== _n1 ,(1- _n))
        (generate _n1 _l) )
(assert (try _ () _l _l _ _))
(assert (try _m _s _l1 _l _c _d)
        (choose _s _a _s1)
        (== _c1 ,(_m + _a))
        (notmem _c1 _c)
        (== _d1 ,(_m - _a))
        (notmem _d1 _d)
        (try ,(_m - 1) _s1 (_a . _l1)
             _l (_c1 . _c) (_d1 . _d) ) )
(assert (choose (_a . _l) _a _l))
(assert (choose (_a . _l) _x (_a . _l1))
        (choose _l _x _l1) )
(assert (notmem _ ()))
(assert (notmem _a (_b . _l))
        (not (== _a _b))        ; not は lisp 関数
        (notmem _a _l) ))


(&progn (&aux _x _y _z _r)
  (== _x ())
  (== _y (3 4 5))
  (append _x _y _z)
  _z)
;→ (3 4 5)


(goal
  (== _x ())
  (== _y (3 4 5))
  (append _x _y _z))

