;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.

(tao:common-lisp)


(in-package :tao.logic)


(defun fail/0 (cont)
  "7.8.1"
  (declare (cl:ignore cont))
  nil)


(defun true/0 (cont)
  "7.8.2"
  (funcall cont))


(defun call/1 (_g cont)
  "7.8.3: Try to prove goal by calling it."
  (deref _g)
  (cond
    ((or (goal-and-p _g) (goal-or-p _g) (goal-if-p _g))
     ;; FIXME: this use of a temporary predicate name is ugly,
     ;; non-threadsafe and basically evil bad and wrong.
     (funcall (compile-predicate 'call/1-tmp-fun 0 (list _g)) cont))
    (t (apply (make-predicate (predicate _g) (length (args _g)))
              (append (args _g) (list cont))))))


(defun !/0 (cont)
  "7.8.4"
  (funcall cont))


;;; 8.2 term unification

(let (#+lispworks (lw:*handle-warn-on-redefinition* nil))
  (defgeneric :unify-next-element (obj)))


;;tao:==
(defun ==/2 (_arg1 _arg2 cont)
  "8.2.1"
  (typecase _arg2
    ((and standard-object
          (not stream)) 
     (when (and (listp _arg1)
                (unify! _arg1 (:unify-next-element _arg2)))
       (funcall cont)))
    (T (when (unify! _arg1 _arg2)
         (funcall cont)))))


(defun &instance-fact/2 (obj _args cont)
  (let ((ifact (make-predicate (tao.object::object-name obj) (length _args))))
    (if (fboundp ifact)
        (let ((args (append _args (list cont))))
          (declare (dynamic-extent args))
          (apply (make-predicate (tao.object::object-name obj) (length _args))
                 args))
        (error "Instance fact does not exist for ~S." obj))))


(defmacro &instance-fact (obj args)
  (let ((cont (gensym "cont")))
    `(tao-internal::with-return-from-reval ,cont (,args)
       (&instance-fact/2 ,obj ',args #',cont))))


(defun unify-with-occurs-check/2 (_arg1 _arg2 cont)
  "8.2.2"
  (let ((*occurs-check* t))
    (when (unify! _arg1 _arg2)
      (funcall cont))))


(defun \\=/2 (_arg1 _arg2 cont)
  "8.2.3"
  (unless (unify! _arg1 _arg2)
    (funcall cont)))


;;; 8.3 type testing

(defun atomicp (exp)
  ;; not ATOM, because we might be implementing unbound VARs as
  ;; something returning true to lisp's ATOM.
  (or (symbolp exp)
      (integerp exp)
      (floatp exp)))


(macrolet ((define-type-testing-predicate (name docstring fun)
               `(defun ,name (x cont)
                 ,docstring
                 (when (,fun (deref x))
                   (funcall cont)))))
  (define-type-testing-predicate var/1 "8.3.1" unbound-var-p)
  (define-type-testing-predicate atom/1 "8.3.2" symbolp)
  (define-type-testing-predicate integer/1 "8.3.3" integerp)
  (define-type-testing-predicate real/1 "8.3.4" floatp)
  (define-type-testing-predicate atomic/1 "8.3.5" atomicp)
  (assert (not (consp (_))))
  (define-type-testing-predicate compound/1 "8.3.6" consp)
  (define-type-testing-predicate nonvar/1 "8.3.7" nonvarp)
  ;; strictly, this should be (OR INTEGERP FLOATP).
  (define-type-testing-predicate number/1 "8.3.8" numberp))


;;; 8.4 term comparison

(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x) (consp y)
           (deref-equal (car x) (car y))
           (deref-equal (cdr x) (cdr y)))))


;; tao:=
(defun =/2 (_x _y cont)
  "8.4.1: Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (when (deref-equal _x _y)
    (funcall cont)))


(defun \\==/2 (_x _y cont)
  "8.4.2"
  (unless (deref-equal _x _y)
    (funcall cont)))


(defun term-precedes (x y)
  (and (not (eql (deref x) (deref y)))
       (typecase x
	 (var (or (not (var-p y))
		    ;; FIXME
		    (evenp (random 2))))
	 ((float) (if (var-p y)
		      nil
		      (or (not (floatp y))
			  (< x y))))
	 ((integer) (if (or (var-p y) (floatp y))
			nil
			(or (not (integerp y))
			    (< x y))))
	 (cl:symbol (if (or (var-p y) (floatp y) (integerp y))
			nil
			(or (not (symbolp y))
			    (string< (string x) (string y)))))
	 ;; "compound term": i.e. not lists, really
	 ((cons) (when (consp y)
		   (or (< (length x) (length y))
		       (and (= (length x) (length y))
			    (or (term-precedes (car x) (car y))
				(and (eql (car x) (car y))
				     (do* ((xis (cdr x) (cdr xis))
					   (xi (car xis) (car xis))
					   (yis (cdr y) (cdr yis))
					   (yi (car yis) (car yis)))
					  ((null xis) nil)
				       (when (term-precedes xi yi)
					 (return t))))))))))))


(defun @</2 (_x _y cont)
  "8.4.3"
  (when (term-precedes _x _y)
    (funcall cont)))


(defun @=</2 (_x _y cont)
  "8.4.4"
  (when (or (deref-equal _x _y)
	    (term-precedes _x _y))
    (funcall cont)))


(defun @>/2 (_x _y cont)
  (when (term-precedes _y _x)
    (funcall cont)))


(defun @>=/2 (_x _y cont)
  (when (or (deref-equal _y _x)
	    (term-precedes _y _x))
    (funcall cont)))


;;; 8.5 term creation and decomposition

(defun functor/3 (_term _name _arity cont)
  "8.5.1"
  (cond
    ((unbound-var-p _term)
     (assert (not (unbound-var-p _name)))
     (assert (not (unbound-var-p _arity)))
     (when (unify! _term (list* (deref _name)
                                (loop repeat (deref _arity) collect (_))))
       (funcall cont)))
    (t (if (atomicp (deref _term))
           (when (and (unify! _arity 0)
                      (unify! _name _term))
             (funcall cont))
           (when (and (unify! _arity (length (cdr _term)))
                      (unify! _name (or (and(car _term)))))
             (funcall cont))))))


(defun arg/3 (_n _term _arg cont)
  "8.5.2"
  (when (unify! (nth (deref _n) (deref _term)) _arg)
    (funcall cont)))


(defun =.. (_term _list cont)
  "8.5.3"
  (declare (ignore _term _list cont))
  ;; FIXME: have to decide how to represent Prolog lists
  )


(defun make-renamed-copy (term)
  (cond
    ((unbound-var-p term) (_))
    ((var-p term) term)
    ((atom term) term)
    (t (cons (make-renamed-copy (car term))
             (make-renamed-copy (cdr term))))))


(defun copy-term/2 (_term1 _term2 cont)
  "8.5.4"
  (when (unify! (make-renamed-copy (deref _term1)) (deref _term2))
    (funcall cont)))


(defun lispp-uq/1 (unquoted-exp cont)
  (when (not (null unquoted-exp))
    (funcall cont)))


;;; FIXME: 8.8 clause retrieval and information

(defun clause/2 (_head _body cont)
  "8.8.1"
  (let ((clauses (get-clauses (predicate (deref _head)))))
    (let ((old-trail (trail-ndx *trail*)))
      (dolist (clause clauses)
	(when (unify! `(,_head . ,_body) clause)
	  (funcall cont))
        (undo-bindings! *trail* old-trail)))))


(defun current-predicate/1 (_pi cont)
  "8.8.2"
  (declare (ignore _pi cont))
  ;; FIXME: need to refactor *DB-PREDICATES* so that it contains arity
  ;; information
  )


;;; 8.9 clause creation and destruction

(defun asserta/1 (_clause cont)
  "8.9.1"
  (let* ((old-trail (trail-ndx *trail*))
	 (_head (_))
	 (_body (_)))
    (unless (unify! (deref _clause) `(<- ,_head . ,_body))
      (undo-bindings! *trail* old-trail)
      (unify! `(,_head . ,_body) `(,_clause (true))))
    (add-clause (cons (deref _head) (deref _body)) :asserta t)
    (funcall cont)))


(defun assertz/1 (_clause cont)
  "8.9.2"
  (let* ((old-trail (trail-ndx *trail*))
	 (_head (_))
	 (_body (_)))
    (unless (unify! (deref _clause) `(<- ,_head . ,_body))
      (undo-bindings! *trail* old-trail)
      (unify! `(,_head . ,_body) `(,_clause (true))))
    (add-clause (cons (deref _head) (deref _body)))
    (funcall cont)))


(defun retract/1 (_clause cont)
  "8.9.3"
  (let* ((old-trail (trail-ndx *trail*))
	 (_head (_))
	 (_body (_)))
    (unless (unify! (deref _clause) `(<- ,_head . ,_body))
      (undo-bindings! *trail* old-trail)
      (unify! `(,_head . ,_body) `(,_clause (true))))
    (retract-clause (cons (deref _head) (deref _body)))
    (funcall cont)))


;;; 8.10 all solutions

;;; FIXME: I think this is right for FINDALL/3.  BAGOF/3 and SETOF/3
;;; have extra complicated stuff to do with witnesses.
(defun findall/3 (term goal bag cont)
  "8.10.1: Find all solutions to GOAL, and for each solution,
  collect the value of TERM into the list BAG."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof _x (p _x) _l) ==> _l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-exp term) answers))) 
    (if (and (not (null answers))
             (unify! bag (nreverse answers)))
        (funcall cont))))


(defun bagof/3 (exp goal result cont)
  "8.10.2: Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof _x (p _x) _l) ==> _l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (unify! result (nreverse answers)))
        (funcall cont))))


(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar #'(lambda (var) (cons (deref var) (_)))
                  (unique-find-anywhere-if #'var-p exp))
          exp))


(defun setof/3 (exp goal result cont)
  "8.10.3: Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof _x (p _x) _l) ==> _l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
                     (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify! result (delete-duplicates
                              answers
                              :test #'deref-equal)))
        (funcall cont))))


;;; FIXME: findall/3

;;; 8.15 logic and control

(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (trail-ndx *trail*)))
         ,(first body)
         ,@(loop for exp in (rest body)
                 collect '(undo-bindings! *trail* old-trail)
                 collect exp))))


(defun &repeat/0 (cont)
  "8.15.3"
  (loop (funcall cont)))


;;; *EOF*
