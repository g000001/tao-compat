;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.
(tao:common-lisp)


(in-package :tao.logic)


(defvar *negation-as-failure* nil)


(defun tao:negation-as-failure (&optional (negation-as-failure? nil negation-as-failure?-sup?))
  (if negation-as-failure?-sup?
      (setq *negation-as-failure* negation-as-failure?)
      *negation-as-failure*))


(define-compiler-macro tao:negation-as-failure (&whole w &optional (negation-as-failure? nil negation-as-failure?-sup?))
  (declare (ignore negation-as-failure?))
  (if negation-as-failure?-sup?
      *negation-as-failure*
      w))


;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))


(defun clause-body (clause) (rest clause))


;; clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))


(defun (setf get-clauses) (val pred) (setf (get pred 'clauses) val))


(defun predicate (relation)
  (if (atom relation)
      relation
      (first relation)))


#|(defun args (x)
  "The arguments of a relation"
  (if (atom x)
      nil
      (rest x)))|#

(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")


(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause ',(replace-_-vars clause)))


(defun add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))


(defun clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))


(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))


(defun rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))


;; cf. unique-find-if-anywhere 
(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (unique-find-if-anywhere predicate tree found-so-far))


;; cf. find-if-anywhere
(defun find-anywhere-if (predicate tree)
  (find-if-anywhere predicate tree))


(defmacro ?- (&rest goals) `(top-level-prove ',(replace-_-vars goals)))


(defmacro ?-all (&rest goals) `(top-level-prove-all ',(replace-_-vars goals)))


(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))


(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
          #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all
                  (append (clause-body new-clause) other-goals)
                  (unify goal (clause-head new-clause) bindings))))
          clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))


(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (format t "~&no")
  (values))


(defun top-level-prove-all (goals)
  (prove-all `(,@goals (show-all-prolog-vars ,@(variables-in goals)))
             no-bindings)
  'tao::that\'s-all)


(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (force-output)
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))


(defun show-all-prolog-vars (vars bindings other-goals)
  (declare (ignore other-goals))
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (force-output)
  fail)


(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)


(setf (get 'show-all-prolog-vars 'clauses) 'show-all-prolog-vars)


(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (variables-in/ exp))

(defun variables-in// (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))


(defun variables-in/ (exp)
  (declare (ignore exp))
  '())


(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '_))))


(defun replace-_-vars (exp)
    "Replace any _ within exp with a var of the form _123."
    (cond ((eq exp '_) (gensym "_"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-_-vars (first exp))
			 (replace-_-vars (rest exp))
			 exp))))


;;; *EOF*
