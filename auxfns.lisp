;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

(in-package :tao.logic)


;;;; Implementation-Specific Details

;;;; REQUIRES

;;; The function REQUIRES is used in subsequent files to state dependencies
;;; between files.  The current definition just loads the required files,
;;; assumming they match the pathname specified in *PAIP-DIRECTORY*.
;;; You should change that to match where you have stored the files.
;;; A more sophisticated REQUIRES would only load it if it has not yet
;;; been loaded, and would search in different directories if needed.

;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro once-only (variables &body body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list .,variables))
	(progn .,body)
	(list 'let
	 ,`(list ,@(mapcar #'(lambda (tmp var)
			       `(list ',tmp ,var))
			   temps variables))
	 (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
		       variables temps)
	   .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
      `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (cl:ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (find-anywhere item (first tree))
	    (find-anywhere item (rest tree)))))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )


;;;; Auxiliary Functions

(setf (symbol-function 'find-all-if) #'remove-if-not)


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))


(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))


;;; ==============================

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))


(defun maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array) 
            (max (fill-pointer array) new-length))))


;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(defun symbol+ (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~:@(~{~A~}~)" args) "TAO.LOGIC"))


(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~A~}" args)))


(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))


;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))


(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))


(defun flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))


(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))


;;; ==============================

(defun member-equal (item list)
  (member item list :test #'equal))


;;; ==============================

(defun compose (&rest functions)
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))


;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")


(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))


(defun debug* (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))


(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))


;;; ==============================

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))


;;;; PATTERN MATCHING FACILITY

(defconstant fail nil)


(defconstant no-bindings (if (boundp 'no-bindings)
			     (symbol-value 'no-bindings)
			     '((t . t))))


(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))


(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))


(defun make-binding (var val) (cons var val))


(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))


(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))


(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))


(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))


(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\_)))


;;; ==============================

;;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))


(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))


(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))


(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))


;;;; Delayed computation:

(defstruct delay value (computed? nil))


(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value #'(lambda () . ,body)))


(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))


;;;; Defresource:

(defmacro defresource (name &key constructor (initial-copies 0)
                       (size (max initial-copies 10)))
  (let ((resource (symbol+ '* (symbol+ name '-resource*)))
        (deallocate (symbol+ 'deallocate- name))
        (allocate (symbol+ 'allocate- name)))
    `(progn
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc #',deallocate (loop repeat ,initial-copies 
                                       collect (,allocate))))
       ',name)))


(defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol+ 'allocate- resource))
        (deallocate (symbol+ 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))


;;;; Queues:

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))


(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))


(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)


(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)


(defun front (q) (first (queue-contents q)))


(defun empty-queue-p (q) (null (queue-contents q)))


(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))


;;;; Other:

(defun sort* (seq pred &key key) 
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))


(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


;;; ==============================

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))


(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))


;;; ==============================

#|(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))|#


(defgeneric unique-find-if-anywhere (predicate tree &optional found-so-far)
  (:documentation "return a list of leaves of tree satisfying predicate,
  with duplicates removed."))

(defmethod unique-find-if-anywhere (predicate (tree cl:cons) &optional found-so-far)
  (unique-find-if-anywhere predicate
                           (first tree)
                           (unique-find-if-anywhere predicate (rest tree)
                                                    found-so-far)))

(defmethod unique-find-if-anywhere (predicate (tree vector) &optional found-so-far)
  (let ((tree (coerce tree 'list)))
    (unique-find-if-anywhere predicate
                             (first tree)
                             (unique-find-if-anywhere predicate (rest tree)
                                                      found-so-far))))


(defmethod unique-find-if-anywhere (predicate (tree string) &optional found-so-far)
  (if (funcall predicate tree)
      (adjoin tree found-so-far)
      found-so-far))


(defmethod unique-find-if-anywhere (predicate tree &optional found-so-far)
  (if (funcall predicate tree)
      (adjoin tree found-so-far)
      found-so-far))


#|(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))|#


(defgeneric find-if-anywhere (predicate tree)
  (:documentation "Does predicate apply to any atom in the tree?"))


(defmethod find-if-anywhere (predicate (tree cons))
  (or (find-if-anywhere predicate (first tree))
      (find-if-anywhere predicate (rest tree))))


(defmethod find-if-anywhere (predicate (tree vector))
  (let ((tree (coerce tree 'list)))
    (or (find-if-anywhere predicate (first tree))
        (find-if-anywhere predicate (rest tree)))))


(defmethod find-if-anywhere (predicate (tree string))
  (funcall predicate tree))


(defmethod find-if-anywhere (predicate tree)
  (funcall predicate tree))


;;; ==============================

(defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(symbol+ type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(symbol+ 'symbol-> type) (symbol+)
       (position symbol ',elements))
     ,@(loop for element in elements
             for i from 0
             collect `(defconstant ,element ,i))))


;;; ==============================

(defun not-null (x) (not (null x)))


(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))


(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; *EOF*
