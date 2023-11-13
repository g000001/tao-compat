;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologc.lisp: Final version of the compiler,
;;;; including all improvements from the chapter.
(tao:common-lisp)


(in-package :tao.logic)


(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
(defvar .unbound. "Unbound")
(defconstant unbound .unbound.))


(defvar *var-counter* 0)


(defstruct (var (:constructor _ ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))


(defun bound-p (var) (not (eq (var-binding var) unbound)))


(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((equal (deref x) (deref y)) t)
        ((var-p x) (set-binding! x y))
        ((var-p y) (set-binding! y x))
        ((and (typep x '(vector (not string)))
              (typep y '(vector (not string)))
              (= (length x) (length y)))
         (every #'unify! x y))
        ((and (consp x) (consp y))
         (and (unify! (rest x) (rest y)) ;rest first.
              (unify! (first x) (first y))))
        (t nil)))


#+nil
(defun set-binding! (var value)
  "Set var's binding to value.  Always succeeds (returns t)."
  (setf (var-binding var) value)
  t)

(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "{undef}~A" (var-name var))
      (format stream "#<logvar: ~S>" var)))


(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))


(defun set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)


(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) unbound)))


(defun prolog-compile (symbol &optional
                       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))


(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause)
                     (relation-arity (clause-head clause)))
            :test test))


(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))


(defun args (x)
  "The arguments of a relation"
  (if (atom x)
      nil
      (rest x)))


(defun make-parameters (arity)
  "Return the list (_arg1 _arg2 ... _arg-arity)"
  (loop for i from 1 to arity
        collect (new-symbol '_arg i)))


(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol+ symbol '/ arity))


(defun make-== (x y) `(tao:== ,x ,y))


(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))


(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (and (symbolp name)
       (get name 'prolog-compiler-macro)))


(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist .,body)))


#+nil
(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))


(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))


(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings! old-trail)
                  collect exp)))))


(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar #'(lambda (var) `(,var (_)))
                       exp-vars)
           ,exp)
        exp)))


(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(make-anonymous clause)))


(defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '_)
        (t exp)))


(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))


(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond ((consp tree)
         (multiple-value-bind (new-seen-once new-seen-more)
                              (anon-vars-in (first tree) seen-once seen-more)
           (anon-vars-in (rest tree) new-seen-once new-seen-more)))
        ((typep tree '(vector (not string)))
         (anon-vars-in (coerce tree 'list) seen-once seen-more))
        ((not (variable-p tree)) (values seen-once seen-more))
        ((member tree seen-once)
         (values (delete tree seen-once) (cons tree seen-more)))
        ((member tree seen-more)
         (values seen-once seen-more))
        (t (values (cons tree seen-once) seen-more))))


(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
   ;; Unify constants and conses:                       ; Case
   ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
    (values (equal x y) bindings))
   ((and (consp x) (consp y))                           ; 3
    (multiple-value-bind (code1 bindings1)
                         (compile-unify (first x) (first y) bindings)
      (multiple-value-bind (code2 bindings2)
                           (compile-unify (rest x) (rest y) bindings1)
        (values (compile-if code1 code2) bindings2))))
   ;; Here x or y is a variable.  Pick the right one:
   ((variable-p x) (compile-unify-variable x y bindings))
   (t              (compile-unify-variable y x bindings))))


(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))


(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '_) (eq y '_)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an _arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings))))))


; 8,9

(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)


(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))


(defun compound-term-vector-p (obj)
  (and (typep obj 'vector)
       (< 0 (length obj))
       (typep (elt obj 0) '(and symbol (not null)))))


(deftype compound-term-vector ()
  `(satisfies compound-term-vector-p))


(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '_) '(_))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
               (compile-arg (binding-val binding) bindings)
               arg)))
        ((compound-term-vector-p arg)
         `(vector ,@(map 'list (lambda (a) (compile-arg a bindings))
                         arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list ,@(mapcar (lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))


(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
             do (setf ,exp (var-binding ,exp)))
          ,exp))


(defun compile-unquote-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (let ((deref-vars '()))
    (labels ((compile-unquote-arg (arg bindings)
               "Generate code for an argument to a goal in the body."
               (cond ((typep arg '(cons (eql tao:unquote) *))
                      (compile-unquote-arg (cadr arg) bindings))
                     ((eq arg '_) '(_))
                     ((variable-p arg)
                      (cl:push arg deref-vars)
                      (let ((binding (get-binding arg bindings)))
                        (if (and (not (null binding))
                                 (not (eq arg (binding-val binding))))
                            (compile-unquote-arg (binding-val binding) bindings)
                            arg)))
                     ((not (find-if-anywhere #'variable-p arg)) arg)
                     ((proper-listp arg)
                      (let ((expr (mapcar (lambda (a) (compile-unquote-arg a bindings))
                                          arg)))
                        (if (consp (car arg))
                            (cons 'list expr)
                            expr)))
                     (t `(cons ,(compile-unquote-arg (first arg) bindings)
                               ,(compile-unquote-arg (rest arg) bindings))))))
      (let* ((xpr (compile-unquote-arg arg bindings))
             (vars (remove-duplicates deref-vars :from-end T)))
        `(let (,@(mapcar (lambda (v) `(,v ,v)) vars))
           ,@(mapcar (lambda (v) `(setq ,v (deref-exp ,v))) vars)
           ,xpr)))))
 
(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))


(defun self-cons (x) (cons x x))


;;; todo
(def-prolog-compiler-macro tao:== (goal body cont bindings)
  (declare (ignore body cont bindings))
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        :pass
        #|(multiple-value-bind (code1 bindings1)
                             (compile-unify (first args)
                                            (second args)
                                            bindings)
          (compile-if code1
                      (compile-body body cont bindings1)))|#)))


(def-prolog-compiler-macro tao:& (goal body cont bindings)
  (if (null (cdr goal))
      (compile-body body cont bindings)
      (let* ((tail (cdr goal))
             (tail (if (typep (car tail) 'tao-internal::&aux-form)
                       (cdr tail)
                       tail)))
        (compile-body (append tail body) cont bindings))))


(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars parms                  
                     ;; fix broken compilation of (setof _x (or clause clause ..) _answer)
                     (if (member (car clause) '(if or and))
                         (compile-body (list clause)
                                       cont
                                       (mapcar #'self-cons parms))
                         (compile-body (nconc (mapcar #'make-== parms (args (clause-head clause)))
                                              (clause-body clause))
                                       cont
                                       (mapcar #'self-cons parms)))))


;***

(defvar *uncompiled* nil 
  "Prolog symbols that have not been compiled.")


(defun add-clause (clause &key asserta)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)                          ;***
    (setf (get pred 'clauses)
	  (if asserta
	      (nconc (list clause) (get-clauses pred))
	      (nconc (get-clauses pred) (list clause))))
    pred))


(defun retract-clause (clause)
  "Retract a clause from the data base"
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete clause (get-clauses pred) :test #'equal))
    pred))


(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '_ (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 (constantly nil))
  (format t "~&no")
  (values))


(defun top-level-prove-all (goals)
  "Prove the list of all goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '_ (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-all-prolog-vars ,(mapcar #'symbol-name vars) ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 (constantly T))
  'tao::that\'s-all)


(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (fill-pointer *trail*) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure cont)))


(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (with-compilation-unit ()
    (mapc #'prolog-compile symbols)
    (setf *uncompiled* (set-difference *uncompiled* symbols))))


(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
            (format t "~&~a = ~a" name (deref-exp var))))
  (finish-output)
  (if (and vars (continue-p))
      (funcall cont)
      (throw 'top-level-prove nil)))


(defun show-all-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
            (format t "~&~a = ~a" name (deref-exp var))))
  (finish-output)
  (funcall cont))


#|(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))|#


(defgeneric deref-exp (exp)
  (:documentation "Build something equivalent to EXP with variables dereferenced."))


(defmethod deref-exp (exp)
  (if (atom (deref exp))
      exp
      (reuse-cons (deref-exp (first exp))
                  (deref-exp (rest exp))
                  exp)))


(defmethod deref-exp ((exp string))
  exp)


(defmethod deref-exp ((exp vector))
  (map-into exp #'deref-exp exp))


(defvar *predicate* nil
  "The Prolog predicate currently being compiled")


(defmacro define-base-method (name (&rest args) &body body)
  `(progn
     (defgeneric ,name (,@args)
       (:method-combination tao:assert)
       (:method tao:assert (,@args) ,@body))
     ',name))


(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let* ((*predicate* (make-predicate symbol arity))    ;***
         (parameters (make-parameters arity))
         (pred-expr `(define-base-method ,*predicate* (,@parameters cont)
                       .,(maybe-add-undo-bindings
                          (mapcar #'(lambda (clause)
                                      (compile-clause parameters clause 'cont))
                                  clauses)))))
    (setf (get symbol arity) pred-expr)
    (let ((pred (eval pred-expr)))
      (unless (and (fboundp pred)
                   (compiled-function-p (fdefinition pred)))
        (compile pred))
      pred)))


(defun compile-local-predicate (symbol arity clauses aux-vars)
  "Compile all the clauses for a given symbol/arity
  into a single LISP local function."
  (declare (ignore aux-vars)) ;todo
  (let* ((*predicate* (make-predicate symbol arity))    ;***
         (parameters (make-parameters arity))
         (pred-expr `(flet ((,*predicate* (,@parameters cont)
                              .,(maybe-add-undo-bindings
                                 (mapcar (lambda (clause)
                                           (compile-clause parameters clause 'cont))
                                         clauses))))
                       #',*predicate*)))
    #+debug (pprint pred-expr *debug-io*)
    (funcall (compile nil `(lambda () ,pred-expr))) ;todo
    ))


(defun goal-cut-p (goal)
  (or (eq goal 'tao:!)
      (and (consp goal)
           (eq 'tao::&cut (car goal)))))


(defun goal-var-p (goal)
  (or (var-p goal)
      (and (symbolp goal)
           (eql 0 (position #\_ (string goal))))))


(defun goal-conjunction-p (goal)
  (goal-and-p goal))


(defun goal-and-p (goal)
  (and (consp goal)
       (eq (car goal) 'and)))


(defun goal-disjunction-p (goal)
  (and (goal-or-p goal)
       (not (goal-if-then-p (cadr goal)))))


(defun goal-or-p (goal)
  (and (consp goal)
       (eq (car goal) 'or)))


(defun goal-if-p (goal)
  (and (consp goal)
       (eq (car goal) 'if)))


(defun goal-if-then-p (goal)
  (and (goal-if-p goal)
       (null (cdddr goal))))


(defun goal-if-then-else-p (goal)
  (or
   ;; (OR (IF A B) C)
   (and (goal-or-p goal)
        (goal-if-then-p (cadr goal)))
   ;; (IF A B C)
   (and (goal-if-p goal)
        (not (null (cdddr goal)))
        (null (cddddr goal)))))


(defun destructure-if-then-else (goal)
  (cond
    ((goal-or-p goal)
     (destructuring-bind (or/2 (if/2 if then) else) goal
       (declare (cl:ignore or/2 if/2))
       (values if then else)))
    ((goal-if-p goal)
     (destructuring-bind (if/3 if then else) goal
       (declare (cl:ignore if/3))
       (values if then else)))
    (t (error "Goal not an IF-THEN-ELSE: ~S" goal))))


(defun find-unquote-expr (expr)
  (typecase expr
    (atom nil)
    ((cons (eql tao:unquote) list) T)
    (T (or (find-unquote-expr (car expr))
           (find-unquote-expr (cdr expr))))))


(defun goal-has-unquote-p (goal)
  (find-unquote-expr goal))


(deftype tao-package ()
  `(member ,(find-package "CL")
           ,(find-package "TAO")))


(defun nil->fail (body)
  (mapcar (lambda (x)
            (if (null x)
                '(fail)
                x))
          body))


(defun goal-tail-p (body)
  (null (cdr body)))


(defun goal-lisp-macro-p (goal)
  (and (symbolp goal)
       (macro-function goal)
       (get goal :logic-macro)))


(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (setq body (nil->fail body)) ;TODO
  (if (null body)
      `(funcall ,cont)
      (let ((goal (first body)))
	(cond ((goal-cut-p goal)
               `(progn
                  ,(compile-body (rest body) cont bindings)
                  (return-from ,*predicate* nil)))
              ((and (goal-tail-p body)
                    (goal-var-p goal))
               `(return-from ,*predicate* (deref-exp ,goal)))
              ((goal-var-p goal)
               `(and ,goal ,(compile-body (rest body) cont bindings)))
              ((goal-lisp-macro-p (predicate goal))
               (compile-body (append (list (macroexpand-1 goal)) (rest body)) cont bindings))
              ((goal-conjunction-p goal)
               (compile-body (append (cdr goal) (rest body)) cont bindings))
              ((goal-disjunction-p goal)
               (let ((bindings (bind-new-variables bindings goal)))
                 `(let ((old-trail (fill-pointer *trail*))
                        (cont (lambda () ,(compile-body
                                           (rest body)
                                           cont bindings))))
                    ,(compile-body (list (cadr goal)) 'cont bindings)
                    (undo-bindings! old-trail)
                    ,(compile-body (list (caddr goal)) 'cont bindings))))
              ((goal-if-then-p goal)
               (let ((bindings (bind-new-variables bindings goal)))
                 `(let ((cont (lambda () ,(compile-body
                                           (cons (caddr goal) (rest body))
                                           cont bindings))))
                    (block nil
                      ,(compile-body (list (cadr goal))
                                     '(lambda () 
                                        (funcall cont)
                                        (return nil))
                                     bindings)))))
              ((goal-if-then-else-p goal)
               (let ((bindings (bind-new-variables bindings goal)))
                 (multiple-value-bind (if then else)
                                      (destructure-if-then-else goal)
                   `(let ((old-trail (fill-pointer *trail*))
                          (cont (lambda ()
                                  ,(compile-body
                                    (rest body)
                                    cont bindings))))
                      (block nil
                        ,(compile-body (list if)
                                       `(lambda () 
                                          ,(compile-body (list then) cont bindings)
                                          (return nil))
                                       bindings)
                        (undo-bindings! old-trail)
                        ,(compile-body (list else) 'cont bindings))))))
              (t
               (let* ((macro (prolog-compiler-macro (predicate goal)))
                      (macro-val (if macro 
                                     (funcall macro goal (rest body) 
                                              cont bindings))))
                 (if (and macro (not (eq macro-val :pass)))
                     macro-val
                     (cond ((goal-has-unquote-p goal)
                            `(,(make-predicate (predicate goal)
                                               (relation-arity goal))
                              ,@(mapcar (lambda (x)
                                          (if (find-unquote-expr x)
                                              (compile-unquote-arg x bindings)
                                              (compile-arg x bindings)))
                                        (args goal))
                              ,(if (null (rest body))
                                   cont
                                   `(lambda ()
                                      ,(compile-body 
                                        (rest body) cont
                                        (bind-new-variables bindings goal))))))
                           ((and (symbolp (car goal))
                                 (not (get-clauses (car goal)))
                                 (or (fboundp (car goal)) (member (car goal) '(call-method call-next-method)))
                                 ;;(typep (symbol-package (car goal)) 'tao-package)
                                 )
                            `(lispp-uq/1
                              ,(compile-unquote-arg goal bindings)
                              (lambda ()
                                ,(compile-body 
                                  (rest body) cont
                                  (bind-new-variables bindings goal)))))
                           (T (if (and (tao:negation-as-failure)
                                       (not (fboundp (make-predicate (predicate goal) (relation-arity goal)))))
                                  `(fail/0 ,(if (null (rest body))
                                                cont
                                                `(lambda ()
                                                   ,(compile-body (rest body)
                                                                  cont
                                                                  (bind-new-variables bindings goal)))))
                                  `(,(make-predicate (predicate goal)
                                                     (relation-arity goal))
                                    ,@(mapcar (lambda (arg)
                                                (compile-arg arg bindings))
                                              (args goal))
                                    ,(if (null (rest body))
                                         cont
                                         `(lambda ()
                                            ,(compile-body (rest body)
                                                           cont
                                                           (bind-new-variables bindings goal)))))))))))))))


(defun translate-&+ (expr)
  (destructuring-bind (lam clause &rest clauses)
                      expr
    (case lam
      ((tao:&+)
       `(,(cons lam clause) ,@clauses))
      (otherwise expr))))


(defun make-anonymous-predicate-expr (arity clauses)
  (let ((parameters (make-parameters arity)))
    `(lambda (,@parameters cont)
       ,@(maybe-add-undo-bindings
          (mapcar (lambda (clause)
                    (typecase clause
                      ((cons (member tao:&+ tao:&+dyn) (cons * (cons (cons (eql tao:&aux)))))
                       (destructuring-bind (lam pat aux &body body)
                                           clause
                         `(tao:let (,@(cdr aux))
                            ,(compile-clause parameters `(,(cons lam pat) ,@body) 'cont))))
                      ((cons (eql tao:&+) *)
                       (compile-clause parameters (translate-&+ clause) 'cont))
                      (T (compile-clause parameters clause 'cont))))
                  clauses)))))


(defun compile-anonymous-predicate (arity clauses)
  (compile nil (make-anonymous-predicate-expr arity clauses))
  #+old
  (let ((parameters (make-parameters arity)))
    (compile nil
             (#+debug print
              #-debug progn
              `(lambda (,@parameters cont)
                 ,@(maybe-add-undo-bindings
                    (mapcar (lambda (clause)
                              (typecase clause
                                ((cons (eql tao:&+) (cons * (cons (cons (eql tao:&aux)))))
                                 (destructuring-bind (tao:&+ pat aux &body body)
                                                     clause
                                   `(tao:let (,@(cdr aux))
                                      ,(compile-clause parameters `(,(cons tao:&+ pat) ,@body) 'cont))))
                                ((cons (eql tao:&+) *)
                                 (compile-clause parameters (translate-&+ clause) 'cont))
                                (T (compile-clause parameters clause 'cont))))
                            clauses)))))))


(defun Hclauses->prolog-clauses (name clauses)
  (destructuring-bind (Hclauses &rest clauses)
                      clauses
    (declare (ignore Hclauses))
    (mapcar (lambda (c)
              (mapcar (lambda (c)
                        (cons name c))
                      (cdr c)))
            clauses)))


(defmacro define-logic (name form)
  (let* ((clauses (Hclauses->prolog-clauses name form))
         (functor/arity (make-predicate name (1- (length (caar clauses))))))
    `(progn
       (declaim (ftype function ,functor/arity))
       (setf (symbol-function ',functor/arity)
             ,form)
       (setf (get-clauses ',name) ',clauses)
       (tao-internal::define-predicate-in-lisp-world ,name))))


;;; *EOF*
