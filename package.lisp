(in-package #:cl-user)


(defpackage #:tao
  (:use)
  (:import-from :cl
   :declare :declaim
   :nil
   :&allow-other-keys :&aux :&body :&environment :&key :&optional :&rest :&whole
   :*break-on-signals*
   :*debug-io*
   :*default-pathname-defaults*
   :*error-output*
   :*features*
   :*load-verbose*
   :*macroexpand-hook*
   :*modules*
   :*package*
   :*print-array*
   :*print-base*
   :*compile-file-pathname*
   :*compile-file-truename* :*compile-print* :*compile-verbose* 
   :*debugger-hook* 
   :*gensym-counter* :*load-pathname* :*load-print* :*load-truename*
   :*print-case* :*print-circle* :*print-escape*
   :*print-gensym*
   :*print-length*
   :*print-level*
   :*print-pretty*
   :*print-radix*
   :*query-io*
   :*read-base*
   :*read-default-float-format*
   :*read-suppress*
   :*readtable*
   :*standard-input*
   :*standard-output*
   :*terminal-io*
   :*trace-output*)
  (:export #|types|#
   simple-vector
   fixnum)
  (:export #|0|#
   #:!
   #:&
   #:&+
   #:&+dyn
   #:&and
   #:&assert
   #:&cond
   #:&repeat
   #:&retract
   #:*
   #:**
   #:***
   #:*applyhook*
   #:*catch
   #:*evalhook*
   #:*file-search-path*
   #:*fn-notation*
   #:*logical-name-alist*
   #:*print-bigfloat-digit*
   #:*print-float-digit*
   #:*print-internal*
   #:*print-nilnum*
   #:*print-package*
   #:*print-shortfloat-digit*
   #:*print-string-marker*
   #:*print-total-chars*
   #:*throw
   #:*trace-level*
   #:*traced-fns*
   #:*untraced-fns*
   #:*user-packages*
   #:+
   #:++
   #:-
   #:--
   #:/
   #://
   #:/=
   #:1+
   #:1-
   #:64b-float
   #:64b-floatp
   #:64b-signed
   #:64b-signedp
   #:64b-unsigned
   #:64b-unsignedp
   #:64bfloc
   #:64bsiloc
   #:64builoc
   #:<
   #:<-
   #:<=
   #:=
   #:==
   #:>
   #:>=
   #:locbit
   #:signed-integer-locatives
   #:signed-integer-locatives
   #:\\\\
   #:\\\\\\\\
   #:_
   #:\|..\|
   #:\|.\|
   #:~)
  (:export #|c|#
   #:caaaar
   #:caaadr
   #:caaar
   #:caadar
   #:caaddr
   #:caadr
   #:caar
   #:cadaar
   #:cadadr
   #:cadar
   #:cadblep
   #:caddar
   #:cadddr
   #:caddr
   #:cadr
   #:call-arguments-limit
   #:car
   #:case
   #:caseq
   #:catch
   #:catcher
   #:catcher-case
   #:caught-value
   #:ccase
   #:cdaaar
   #:cdaadr
   #:cdaar
   #:cdadar
   #:cdaddr
   #:cdadr
   #:cdar
   #:cddaar
   #:cddadr
   #:cddar
   #:cdddar
   #:cddddr
   #:cdddr
   #:cddr
   #:cdr
   #:cdr!
   #:ceiling
   #:cell
   #:cellp
   #:cerror
   #:change-font
   #:char
   #:char-bit
   #:char-bits
   #:char-bits-limit
   #:char-code
   #:char-code-limit
   #:char-code-p
   #:char-control-bit
   #:char-downcase
   #:char-equal
   #:char-font
   #:char-font-limit
   #:char-greaterp
   #:char-hyper-bit
   #:char-int
   #:char-lessp
   #:char-meta-bit
   #:char-name
   #:char-not-equal
   #:char-not-greaterp
   #:char-not-lessp
   #:char-super-bit
   #:char-to-strh
   #:char-upcase
   #:char/=
   #:char<
   #:char<=
   #:char=
   #:char>
   #:char>=
   #:character
   #:characterp
   #:charp
   #:check-type
   #:circular-list
   #:cis
   #:cit101e-terminal
   #:cit600-terminal
   #:class-name-of
   #:class-of
   #:class-variable
   #:clause
   #:clear-input
   #:clear-memblk
   #:clear-output
   #:close
   #:closure
   #:closurep
   #:clrhash
   #:code-char
   #:codnum
   #:codnump
   #:coerce
   #:comment
   #:commentp
   #:commonp
   #:compile
   #:compile-file
   #:compiled-function-p
   #:compiler-let
   #:complex
   #:complexp
   #:concatenate
   #:concatenated-stream
   #:cond
   #:conjugate
   #:connect-dir
   #:cons
   #:cons!
   #:consp
   #:constantp
   #:convert-to-jstring
   #:copy
   #:copy-alist
   #:copy-file
   #:copy-from-floppy
   #:copy-list
   #:copy-memblk
   #:copy-readtable
   #:copy-seq
   #:copy-symbol
   #:copy-tree
   #:cos
   #:cosh
   #:count
   #:count-if
   #:count-if-not
   #:create-dir
   #:critical-section
   #:crlf
   #:ctypecase
   #:current-dir
   #:current-package
   #:cvar
   #:cycle)
  (:export #|d|#
   #:dashift
   #:day-of-week-string
   #:dbit-off
   #:dbit-on
   #:dbp
   #:dcu-terminal
   #:de
   #:debug
   #:dec
   #:decf
   #:decnum
   #:decode-float
   #:decode-universal-time
   #:defclass
   #:defclass-method
   #:defconstant
   #:defglobal
   #:define
   #:define-modify-macro
   #:define-setf-method
   #:definition
   #:deflogic-method
   #:defmacro
   #:defmethod
   #:defparameter
   #:defprop
   #:defrel
   #:defsetf
   #:defstruct
   #:deftype
   #:defun
   #:defvar
   #:del
   #:del-alist
   #:del-if
   #:del-if-not
   #:delete
   #:delete-dir
   #:delete-duplicates
   #:delete-file
   #:delete-if
   #:delete-if-not
   #:delq
   #:delql
   #:delqu
   #:denominator
   #:deposit-field
   #:deref
   #:describe
   #:describe
   #:describe-operations
   #:dev-dir-namestring
   #:diff
   #:digit-char
   #:digit-char-p
   #:directory
   #:directory-namestring
   #:dired
   #:disassemble
   #:dlogand
   #:dlogior
   #:dlognot
   #:dlogxor
   #:dlsh
   #:dnil
   #:do
   #:do*
   #:do*-named
   #:do-all-symbols
   #:do-external-symbols
   #:do-forever
   #:do-named
   #:do-symbols
   #:documentation
   #:dolist
   #:dotimes
   #:double-float-epsilon
   #:double-float-negative-epsilon
   #:dribble
   #:dsys
   #:dumb
   #:dumb-terminal
   #:dump-to-floppy
   #:dye)
  (:export #|a|#
   #:abolish
   #:abort
   #:abs
   #:acons
   #:acos
   #:acosh
   #:addprop
   #:adjoin
   #:adjust-array
   #:adjustable-array-p
   #:advise
   #:all-deleted-files
   #:all-directories
   #:all-files
   #:all-processes
   #:alpha-char-p
   #:alphanumericp
   #:and
   #:and\#
   #:ansi$backspace
   #:ansi$bell
   #:ansi$caution
   #:ansi$clear-all-tab-stop
   #:ansi$clear-tab-stop
   #:ansi$cr
   #:ansi$crlf
   #:ansi$cursor-down
   #:ansi$cursor-home
   #:ansi$cursor-left
   #:ansi$cursor-position
   #:ansi$cursor-position-p
   #:ansi$cursor-right
   #:ansi$cursor-up
   #:ansi$enter-keypad-application-mode
   #:ansi$enter-keypad-numeric-mode
   #:ansi$erase-entire-line
   #:ansi$erase-entire-screen
   #:ansi$erase-from-tol
   #:ansi$erase-from-tos
   #:ansi$erase-to-eol
   #:ansi$erase-to-eos
   #:ansi$file-out
   #:ansi$form-feed
   #:ansi$get-character
   #:ansi$get-line
   #:ansi$index
   #:ansi$init
   #:ansi$lf
   #:ansi$new-line
   #:ansi$reset-cursor-application-mode
   #:ansi$reset-to-normal-screen-mode
   #:ansi$restore-cursor
   #:ansi$reverse-index
   #:ansi$rubout
   #:ansi$save-cursor
   #:ansi$screen-height
   #:ansi$screen-width
   #:ansi$scroll-down
   #:ansi$scroll-up
   #:ansi$set-caution-type
   #:ansi$set-character-attribute
   #:ansi$set-cursor-application-mode
   #:ansi$set-cursor-attribute
   #:ansi$set-line-attribute
   #:ansi$set-scroll-region
   #:ansi$set-tab-stop
   #:ansi$set-to-reverse-screen-mode
   #:ansi$special-graphics
   #:ansi$standard-character
   #:ansi$tab
   #:ansi$vertical-tab
   #:append
   #:append!
   #:append2
   #:applobj
   #:applobj-of
   #:applobjp
   #:apply
   #:apply*
   #:applyhook
   #:apropos
   #:apropos-list
   #:aref
   #:array
   #:array-dimension
   #:array-dimension-limit
   #:array-dimensions
   #:array-element-type
   #:array-has-fill-pointer-p
   #:array-in-bounds-p
   #:array-info
   #:array-rank
   #:array-rank-limit
   #:array-row-major-index
   #:array-total-size
   #:array-total-size-limit
   #:array-type
   #:arrayp
   #:as-char
   #:as-shortnum
   #:ash
   #:ashift
   #:ashift
   #:asin
   #:asinh
   #:ass
   #:assert
   #:asserta
   #:assertz
   #:assign-cons
   #:assign-list
   #:assign-logical-name
   #:assignee-cons
   #:assigneep
   #:assignp
   #:assoc
   #:assoc-if
   #:assoc-if-not
   #:assq
   #:assql
   #:assqu
   #:atan
   #:atanh
   #:atom)
  (:export #|m|#
   #:machine-instance
   #:machine-type
   #:machine-version
   #:macro-function
   #:macroexpand
   #:macroexpand-1
   #:macrolet
   #:mailbox
   #:make-array
   #:make-broadcast-stream
   #:make-char
   #:make-concatenated-stream
   #:make-dispatch-macro-character
   #:make-echo-stream
   #:make-fatstring
   #:make-hash-table
   #:make-instance
   #:make-kanji-char
   #:make-list
   #:make-mutant
   #:make-package
   #:make-pathname
   #:make-process
   #:make-random-state
   #:make-row-buf
   #:make-sequence
   #:make-string
   #:make-string-input-stream
   #:make-string-output-stream
   #:make-string-with-fill-pointer
   #:make-symbol
   #:make-synonym-stream
   #:make-two-way-stream
   #:makunbound
   #:map
   #:mapatoms
   #:mapc
   #:mapcan
   #:mapcar
   #:mapcatoms
   #:mapcon
   #:maphash
   #:mapl
   #:maplist
   #:mask\#
   #:mask-field
   #:max
   #:max2
   #:memass
   #:member
   #:member-if
   #:member-if-not
   #:memblk-size
   #:memblkp
   #:memory-capacity
   #:merge
   #:merge-pathnames
   #:mig
   #:min
   #:min2
   #:minus
   #:minusp
   #:mismatch
   #:mod
   #:mod\#
   #:month-string
   #:more
   #:most-negative-double-float
   #:most-negative-fixnum
   #:most-negative-long-float
   #:most-negative-short-float
   #:most-negative-single-float
   #:most-positive-double-float
   #:most-positive-fixnum
   #:most-positive-long-float
   #:most-positive-short-float
   #:most-positive-single-float
   #:multiple-value-bind
   #:multiple-value-call
   #:multiple-value-list
   #:multiple-value-prog1
   #:multiple-value-setq
   #:multiple-values-limit)
  (:export #|p|#
   #:p-sem
   #:p-sem-with-timeout
   #:package-name
   #:package-nicknames
   #:package-shadowing-symbols
   #:package-use-list
   #:package-used-by-list
   #:packagep
   #:pairlis
   #:parent-package
   #:parent-package-chain
   #:parse-integer
   #:parse-namestring
   #:parse-universal-time
   #:pathname
   #:pathname-device
   #:pathname-directory
   #:pathname-host
   #:pathname-name
   #:pathname-type
   #:pathname-version
   #:pathnamep
   #:pc98k-terminal
   #:peek-char
   #:peelinv
   #:phase
   #:pi
   #:pipe-stream
   #:plist
   #:plus
   #:plusp
   #:pname
   #:pname-of-time
   #:pop
   #:position
   #:position-if
   #:position-if-not
   #:pprint
   #:prin1
   #:prin1-to-string
   #:princ
   #:princ-to-string
   #:prins
   #:print
   #:print-methods-of-class
   #:print-time
   #:probe-file
   #:process
   #:process-allow-schedule
   #:process-fork
   #:process-interrupt
   #:process-kill
   #:process-peep
   #:process-preset
   #:process-reset
   #:proclaim
   #:prog
   #:prog1
   #:prog2
   #:progi
   #:progn
   #:progv
   #:protect-file
   #:provide
   #:psetf
   #:psetq
   #:pure-jstring-p
   #:purge
   #:purge-spies
   #:purge-spy
   #:push
   #:pushnew
   #:put-alist
   #:put-comma
   #:put-toga
   #:putplist
   #:putprop)
  (:export #|l|#
   #:labels
   #:lambda
   #:lambda-list-keywords
   #:lambda-parameters-limit
   #:lappend
   #:last
   #:lcm
   #:ldb
   #:ldb-test
   #:ldiff
   #:leap-year-p
   #:least-negative-double-float
   #:least-negative-long-float
   #:least-negative-short-float
   #:least-negative-single-float
   #:least-positive-double-float
   #:least-positive-long-float
   #:least-positive-short-float
   #:least-positive-single-float
   #:length
   #:lessp
   #:let
   #:let*
   #:lins
   #:lisp-implementation-type
   #:lisp-implementation-version
   #:list
   #:list*
   #:list-all-global-packages
   #:list-all-packages
   #:list-length
   #:list-stream
   #:listen
   #:listing
   #:listp
   #:listq
   #:load
   #:load-factor-min
   #:load-factor-sec
   #:load-if-non-existent
   #:loc-diff
   #:loc-equate
   #:loc-greaterp
   #:loc-lessp
   #:loc-memblk
   #:loc-offset
   #:loc-size
   #:loc-type
   #:local-echo
   #:locally
   #:locativep
   #:locbit
   #:locbitp
   #:log
   #:logand
   #:logandc1
   #:logandc2
   #:logbitp
   #:logcount
   #:logeqv
   #:logic
   #:logical-names
   #:logicp
   #:login
   #:logior
   #:logmask
   #:lognand
   #:lognand*
   #:lognor
   #:lognor*
   #:lognot
   #:logorc1
   #:logorc2
   #:logout
   #:logtest
   #:logxor
   #:long-float-epsilon
   #:long-float-negative-epsilon
   #:long-site-name
   #:loop
   #:lower-case-p
   #:lsh)
  (:export #|n|#
   #:nambra-cons
   #:nambra-list
   #:nambracketp
   #:namcell-cons
   #:namcell-list
   #:namcellp
   #:name-char
   #:named-for
   #:namep
   #:namestring
   #:nand\#
   #:nbutlast
   #:ncadblep
   #:nconc
   #:nconc!
   #:ncons
   #:neg
   #:neq
   #:nidp
   #:nil
   #:nintersection
   #:ninth
   #:nleft
   #:nlistp
   #:no-dumb
   #:no-local-echo
   #:no-more
   #:no-screen
   #:no-wrap
   #:nor\#
   #:normal-stream-p
   #:not
   #:notany
   #:notevery
   #:nreconc
   #:nreverse
   #:nset-difference
   #:nset-exclusive-or
   #:nstring-capitalize
   #:nstring-downcase
   #:nstring-fill
   #:nstring-left-trim
   #:nstring-right-trim
   #:nstring-trim
   #:nstring-upcase
   #:nsublis
   #:nsublisq
   #:nsubst
   #:nsubst-if
   #:nsubst-if-not
   #:nsubstitute
   #:nsubstitute-if
   #:nsubstitute-if-not
   #:nsubstqu
   #:nsubstring
   #:nsymbolp
   #:nth
   #:nthcdr
   #:nthm
   #:nthv
   #:ntrim
   #:null
   #:null-stream
   #:null-string
   #:numberp
   #:numerator
   #:nunion
   #:nxor\#)
  (:export #|o|#
   #:octal-numberp
   #:octnum
   #:oddp
   #:off\#
   #:on\#
   #:open
   #:operation-handle-p
   #:or
   #:or\#
   #:output-stream-p
   #:own-file)
  (:export #|f|#
   #:fatjstringp
   #:fatstringp
   #:fboundp
   #:fceiling
   #:ffloor
   #:fgrep
   #:fifth
   #:file-author
   #:file-conc
   #:file-length
   #:file-namestring
   #:file-position
   #:file-stream
   #:file-write-date
   #:fill
   #:fill-pointer
   #:filstring
   #:filter-copy
   #:find
   #:find-all-symbols
   #:find-if
   #:find-if-not
   #:find-package
   #:find-position-in-list
   #:find-position-in-list-equal
   #:find-symbol
   #:find-symbol-local
   #:finish-output
   #:first
   #:firstn
   #:fix-all-dir
   #:fix-dir
   #:fixp
   #:flatsize
   #:flet
   #:float
   #:float-digits
   #:float-locative-arrays
   #:float-locatives
   #:float-precision
   #:float-radix
   #:float-sign
   #:floatp
   #:floor
   #:fmakunbound
   #:for
   #:force-output
   #:format
   #:forn
   #:fourth
   #:fresh-line
   #:fround
   #:ftruncate
   #:funcall
   #:funcall-init
   #:function
   #:functionp
   #:fundamental
   #:fundamental-stream)
  (:export #|g|#
   #:gc
   #:gcd
   #:gcdr
   #:gensym
   #:gentemp
   #:get
   #:get-decoded-time
   #:get-dispatch-macro-character
   #:get-handler-for
   #:get-internal-real-time
   #:get-internal-run-time
   #:get-kanji-code
   #:get-macro-character
   #:get-memblk
   #:get-output-stream-string
   #:get-properties
   #:get-setf-method
   #:get-setf-method-multiple-value
   #:get-sysmode
   #:get-universal-time
   #:getcharn
   #:getf
   #:gethash
   #:getl
   #:global-package
   #:go
   #:goal
   #:goal-all
   #:goal-all-list
   #:graphic-char-p
   #:greaterp
   #:grep
   #:growlistp)
  (:export #|e|#
   #:ecase
   #:echo-stream
   #:ed
   #:eighth
   #:elapse-time
   #:elt
   #:encode-universal-time
   #:endp
   #:enough-namestring
   #:environment
   #:eq
   #:eql
   #:equal
   #:equalp
   #:error
   #:etypecase
   #:eval
   #:eval-in-upper-env
   #:eval-inside-yourself
   #:eval-when
   #:evalhook
   #:evalp
   #:evenp
   #:every
   #:exit
   #:exit-for
   #:exit-image
   #:exit-loop
   #:exit-progi
   #:exp
   #:exploden
   #:export
   #:expr
   #:expt
   #:expunge-files)
  (:export #|i|#
   #:i/=
   #:i<
   #:i<=
   #:i=
   #:i>
   #:i>=
   #:id
   #:identity
   #:idp
   #:if
   #:ifundef
   #:ignore
   #:image
   #:image-can
   #:imagen
   #:imagpart
   #:import
   #:in-package
   #:inc
   #:incf
   #:index
   #:input-stream-p
   #:inspect
   #:int-char
   #:integer
   #:integer-decode-float
   #:integer-length
   #:integerp
   #:intern
   #:intern-local
   #:intern-local*
   #:internal-time-units-per-second
   #:interprocess-closure
   #:intersection
   #:intersectionq
   #:isqrt)
  (:export #|h|#
   #:hash
   #:hash-table-count
   #:hash-table-p
   #:hclauses
   #:hidar
   #:host-fullname
   #:host-name
   #:host-namestring)
  (:export #|k|# #:keywordp)
  (:export #|j|# #:jcharp #:jstringp)
  (:export #|q|#
   #:qd #:qdt #:quit-spy #:quote #:quotedp #:quotient #:quotify)
  (:export #|r|#
   #:random
   #:random-state
   #:random-state-p
   #:rassoc
   #:rassoc-if
   #:rassoc-if-not
   #:ratio
   #:rational
   #:rationalize
   #:rationalp
   #:read-byte
   #:read-char
   #:read-char-no-hang
   #:read-delimited-list
   #:read-from-string
   #:read-preserving-whitespace
   #:readtablep
   #:real
   #:realp
   #:realpart
   #:reduce
   #:remf
   #:remhash
   #:remove
   #:remove-duplicates
   #:remove-if
   #:remove-if-not
   #:remprop
   #:rename-file
   #:rename-package
   #:replace
   #:require
   #:rest
   #:return
   #:return-from
   #:revappend
   #:reverse
   #:room
   #:rotatef
   #:round
   #:rplaca
   #:rplacd)
  (:export #|s|#
   #:standard-char-p
   #:sbit
   #:scale-float
   #:set-dispatch-macro-character
   #:set-exclusive-or
   #:set-macro-character
   #:set-syntax-from-char
   :set-pprint-dispatch :setf :setq :seventh :shadow
   :shadowing-import :shared-initialize :shiftf :short-float :short-float-epsilon
   :short-float-negative-epsilon :short-site-name :signal :signed-byte :signum
   :simple-array :simple-base-string :simple-bit-vector :simple-bit-vector-p
   :simple-condition :simple-condition-format-arguments
   :simple-condition-format-control :simple-error :simple-string :simple-string-p
   :simple-type-error :simple-vector :simple-vector-p :simple-warning :sin
   :single-float :single-float-epsilon :single-float-negative-epsilon :sinh
   :sixth :sleep :slot-boundp :slot-exists-p :slot-makunbound :slot-missing
   :slot-unbound :slot-value :software-type :software-version :some :space
   :special :special-operator-p :speed :sqrt :stable-sort :standard
   :standard-char :standard-class :standard-generic-function
   :standard-method :standard-object :step :storage-condition :store-value
   :stream :stream-element-type :stream-error :stream-error-stream
   :stream-external-format :streamp :string-downcase
   :string-equal :string-greaterp :string-left-trim :string-lessp
   :string-not-equal :string-not-greaterp :string-not-lessp :string-right-trim
   :string-stream :string-upcase :string/= :string< :string<=
   :string= :string> :string>= :stringp :structure :structure-class
   :structure-object :style-warning :sublis :subseq :subst :subst-if
   :subst-if-not :substitute :substitute-if :substitute-if-not :subtypep :svref
   :sxhash :symbol :symbol-function :symbol-macrolet :symbol-name :symbol-package
   :symbol-plist :symbol-value :symbolp :synonym-stream :synonym-stream-symbol
   )
  (:export #|t|#
   #:t
   #:tailp
   #:tan
   #:tanh
   #:tenth
   #:terpri
   #:the
   #:third
   #:throw
   #:time
   #:trace
   #:tree-equal
   #:truename
   #:truncate
   #:two-way-stream
   #:type
   #:type-of
   #:typecase
   #:typep)
  (:export #|u|#
   #:user-homedir-pathname
   #:use-package
   #:upper-case-p
   #:unwind-protect
   #:unuse-package
   #:untrace
   #:unread-char
   #:unless
   #:unintern
   #:unexport
   #:union)
  (:export #|b|#
   #:backquotedp
   #:backquotify
   #:backtrace
   #:backtrace-stopper
   #:belongs-to
   #:bex-stream
   #:bigfloatp
   #:bigp
   #:bins
   #:bit
   #:bit-and
   #:bit-andc1
   #:bit-andc2
   #:bit-array-p
   #:bit-eqv
   #:bit-ior
   #:bit-nand
   #:bit-nor
   #:bit-not
   #:bit-off
   #:bit-on
   #:bit-orc1
   #:bit-orc2
   #:bit-test
   #:bit-xor
   #:blank
   #:blanks
   #:block
   #:boole
   #:both-case-p
   #:boundp
   #:bra-cons
   #:bra-list
   #:bracketp
   #:break
   #:broadcast
   #:broadcast-stream
   #:butlast
   #:byte
   #:byte-position
   #:byte-size)
  (:export #|v|#
   #:values
   values-list
   vector
   vector-pop
   vector-push
   vector-push-extend
   vectorp
   )
  
  (:export #|w|#
   #:write-to-string
   #:write-string
   #:write-line
   #:write-char
   #:write-byte
   #:warn
   #:with-output-to-string
   #:with-open-file
   #:with-open-stream
   #:with-input-from-string
   #:when
   #:warn)
  (:export #|y|#
   #:y-or-n-p
   #:yes-or-no-p)
  (:export :tao :common-lisp)
  ;; Common Lisp
  (:export
   :&allow-other-keys :&aux :&body :&environment :&key :&optional :&rest :&whole
   :* :** :*** :*break-on-signals* :*compile-file-pathname*
   :*compile-file-truename* :*compile-print* :*compile-verbose* :*debug-io*
   :*debugger-hook* :*default-pathname-defaults* :*error-output* :*features*
   :*gensym-counter* :*load-pathname* :*load-print* :*load-truename*
   :*load-verbose* :*macroexpand-hook* :*modules* :*package* :*print-array*
   :*print-base* :*print-case* :*print-circle* :*print-escape* :*print-gensym*
   :*print-length* :*print-level* :*print-lines* :*print-miser-width*
   :*print-pprint-dispatch* :*print-pretty* :*print-radix* :*print-readably*
   :*print-right-margin* :*query-io* :*random-state* :*read-base*
   :*read-default-float-format* :*read-eval* :*read-suppress* :*readtable*
   :*standard-input* :*standard-output* :*terminal-io* :*trace-output* :+ :++
   :+++ :- :/ :// :/// :/= :1+ :1- :< :<= := :> :>= :abort :abs :acons :acos
   :acosh :add-method :adjoin :adjust-array :adjustable-array-p
   :allocate-instance :alpha-char-p :alphanumericp :and :append :apply :apropos
   :apropos-list :aref :arithmetic-error :arithmetic-error-operands
   :arithmetic-error-operation :array :array-dimension :array-dimension-limit
   :array-dimensions :array-displacement :array-element-type
   :array-has-fill-pointer-p :array-in-bounds-p :array-rank :array-rank-limit
   :array-row-major-index :array-total-size :array-total-size-limit :arrayp :ash
   :asin :asinh :assert :assoc :assoc-if :assoc-if-not :atan :atanh :atom
   :caaaar :caaadr :caaar :caadar
   :caaddr :caadr :caar :cadaar :cadadr :cadar :caddar :cadddr :caddr :cadr
   :call-arguments-limit :call-method :call-next-method :car :case :catch :ccase
   :cdaaar :cdaadr :cdaar :cdadar :cdaddr :cdadr :cdar :cddaar :cddadr :cddar
   :cdddar :cddddr :cdddr :cddr :cdr :ceiling :cell-error :cell-error-name
   :cerror :change-class :char-code :char-code-limit :char-downcase
   :char-equal :char-greaterp :char-int :char-lessp :char-name :char-not-equal
   :char-not-greaterp :char-not-lessp :char-upcase :char/= :char< :char<= :char=
   :char> :char>= :character :characterp :check-type :cis :class :class-name
   :class-of :clear-input :clear-output :close :clrhash :code-char :coerce
   :compilation-speed :compile :compile-file :compile-file-pathname
   :compiled-function :compiled-function-p :compiler-macro
   :compiler-macro-function :complement :complex :complexp
   :compute-applicable-methods :compute-restarts :concatenate
   :concatenated-stream :concatenated-stream-streams :cond :condition :conjugate
   :cons :consp :constantly :constantp :continue :control-error :copy-alist
   :copy-list :copy-pprint-dispatch :copy-readtable :copy-seq :copy-structure
   :copy-symbol :copy-tree :cos :cosh :count :count-if :count-if-not :ctypecase
   :debug :decf :declaim :declaration :declare :decode-float
   :decode-universal-time :defclass :defconstant :defgeneric
   :define-compiler-macro :define-condition :define-method-combination
   :define-modify-macro :define-setf-expander :define-symbol-macro :defmacro
   :defmethod :defpackage :defparameter :defsetf :defstruct :deftype :defun
   :defvar :delete :delete-duplicates :delete-file :delete-if :delete-if-not
   :delete-package :denominator :deposit-field :describe :describe-object
   :destructuring-bind :digit-char :digit-char-p :directory :directory-namestring
   :disassemble :division-by-zero :do :do* :do-all-symbols :do-external-symbols
   :do-symbols :documentation :dolist :dotimes :double-float
   :double-float-epsilon :double-float-negative-epsilon :dpb :dribble
   :dynamic-extent :ecase :echo-stream :echo-stream-input-stream
   :echo-stream-output-stream :ed :eighth :elt :encode-universal-time
   :end-of-file :endp :enough-namestring :ensure-directories-exist
   :ensure-generic-function :eq :eql :equal :equalp :etypecase :eval
   :eval-when :evenp :every :exp :export :expt :extended-char :fboundp :fceiling
   :fdefinition :ffloor :fifth :file-author :file-error :file-error-pathname
   :file-length :file-namestring :file-position :file-stream :file-string-length
   :file-write-date :fill :fill-pointer :find :find-all-symbols :find-class
   :find-if :find-if-not :find-method :find-package :find-restart :find-symbol
   :finish-output :first :fixnum :flet :float :float-digits :float-precision
   :float-radix :float-sign :floating-point-inexact
   :floating-point-invalid-operation :floating-point-overflow
   :floating-point-underflow :floatp :floor :fmakunbound :force-output :format
   :formatter :fourth :fresh-line :fround :ftruncate :ftype :funcall :function
   :function-keywords :function-lambda-expression :functionp 
   :handler-bind :handler-case :hash-table :hash-table-count :hash-table-p
   :hash-table-rehash-size :hash-table-rehash-threshold :hash-table-size
   :hash-table-test :host-namestring
   :keyword
   :keywordp :labels :lambda :lambda-list-keywords :lambda-parameters-limit :last
   :lcm :ldb :ldb-test :ldiff :least-negative-double-float
   :least-negative-long-float :least-negative-normalized-double-float
   :least-negative-normalized-long-float :least-negative-normalized-short-float
   :least-negative-normalized-single-float :least-negative-short-float
   :least-negative-single-float :least-positive-double-float
   :least-positive-long-float :least-positive-normalized-double-float
   :least-positive-normalized-long-float :least-positive-normalized-short-float
   :least-positive-normalized-single-float :least-positive-short-float
   :least-positive-single-float :length :let :let* :lisp-implementation-type
   :lisp-implementation-version :list :list* :list-all-packages :list-length
   :listing :listen :listp :load :load-logical-pathname-translations :load-time-value
   :locally :log :logand :logandc1 :logandc2 :logbitp :logcount :logeqv
   :logical-pathname :logical-pathname-translations :logior :lognand :lognor
   :lognot :logorc1 :logorc2 :logtest :logxor :long-float :long-float-epsilon
   :long-float-negative-epsilon :long-site-name :loop :loop-finish :lower-case-p
   :machine-instance :machine-type :machine-version :macro-function :macroexpand
   :macroexpand-1 :macrolet :make-array :make-broadcast-stream
   :make-concatenated-stream :make-condition :make-dispatch-macro-character
   :make-echo-stream :make-hash-table :make-instance :make-instances-obsolete
   :make-list :make-load-form :make-load-form-saving-slots :make-method
   :make-package :make-pathname :make-random-state :make-sequence :make-string
   :make-string-input-stream :make-string-output-stream :make-symbol
   :make-synonym-stream :make-two-way-stream :makunbound :map :map-into :mapc
   :mapcan :mapcar :mapcon :maphash :mapl :maplist :mask-field :max :member
   :member-if :member-if-not :merge :merge-pathnames :method :method-combination
   :method-combination-error :method-qualifiers :min :minusp :mismatch :mod
   :most-negative-double-float :most-negative-fixnum :most-negative-long-float
   :most-negative-short-float :most-negative-single-float
   :most-positive-double-float :most-positive-fixnum :most-positive-long-float
   :most-positive-short-float :most-positive-single-float :muffle-warning
   :multiple-value-bind :multiple-value-call :multiple-value-list
   :multiple-value-prog1 :multiple-value-setq :multiple-values-limit :name-char
   :namestring :nbutlast :nconc :negation-as-failure :next-method-p :nil :nintersection :ninth
   :no-applicable-method :no-next-method :not :notany :notevery :notinline
   :nreconc :nreverse :nset-difference :nset-exclusive-or :nstring-capitalize
   :nstring-downcase :nstring-upcase :nsublis :nsubst :nsubst-if :nsubst-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not :nth :nth-value :nthcdr :null
   :number :numberp :numerator :nunion
   :package :package-error :package-error-package
   :package-name :package-nicknames :package-shadowing-symbols :package-use-list
   :package-used-by-list :packagep :pairlis :parse-error :parse-integer
   :parse-namestring :pathname :pathname-device :pathname-directory
   :pathname-host :pathname-match-p :pathname-name :pathname-type
   :pathname-version :pathnamep :peek-char :phase :pi :plusp :pop :position
   :position-if :position-if-not :pprint :pprint-dispatch
   :pprint-exit-if-list-exhausted :pprint-fill :pprint-indent :pprint-linear
   :pprint-logical-block :pprint-newline :pprint-pop :pprint-tab :pprint-tabular
   :prin1 :prin1-to-string :princ :princ-to-string :print :print-not-readable
   :print-not-readable-object :print-object :print-unreadable-object :probe-file
   :proclaim :prog :prog* :prog1 :prog2 :progn :program-error :progv :provide
   :psetf :psetq :push :pushnew :quote :random :random-state :random-state-p
   :rassoc :rassoc-if :rassoc-if-not :ratio :rational :rationalize :rationalp
   :read :read-byte :read-char :read-char-no-hang :read-delimited-list
   :read-from-string :read-line :read-preserving-whitespace :read-sequence
   :reader-error :readtable :readtable-case :readtablep :real :realp :realpart
   :reduce :reinitialize-instance :rem :remf :remhash :remove :remove-duplicates
   :remove-if :remove-if-not :remove-method :remprop :rename-file :rename-package
   :replace :require :rest :restart :restart-bind :restart-case :restart-name
   :return :return-from :revappend :reverse :room :rotatef :round :row-major-aref
   :rplaca :rplacd :safety :satisfies :sbit :scale-float :schar :search :second
   :sequence :serious-condition :set :set-difference
   :set-dispatch-macro-character :set-exclusive-or :set-macro-character
   :set-pprint-dispatch :set-syntax-from-char :setf :setq :seventh :shadow
   :shadowing-import :shared-initialize :shiftf :short-float :short-float-epsilon
   :short-float-negative-epsilon :short-site-name :signal :signed-byte :signum
   :simple-array :simple-base-string :simple-bit-vector :simple-bit-vector-p
   :simple-condition :simple-condition-format-arguments
   :simple-condition-format-control :simple-error :simple-string :simple-string-p
   :simple-type-error :simple-vector :simple-vector-p :simple-warning :sin
   :single-float :single-float-epsilon :single-float-negative-epsilon :sinh
   :sixth :sleep :slot-boundp :slot-exists-p :slot-makunbound :slot-missing
   :slot-unbound :slot-value :software-type :software-version :some :sort :space
   :special :special-operator-p :speed :sqrt :stable-sort :standard
   :standard-char :standard-char-p :standard-class :standard-generic-function
   :standard-method :standard-object :step :storage-condition :store-value
   :stream :stream-element-type :stream-error :stream-error-stream
   :stream-external-format :streamp :string :string-capitalize :string-downcase
   :string-equal :string-greaterp :string-left-trim :string-lessp
   :string-not-equal :string-not-greaterp :string-not-lessp :string-right-trim
   :string-stream :string-trim :string-upcase :string/= :string< :string<=
   :string= :string> :string>= :stringp :structure :structure-class
   :structure-object :style-warning :sublis :subseq :subsetp :subst :subst-if
   :subst-if-not :substitute :substitute-if :substitute-if-not :subtypep :svref
   :sxhash :symbol :symbol-function :symbol-macrolet :symbol-name :symbol-package
   :symbol-plist :symbol-value :symbolp :synonym-stream :synonym-stream-symbol :t
   :tagbody :tailp :tan :tanh :tenth :terpri :the :third :throw :time :trace
   :translate-logical-pathname :translate-pathname :tree-equal :truename
   :truncate :two-way-stream :two-way-stream-input-stream
   :two-way-stream-output-stream :type :type-error :type-error-datum
   :type-error-expected-type :type-of :typecase :typep :unbound-slot
   :unbound-slot-instance :unbound-variable :undefined-function :unexport
   :unintern :union :unless :unread-char :unsigned-byte :untrace :unuse-package
   :unwind-protect :update-instance-for-different-class
   :update-instance-for-redefined-class :upgraded-array-element-type
   :upgraded-complex-part-type :upper-case-p :use-package :use-value
   :user-homedir-pathname :values :values-list :variable :vector :vector-pop
   :vector-push :vector-push-extend :vectorp :warn :warning :when
   :wild-pathname-p :with-accessors :with-compilation-unit
   :with-condition-restarts :with-hash-table-iterator :with-input-from-string
   :with-open-file :with-open-stream :with-output-to-string
   :with-package-iterator :with-simple-restart :with-slots
   :with-standard-io-syntax :write :write-byte :write-char :write-line
   :write-sequence :write-string :write-to-string :y-or-n-p :yes-or-no-p :zerop)
  ;; TAO
  ;; lambda-list-keywords
  (:export :&optional :&optn :&opt :&rest :&key :&allow-other-keys 
   :&aux :&whole :&body :&environment)
  (:export
   :! :! :! :!! :& :& :&+ :&+dyn :&and :&assert :&cond :&cut :&repeat :&opt
   :&retract :*applyhook* :*break-on-warnings* :*catch :*evalhook*
   :*file-search-path* :*fn-notation* :*logical-name-alist*
   :*print-bigfloat-digit* :*print-float-digit* :*print-internal*
   :*print-nilnum* :*print-package* :*print-shortfloat-digit*
   :*print-string-marker* :*print-total-chars* :*throw :*trace-level*
   :*traced-fns* :*untraced-fns* :*user-packages* :-- :-- :|.| :|..| :64b-float
   :64b-floatp :64b-signed :64b-signedp :64b-unsigned :64b-unsignedp :64bfloc
   :64bsiloc :64builoc :<- :== :|\\| :|\\\\| :_ :~
   :*read-eof-value*)
  (:export
   :abolish :addprop :advise :all-deleted-files :all-directories
   :all-files :all-processes :and\# :ansi$backspace :ansi$bell :ansi$caution
   :ansi$clear-all-tab-stop :ansi$clear-tab-stop :ansi$cr :ansi$crlf
   :ansi$cursor-down :ansi$cursor-home :ansi$cursor-left :ansi$cursor-position
   :ansi$cursor-position-p :ansi$cursor-right :ansi$cursor-up
   :ansi$enter-keypad-application-mode :ansi$enter-keypad-numeric-mode
   :ansi$erase-entire-line :ansi$erase-entire-screen :ansi$erase-from-tol
   :ansi$erase-from-tos :ansi$erase-to-eol :ansi$erase-to-eos :ansi$file-out
   :ansi$form-feed :ansi$get-character :ansi$get-line :ansi$index :ansi$init
   :ansi$lf :ansi$new-line :ansi$reset-cursor-application-mode
   :ansi$reset-to-normal-screen-mode :ansi$restore-cursor :ansi$reverse-index
   :ansi$rubout :ansi$save-cursor :ansi$screen-height :ansi$screen-width
   :ansi$scroll-down :ansi$scroll-up :ansi$set-caution-type
   :ansi$set-character-attribute :ansi$set-cursor-application-mode
   :ansi$set-cursor-attribute :ansi$set-line-attribute :ansi$set-scroll-region
   :ansi$set-tab-stop :ansi$set-to-reverse-screen-mode :ansi$special-graphics
   :ansi$standard-character :ansi$tab :ansi$vertical-tab :append! :append2
   :applobj :applobj-of :applobjp :apply* :applyhook :array-info :array-type
   :as-char :as-shortnum :ashift :ashift :ass :asserta :assertz :assign-cons
   :assign-list :assign-logical-name :assignee-cons :assigneep :assignp :assq
   :assql :assqu)
  (:export
   :backquotedp :backquotify :backtrace :backtrace-stopper :belongs-to
   :bex-stream :bigfloatp :bigp :bins :bit-array-p :bit-off :bit-on :bit-test
   :blank :blanks :bra-cons :bra-list :bracketp :broadcast)
  (:export
   :cadblep :caseq :catcher :catcher-case :caught-value :cdr! :cell
   :char
   :cellp :change-font :char-bit :char-bits :char-bits-limit :char-code-p
   :char-control-bit :char-font :char-font-limit :char-hyper-bit :char-meta-bit
   :char-super-bit :char-to-strh :charp :circular-list :cit101e-terminal
   :cit600-terminal :class-name-of :class-variable :clause :clear-memblk
   :closure :closurep :codnum :codnump :comment :commentp :commonp :compiler-let
   :connect-dir :cons! :convert-to-jstring :copy :copy-file :copy-from-floppy
   :copy-memblk :create-dir :critical-section :crlf :current-dir
   :current-package :cvar :cycle)
  (:export
   :dashift :day-of-week-string :dbit-off :dbit-on :dbp :dcu-terminal
   :de :dec :decnum :defclass-method :defglobal :define :define-setf-method
   :definition :deflogic-macro :deflogic-method :defprop :defrel :del :del-alist :del-if
   :del-if-not :delete-dir :delq :delql :delqu :deref :describe-operations
   :dev-dir-namestring :diff :dired :dlogand :dlogior :dlognot :dlogxor :dlsh
   :dnil :do*-named :do-forever :do-named :dsys :dumb :dumb-terminal
   :dump-to-floppy :dye)
  (:export
   :elapse-time :environment :eval-in-upper-env :eval-inside-yourself
   :evalhook :evalp :exit :exit-for :exit-image :exit-loop :exit-progi :exploden
   :expr :expunge-files :error)
  (:export
   :fatjstringp :fatstringp :fgrep :file-conc :filstring :filter-copy
   :find-position-in-list :find-position-in-list-equal :find-symbol-local
   :firstn :fix-all-dir :fix-dir :fixp :flatsize :float-locative-arrays
   :float-locatives :for :forn :funcall-init :fundamental :fundamental-stream)
  (:export
   :gc :gcdr :get-handler-for :get-kanji-code :get-memblk
   :get-setf-method :get-setf-method-multiple-value :get-sysmode :getcharn :getl
   :global-package :goal :goal-all :goal-all-list :greaterp :grep :growlistp)
  (:export
   :hash :hclauses :hidar :host-fullname :host-name)
  (:export
   :i/= :i< :i<= :i= :i> :i>= :id :idp :ifundef :image :image-can
   :in-package
   :imagen :inc :index :int-char :intern-local :intern-local*
   :interprocess-closure :intersectionq)
  (:export
   :jcharp :jstringp)
  (:export
   :lappend :leap-year-p :lessp :lins :list-all-global-packages
   :list-stream :listq :load-factor-min :load-factor-sec :load-if-non-existent
   :loc-diff :loc-equate :loc-greaterp :loc-lessp :loc-memblk :loc-offset
   :loc-size :loc-type :local-echo :locativep :locbit :locbitp :logic
   :logical-names :logicp :login :logmask :lognand* :lognor* :logout :lsh)
  (:export
   :mailbox :make-char :make-fatstring :make-kanji-char :make-mutant
   :make-process :make-row-buf :make-string-with-fill-pointer :mapatoms
   :mapcatoms :mask\# :max2 :mem :memass :memblk-size :memblkp :memory-capacity
   :memq :memql :memqu :mig :min2 :minus :mod\# :month-string :more)
  (:export
   :nambra-cons :nambra-list :nambracketp :namcell-cons :namcell-list
   :namcellp :named-for :namep :nand\# :ncadblep :nconc! :ncons :neg :neq :nidp
   :nleft :nlistp :no-dumb :no-local-echo :no-more :no-screen :no-wrap :nor\#
   :normal-stream-p :nstring-fill :nstring-left-trim :nstring-right-trim
   :nstring-trim :nsublisq :nsubstqu :nsubstring :nsymbolp :nthm :nthv :ntrim
   :null-stream :null-string :nxor\#)
  (:export                              ;common:
   :nstring-fill :nstring-left-trim :nstring-right-trim :nstring-trim
   :simple-array-p :string-fill)
  (:export
   :octal-numberp :octnum :off\# :on\# :operation-handle-p :or\#
   :own-file)
  (:export
   :p-sem :p-sem-with-timeout :parent-package :parent-package-chain
   :parse-universal-time :pc98k-terminal :peelinv :pipe-stream :plist :plus
   :pname :pname-of-time :prins :print-methods-of-class :print-time :process
   :process-allow-schedule :process-fork :process-interrupt :process-kill
   :process-peep :process-preset :process-reset :progi :protect-file
   :pure-jstring-p :purge :purge-spies :purge-spy :put-alist :put-comma
   :put-toga :putplist :putprop)
  (:export
   :qd :qdt :quit-spy :quotedp :quotient :quotify)
  (:export
   :rass :rassq :rassql :rassqu :ratiop :readl :readline
   :readline-to-string :reads :receive-mail :receive-mail-with-timeout
   :recover-sstatus :register-global-package :rem-if :rem-if-not
   :remainder :remplist :remq :remql :remqu
   :rest1 :rest2 :rest3 :rest4 :reply :reset-io :retrace :retract :rtname)
  (:export
   :sass :sassq :sassql :sassqu :save-sstatus :sconc :screen :select
   :selector :selectq :selectq-every :self-eval-form-p :self
   :selfass :selfass-cons
   :selfass-list :selfassp :semaphore :semi-globals :send :send-class-message
   :send-mail :seq :seqt :sequal :sequencep :set-char-bit :set-date
   :set-default-keep-generation-count :set-differenceq :set-error-function
   :set-in-instance :set-job-name :set-keep-generation-count :set-loc-offset
   :set-priority :set-quantum :set-sysmode :set-terminal-type :sg-value :shead
   :shift\# :shortfloatp :shortnump :show-bit-vector :show-class
   :show-class-variables :show-terminal :show-vector
   :signed-integer-locative-arrays :signed-integer-locatives :singlefloatp
   :slength :slex :smemq :smemq-case :snull :sortcar :special-form-p
   :special-stream :special-stream-p :special-variables :spop :spy :sreverse
   :sstatus :stail :standard-read :standard-write :strh-to-char :string-append
   :string-byte-count :string-char-p :string-compare :string-compare-case
   :string-fill :string-fill-pointer :string-greater-or-equal
   :string-input-stream :string-length :string-less-or-equal
   :string-output-stream :string-replace :string-reverse :string-reverse-search
   :string-reverse-search-case :string-search :string-search-case :sublisq
   :sublisql
   :sublisq-copy :subpackages :subset :subset-not :substql
   :substqu :substring :super
   :symeval :systat)
  (:export
   :talk :tao-standard-readtable :tao-standard-sysmode :tconc :tcons
   :tdir :tek4404-terminal :terminal :terminal-mode :terminal-ports
   :terminal-stream :terminal-stream-p :terminal-type :throwablep :times :togap
   :trim :tyi :tyi-no-hang :tyi-with-timeout :tyib :tyo :tyo-with-timeout :tyob)
  (:export
   :udo :udo-equal :udop :ueq :undeclare-semi-globals :undef :undefclass
   :undeflogic-method :undefmethod :undefp :undelete-file :unionq :unit-clauses
   :unitern :unprotect-file :unquote :unsigned-integer-locatives
   :unsigned-integer-locative-arrays :untrail :untyi :unuse :use :user)
  (:export
   :v-sem :value :vanilla-class :vcons :vdir :vector-equal
   :vector-filler :vprint :vsize :vt100-terminal :vt200-terminal :vtitle)
  (:export
   :wait-until-timeout :which-operations :which-operations :who-spy
   :within-class :within-package :without-interrupts :wrap :write-string-justify
   :write-string-with-blank)
  (:export
   :xcons :xor\#)
  ;; codnum
  (:export 
   :expr :exprdyn :macro :subst :closure :array :&+ :hclauses :&+dyn :subr
   :expr-simple :exprdyn-simple :subr-simple :unit-clauses :1b-memblk :2b-memblk
   :4b-memblk :8b-memblk :16b-memblk :32b-memblk :64b-memblk)
  (:export :assert)
  (:export :&progn :query))


(defpackage tao-user
  (:use tao))


;(delete-package :common)
(defpackage common
  (:use)
  (:export
   :sort
   :stable-sort
   :string-capitalize
   :string-trim
   :/
   :/=
   :<
   :<=
   :=
   :>
   :>=
   :apply
   :arrayp
   :nreverse
   :prog
   :prog*
   :read-line
   :length
   :loop
   #:bit-vector-p
   #:boundp
   #:write-to-string
   #:write
   #:vectorp
   #:tagbody
   #:simple-array-p
   #:simple-bit-vector-p
   #:simple-string-p
   #:simple-vector-p
   #:string-downcase
   #:string-equal
   #:string-fill
   #:string-greaterp
   #:string-left-trim
   #:string-lessp
   #:string-not-equal
   #:string-not-greaterp
   #:string-not-lessp
   #:string-right-trim
   #:string-trim
   #:string-upcase
   #:string/=
   #:string<
   #:string<=
   #:string=
   #:string>
   #:string>=
   #:string>=
   #:stringp
   #:read
   #:read-from-string
   #:remprop
   #:reverse
   #:gcd
   #:funcall
   #:nstring-capitalize
   #:nstring-downcase
   #:nstring-fill
   #:nstring-left-trim
   #:nstring-right-trim
   #:nstring-trim
   #:nstring-upcase
   #:lambda
   #:length
   #:logand
   #:logior
   #:lognot
   #:logxor
   #:loop
   #:prog1
   #:prog2
   #:make-string
   #:/
   #:/=
   #:<
   #:<=
   #:=
   #:>
   #:>=
   ))


(defpackage #:tao-internal
  (:nicknames #:taoi)
  (:use #:cl))


(defpackage tao.sys 
  (:use)
  (:export
   :prestk-memblk :id-hash-memblk :64bloc-memblk :strhead-memblk :locbit-memblk
   :cell-memblk :vector-memblk :id-memblk :str-memblk :bad-memblk :free-memblk
   :kill-job :key-package
   :tagep :tao-package :v-sem
   :univ-package
   :error-in-error
   :set-tage
   :error-in-error
   :logout-him
   :lt
   :process-stop
   :export :gt
   :data-type
   :clear-tage
   :common-package
   :current-process
   )
  (:export #:quantum-remaining)
  (:export :key-package)
  (:export :bigfloat :float :de :dye :shortfloat))


(defpackage tao.logic 
  (:use cl)
  (:shadowing-import-from tao _))


(defpackage tao.ext
  (:use tao)
  (:shadowing-import-from tao _)
  (:export &arithmetic-function))


;;; *EOF*
