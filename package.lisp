(in-package #:cl-user)
;(delete-package :tao)
(defpackage #:tao
  (:use #:named-readtables)
  (:import-from
   :cl . #0=(:*
             :***
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
             :*print-case*
             :*print-circle*
             :*print-escape*
             :*print-gensym*
             :*print-length*
             :*print-level*
             :*print-pretty*
             :*print-radix*
             :*query-io*
             :*random-state*
             :*read-base*
             :*read-default-float-format*
             :*read-suppress*
             :*readtable*
             :*standard-input*
             :*standard-output*
             :*terminal-io*
             :*trace-output*
             :+
             :++
             :+++
             :-
             :1+
             :1-
             ))
  (:export . #0#)
  (:export
   :&opt
   :**
   :*read-eof-value*
   :length
   :listp
   :loop
   :make-string
   :package-name
   :read
   :rem
   :schar
   :sort
   :string-capitalize
   :zerop
   :apropos
   :apropos-list
   :aref
   :atom
   :atanh
   :atan
   :assoc-if-not
   :assoc-if
   :assoc
   :assignp
   :assigneep
   )
  (:export
   ;; 0
   :!
   :&
   :**
   :self                               ;!
   :selfass                            ;!!
   :togap                              ;^
   :*catch
   :*file-search-path*
   :*throw
   :/
   :/=
   :\\
   :<
   :<=
   :=
   :>
   :>=
   ://
   ;; a
   :abs
   :acons
   :acos
   :acosh
   :addprop
   :adjoin
   :applobjp
   :applobj-of
   :array
   :array-dimension
   :apply
   :apply*
   :as-char
   :ass
   :assign-cons
   :assignee-cons
   :assq
   :assql
   :assqu
   :arrayp
   ;; b
   #:belongs-to
   #:bins
   #:blank
   #:blanks
   ;; c
   :cadblep
   :caseq
   :catch-x
   :catcher
   :catcher-case
   :cdr!
   :circular-list
   :closure
   :comment
   :commentp
   :cons!
   :copy
   :crlf
   :find-throw
   :current-dir
   ;; d
   #:day-of-week-string
   #:de
   #:dec
   #:define
   #:do-forever
   :dye
   ;; e
   :exit-for
   :exit-loop
   :exit-progi
   :exploden
   :expr
   ;; f
   #:flatsize
   #:for
   #:forn
   ;; g
   #:gl->list
   #:gl-cons
   #:gl-list
   #:greaterp
   #:grep
   #:grep*
   #:print-gl
   ;; h
   #:hidar
   #:host-fullname
   ;; i
   #:ifundef
   #:image
   #:image-can
   #:imagen
   #:index
   #:intersectionq
   #:ignore
   ;; j
   ;; k
   ;; l
   #:leap-year-p
   #:length
   #:lessp
   #:lins
   #:listp
   #:listq
   #:load-if-non-existent
   #:loop
   ;; m
   #:make-string
   #:make-string-with-fill-pointer
   #:mapatoms
   #:max2
   ;; n
   #:named-for
   #:ncons
   #:neg
   #:neq
   #:nleft
   ;; o
   ;; p
   :package-name
   :plist
   :plus
   :progi
   :pname-of-time
   :put-toga
   :progn
   :progv
   :provide
   :psetf
   :psetq
   :push
   :pushnew
   :put-alist
   :putplist
   :putprop
   ;; q
   ;; r
   #:rass
   #:rassq
   #:rassql
   #:rassqu
   #:read
   #:readl
   #:readline
   #:readline-to-string
   #:reads
   #:rem
   #:rem-if
   #:rem-if-not
   #:remq
   #:remql
   #:remqu
   ;; s
   #:sass
   #:sassq
   #:sassql
   #:sassqu
   #:schar
   #:sconc
   #:select
   #:selector
   #:selectq
   #:selectq-every
   #:self-eval-form-p
   #:selfass-cons
   #:selfass-list
   #:seq
   #:seqt
   #:sequal
   #:sequencep
   #:set-differenceq
   #:shead
   #:slength
   #:slex
   #:smemq
   #:smemq*
   #:smemq-case
   #:snull
   #:sortcar
   #:spop
   #:sreverse
   #:stail
   :standard-read
   #:strh-to-char
   :string
   #:string-char-p
   #:string-greater-or-equal
   #:string-length
   #:string-replace
   #:string-less-or-equal
   #:sort
   #:string*-arg-check
   #:string-append
   #:string-capitalize
   #:string-compare
                                        ;   #:string-compare-*
   #:string-compare-case
   #:string-fill
   #:string-fill-pointer
   #:string-or-symbol->string
   #:string-reverse
   #:string-reverse-search
                                        ;   #:string-reverse-search-*
   #:string-reverse-search-case
   #:string-search
                                        ;   #:string-search-*
   #:string-search-case
   #:sublisq
   #:sublisq-copy
   #:subpackages
   #:subset
   :subsetp
   :second
   :search
   :selfassp
   :set
   :set-difference
   :string-trim
   #:subset-not
   #:substqu
   #:substring
   :symeval
   ;; t
   :togap
   :trans-progi-if-toga
   ;; u
   #:unionq
   #:unquote
   ;; v -
   :vprint
   ;; w
   ;; x
   #:xcons
   ;; y
   ;; z
   #:zerop
   ))

;(delete-package :common)
(defpackage common
  (:use)
  (:export :sort
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
           ))

(defpackage #:tao-internal
  (:use #:cl #:named-readtables))