(in-package #:cl-user)

(defpackage #:tao
  (:use #:named-readtables)
  (:export
   :**
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
   )
  (:export
   ;; 0
   #:!
   #:&
   #:**
   #:self                               ;!
   #:selfass                            ;!!
   #:togap                              ;^
   ;; a
   :apply
   :apply*
   :as-char
   :ass
   :assign-cons
   :assignee-cons
   :assq
   :assql
   :assqu
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
   #:exit-for
   #:exit-loop
   #:exit-progi
   #:exploden
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
   #:package-name
   #:plus
   #:progi
   #:pname-of-time
   #:put-toga
   #:trans-progi-if-toga
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
   #:strh-to-char
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
   #:subset-not
   #:substqu
   #:substring
   ;; t
   #:togap
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


(defpackage #:tao-internal
  (:use #:cl #:named-readtables))