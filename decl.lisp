(cl:in-package :cl-user)

(deftype tao::special-symbol ()
  "!                                      スペシャルシンボル

 <説明>
   カット記号と呼ばれ、バックトラックを制御する。1 つの ! が定理の宣言
 または U-resolver の本体 (関数 &+ 参照) にある場合、局所スコープで
 カット記号を含む残る選択肢としての定理があるならば、それらは無視される。
 つまりバックトラック中に、制御が、逆向きである右から左に、! を通って
 移ることはできない。

 <例>
 (assertz (p a1..) ... B2 ! B3 ...)
 (assertz (p a2..) ...)
 (assertz (p a3..) ...)
 とすると、B2 の評価に成功したあと ! を通って B3 の評価に制御が移ると、
 たとえ B3 の評価に失敗しても B3 から B2 へのバックトラックは起こらず、
 残る P 言明、(assertz (p a2..) ...) と (assertz (p a3..) ...) は、無視
 される。次の 2 つの言明は 「もし a > 10 が t なら B2 を実行し、そうでな
 ければ B3 を評価する」 ということを意味する。
 (assertz (p a) (a > 10) ! B2 )
 (assertz (p a) B3 )"
  `(member tao:!))


(deftype tao::cut-operator ()
  `(member tao:! (cons (eql tao:&cut) null)))


(defun tao-internal::symbol-list-p (list)
  (every #'symbolp list))


(deftype tao-internal::symbol-list ()
  `(and list
        (satisfies symbol-list-p)))

(deftype tao-internal::&aux-form ()
  `(cons (member &aux :aux) symbol-list))


;;; ------------------
;;; *
;;; ------------------
;;; (declaim (ftype (function (&rest number) (values number &optional)) *))
;;; ------------------

;;; ------------------
;;; **
;;; ------------------
(declaim (ftype (function (t t) (values number &optional)) tao:**))
;;; ------------------

;;; ------------------
;;; +
;;; ------------------
;;; (declaim (ftype (function (&rest number) (values number &optional)) +))
;;; ------------------

;;; ------------------
;;; -
;;; ------------------
;;; (declaim (ftype (function (number &rest number) (values number &optional)) -))
;;; ------------------

;;; ------------------
;;; /
;;; ------------------
(declaim (ftype (function (t t) (values number &optional integer)) tao:/))
;;; ------------------

;;; ------------------
;;; //
;;; ------------------
(declaim (ftype (function * (values t &optional t)) tao://))
;;; ------------------

;;; ------------------
;;; /=
;;; ------------------
(declaim (ftype (function (t t) (values (or number null (vector character) (vector nil) base-string) &optional)) tao:/=))
;;; ------------------

;;; ------------------
;;; 1+
;;; ------------------
;;; (declaim (ftype (function (number) (values number &optional)) 1+))
;;; ------------------

;;; ------------------
;;; 1-
;;; ------------------
;;; (declaim (ftype (function (number) (values number &optional)) 1-))
;;; ------------------

;;; ------------------
;;; <
;;; ------------------
(declaim (ftype (function (t t) (values (or single-float double-float rational null (vector character) (vector nil) base-string) &optional)) tao:<))
;;; ------------------

;;; ------------------
;;; <=
;;; ------------------
(declaim (ftype (function (t t) (values (or single-float double-float rational null (vector character) (vector nil) base-string) &optional)) tao:<=))
;;; ------------------

;;; ------------------
;;; =
;;; ------------------
(declaim (ftype (function (t t) (values (or number null (vector character) (vector nil) base-string) &optional)) tao:=))
;;; ------------------

;;; ------------------
;;; >
;;; ------------------
(declaim (ftype (function (t t) (values (or single-float double-float rational null (vector character) (vector nil) base-string) &optional)) tao:>))
;;; ------------------

;;; ------------------
;;; >=
;;; ------------------
(declaim (ftype (function (t t) (values (or single-float double-float rational null (vector character) (vector nil) base-string) &optional)) tao:>=))
;;; ------------------

;;; ------------------
;;; abort
;;; ------------------
;;; (declaim (ftype (function (&optional (or condition null)) nil) abort))
;;; ------------------

;;; ------------------
;;; abs
;;; ------------------
(declaim (ftype (function (t) (values (or (complex single-float) (complex double-float) single-float double-float rational) &optional)) tao:abs))
;;; ------------------

;;; ------------------
;;; acons
;;; ------------------
(declaim (ftype (function (t t t) (values cons &optional)) tao:acons))
;;; ------------------

;;; ------------------
;;; acos
;;; ------------------
(declaim (ftype (function (t) *) tao:acos))
;;; ------------------

;;; ------------------
;;; acosh
;;; ------------------
(declaim (ftype (function (t) *) tao:acosh))
;;; ------------------

;;; ------------------
;;; add-method
;;; ------------------
;;; (declaim (ftype (function (t t) *) add-method))
;;; ------------------

;;; ------------------
;;; addprop
;;; ------------------
(declaim (ftype (function (t t t) (values cons &optional)) tao:addprop))
;;; ------------------

;;; ------------------
;;; adjoin
;;; ------------------
(declaim (ftype (function (t list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values cons &optional)) tao:adjoin))
;;; ------------------

;;; ------------------
;;; adjust-array
;;; ------------------
;;; (declaim (ftype (function (array (or (mod 4611686018427387901) cons null) &key (:element-type (or cons symbol sb-kernel:instance)) (:initial-element t) (:initial-contents t) (:fill-pointer t) (:displaced-to (or array null)) (:displaced-index-offset (mod 4611686018427387901))) (values array &optional)) adjust-array))
;;; ------------------

;;; ------------------
;;; adjustable-array-p
;;; ------------------
;;; (declaim (ftype (function (array) (values (member t nil) &optional)) adjustable-array-p))
;;; ------------------

;;; ------------------
;;; allocate-instance
;;; ------------------
;;; (declaim (ftype (function (t &rest t) *) allocate-instance))
;;; ------------------

;;; ------------------
;;; alpha-char-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) alpha-char-p))
;;; ------------------

;;; ------------------
;;; alphanumericp
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) alphanumericp))
;;; ------------------

;;; ------------------
;;; append
;;; ------------------
;;; (declaim (ftype (function * (values t &optional)) append))
;;; ------------------

;;; ------------------
;;; applobj-of
;;; ------------------
(declaim (ftype (function (t) (values (or null function) &optional)) tao:applobj-of))
;;; ------------------

;;; ------------------
;;; applobjp
;;; ------------------
(declaim (ftype (function (t) (values (or null function) &optional)) tao:applobjp))
;;; ------------------

;;; ------------------
;;; apply
;;; ------------------
(declaim (ftype (function (t t) *) tao:apply))
;;; ------------------

;;; ------------------
;;; apply*
;;; ------------------
(declaim (ftype (function (t &rest t) *) tao:apply*))
;;; ------------------

;;; ------------------
;;; apropos
;;; ------------------
(declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) &optional (or (vector character) (vector nil) base-string character package symbol) t) (values &optional)) tao:apropos))
;;; ------------------

;;; ------------------
;;; apropos-list
;;; ------------------
(declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) &optional (or (vector character) (vector nil) base-string character package symbol) t) (values list &optional)) tao:apropos-list))
;;; ------------------

;;; ------------------
;;; aref
;;; ------------------
(declaim (ftype (function (array &rest t) (values t &optional)) tao:aref))
;;; ------------------

;;; ------------------
;;; arithmetic-error-operands
;;; ------------------
;;; (declaim (ftype (function (t) *) arithmetic-error-operands))
;;; ------------------

;;; ------------------
;;; arithmetic-error-operation
;;; ------------------
;;; (declaim (ftype (function (t) *) arithmetic-error-operation))
;;; ------------------

;;; ------------------
;;; array
;;; ------------------
(declaim (ftype (function * (values (simple-array t) &optional)) tao:array))
;;; ------------------

;;; ------------------
;;; array-dimension
;;; ------------------
(declaim (ftype (function (array (mod 65529)) (values (mod 4611686018427387901) &optional)) tao:array-dimension))
;;; ------------------

;;; ------------------
;;; array-dimensions
;;; ------------------
;;; (declaim (ftype (function (array) (values list &optional)) array-dimensions))
;;; ------------------

;;; ------------------
;;; array-displacement
;;; ------------------
;;; (declaim (ftype (function (array) (values (or (simple-array * (*)) null) (or null (mod 4611686018427387901)) &optional)) array-displacement))
;;; ------------------

;;; ------------------
;;; array-element-type
;;; ------------------
;;; (declaim (ftype (function (array) (values (or cons symbol sb-kernel:instance) &optional)) array-element-type))
;;; ------------------

;;; ------------------
;;; array-has-fill-pointer-p
;;; ------------------
;;; (declaim (ftype (function (array) (values (member t nil) &optional)) array-has-fill-pointer-p))
;;; ------------------

;;; ------------------
;;; array-in-bounds-p
;;; ------------------
;;; (declaim (ftype (function (array &rest integer) (values (member t nil) &optional)) array-in-bounds-p))
;;; ------------------

;;; ------------------
;;; array-rank
;;; ------------------
;;; (declaim (ftype (function (array) (values (mod 65529) &optional)) array-rank))
;;; ------------------

;;; ------------------
;;; array-row-major-index
;;; ------------------
;;; (declaim (ftype (function (array &rest (mod 4611686018427387901)) (values (mod 4611686018427387901) &optional)) array-row-major-index))
;;; ------------------

;;; ------------------
;;; array-total-size
;;; ------------------
;;; (declaim (ftype (function (array) (values (mod 4611686018427387901) &optional)) array-total-size))
;;; ------------------

;;; ------------------
;;; arrayp
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:arrayp))
;;; ------------------

;;; ------------------
;;; as-char
;;; ------------------
(declaim (ftype (function (t) (values character &optional)) tao:as-char))
;;; ------------------

;;; ------------------
;;; as-shortnum
;;; ------------------
(declaim (ftype (function (t) (values (unsigned-byte 64) &optional)) tao:as-shortnum))
;;; ------------------

;;; ------------------
;;; ash
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) ash))
;;; ------------------

;;; ------------------
;;; asin
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) asin))
;;; ------------------

;;; ------------------
;;; asinh
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) asinh))
;;; ------------------

;;; ------------------
;;; ass
;;; ------------------
(declaim (ftype (function (t t t) (values list &optional)) tao:ass))
;;; ------------------

;;; ------------------
;;; assignee-cons
;;; ------------------
(declaim (ftype (function (t) (values symbol &optional)) tao:assignee-cons))
;;; ------------------

;;; ------------------
;;; assigneep
;;; ------------------
(declaim (ftype (function (t) (values (member t nil) &optional)) tao:assigneep))
;;; ------------------

;;; ------------------
;;; assignp
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:assignp))
;;; ------------------

;;; ------------------
;;; assoc
;;; ------------------
(declaim (ftype (function (t list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) tao:assoc))
;;; ------------------

;;; ------------------
;;; assoc-if
;;; ------------------
(declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) tao:assoc-if))
;;; ------------------

;;; ------------------
;;; assoc-if-not
;;; ------------------
(declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) tao:assoc-if-not))
;;; ------------------

;;; ------------------
;;; assq
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:assq))
;;; ------------------

;;; ------------------
;;; assql
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:assql))
;;; ------------------

;;; ------------------
;;; assqu
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:assqu))
;;; ------------------

;;; ------------------
;;; atan
;;; ------------------
(declaim (ftype (function (t &optional t) *) tao:atan))
;;; ------------------

;;; ------------------
;;; atanh
;;; ------------------
(declaim (ftype (function (t) *) tao:atanh))
;;; ------------------

;;; ------------------
;;; atom
;;; ------------------
(declaim (ftype (function (t) (values (member t nil) &optional)) tao:atom))
;;; ------------------

;;; ------------------
;;; belongs-to
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:belongs-to))
;;; ------------------

;;; ------------------
;;; bins
;;; ------------------
(declaim (ftype (function (t t) (values t &optional)) tao:bins))
;;; ------------------

;;; ------------------
;;; bit
;;; ------------------
;;; (declaim (ftype (function ((array bit) &rest (mod 4611686018427387901)) (values bit &optional)) bit))
;;; ------------------

;;; ------------------
;;; bit-and
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-and))
;;; ------------------

;;; ------------------
;;; bit-andc1
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-andc1))
;;; ------------------

;;; ------------------
;;; bit-andc2
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-andc2))
;;; ------------------

;;; ------------------
;;; bit-eqv
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-eqv))
;;; ------------------

;;; ------------------
;;; bit-ior
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-ior))
;;; ------------------

;;; ------------------
;;; bit-nand
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-nand))
;;; ------------------

;;; ------------------
;;; bit-nor
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-nor))
;;; ------------------

;;; ------------------
;;; bit-not
;;; ------------------
;;; (declaim (ftype (function ((array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-not))
;;; ------------------

;;; ------------------
;;; bit-orc1
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-orc1))
;;; ------------------

;;; ------------------
;;; bit-orc2
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-orc2))
;;; ------------------

;;; ------------------
;;; bit-vector-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) bit-vector-p))
;;; ------------------

;;; ------------------
;;; bit-xor
;;; ------------------
;;; (declaim (ftype (function ((array bit) (array bit) &optional (or (array bit) (member t nil))) (values (array bit) &optional)) bit-xor))
;;; ------------------

;;; ------------------
;;; blank
;;; ------------------
(declaim (ftype (function (&optional t) (values (eql #\Space) &optional))
                tao:blank))
;;; ------------------

;;; ------------------
;;; blanks
;;; ------------------
(declaim (ftype (function (&optional t t) 
                          (values (member #\Space t nil) &optional))
                tao:blanks))
;;; ------------------

;;; ------------------
;;; boole
;;; ------------------
;;; (declaim (ftype (function ((unsigned-byte 4) integer integer) (values integer &optional)) boole))
;;; ------------------

;;; ------------------
;;; both-case-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) both-case-p))
;;; ------------------

;;; ------------------
;;; boundp
;;; ------------------
;;; (declaim (ftype (function (symbol) (values (member t nil) &optional)) boundp))
;;; ------------------

;;; ------------------
;;; break
;;; ------------------
;;; (declaim (ftype (function (&optional (or (vector character) (vector nil) base-string function) &rest t) (values null &optional)) break))
;;; ------------------

;;; ------------------
;;; broadcast-stream-streams
;;; ------------------
;;; (declaim (ftype (function (broadcast-stream) (values list &optional)) broadcast-stream-streams))
;;; ------------------

;;; ------------------
;;; butlast
;;; ------------------
;;; (declaim (ftype (function (list &optional unsigned-byte) (values list &optional)) butlast))
;;; ------------------

;;; ------------------
;;; byte
;;; ------------------
;;; (declaim (ftype (function ((unsigned-byte 62) (unsigned-byte 62)) (values cons &optional)) byte))
;;; ------------------

;;; ------------------
;;; byte-position
;;; ------------------
;;; (declaim (ftype (function (cons) (values (unsigned-byte 62) &optional)) byte-position))
;;; ------------------

;;; ------------------
;;; byte-size
;;; ------------------
;;; (declaim (ftype (function (cons) (values (unsigned-byte 62) &optional)) byte-size))
;;; ------------------

;;; ------------------
;;; caaaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caaaar))
;;; ------------------

;;; ------------------
;;; caaadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caaadr))
;;; ------------------

;;; ------------------
;;; caaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caaar))
;;; ------------------

;;; ------------------
;;; caadar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caadar))
;;; ------------------

;;; ------------------
;;; caaddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caaddr))
;;; ------------------

;;; ------------------
;;; caadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caadr))
;;; ------------------

;;; ------------------
;;; caar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caar))
;;; ------------------

;;; ------------------
;;; cadaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cadaar))
;;; ------------------

;;; ------------------
;;; cadadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cadadr))
;;; ------------------

;;; ------------------
;;; cadar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cadar))
;;; ------------------

;;; ------------------
;;; cadblep
;;; ------------------
(declaim (ftype (function (t) (values list &optional)) tao:cadblep))
;;; ------------------

;;; ------------------
;;; caddar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caddar))
;;; ------------------

;;; ------------------
;;; cadddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cadddr))
;;; ------------------

;;; ------------------
;;; caddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) caddr))
;;; ------------------

;;; ------------------
;;; cadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cadr))
;;; ------------------

;;; ------------------
;;; car
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) car))
;;; ------------------

;;; ------------------
;;; cdaaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdaaar))
;;; ------------------

;;; ------------------
;;; cdaadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdaadr))
;;; ------------------

;;; ------------------
;;; cdaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdaar))
;;; ------------------

;;; ------------------
;;; cdadar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdadar))
;;; ------------------

;;; ------------------
;;; cdaddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdaddr))
;;; ------------------

;;; ------------------
;;; cdadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdadr))
;;; ------------------

;;; ------------------
;;; cdar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdar))
;;; ------------------

;;; ------------------
;;; cddaar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cddaar))
;;; ------------------

;;; ------------------
;;; cddadr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cddadr))
;;; ------------------

;;; ------------------
;;; cddar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cddar))
;;; ------------------

;;; ------------------
;;; cdddar
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdddar))
;;; ------------------

;;; ------------------
;;; cddddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cddddr))
;;; ------------------

;;; ------------------
;;; cdddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdddr))
;;; ------------------

;;; ------------------
;;; cddr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cddr))
;;; ------------------

;;; ------------------
;;; cdr
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) cdr))
;;; ------------------

;;; ------------------
;;; ceiling
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values integer real &optional)) ceiling))
;;; ------------------

;;; ------------------
;;; cell-error-name
;;; ------------------
;;; (declaim (ftype (function (t) *) cell-error-name))
;;; ------------------

;;; ------------------
;;; cellp
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:cellp))
;;; ------------------

;;; ------------------
;;; cerror
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string function) t &rest t) (values null &optional)) cerror))
;;; ------------------

;;; ------------------
;;; change-class
;;; ------------------
;;; (declaim (ftype (function (t t &rest t &key &allow-other-keys) *) change-class))
;;; ------------------

;;; ------------------
;;; char
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array character (0)) character) &optional)) tao:char))
;;; ------------------

;;; ------------------
;;; char-code
;;; ------------------
;;; (declaim (ftype (function (character) (values (mod 1114112) &optional)) char-code))
;;; ------------------

;;; ------------------
;;; char-downcase
;;; ------------------
;;; (declaim (ftype (function (character) (values character &optional)) char-downcase))
;;; ------------------

;;; ------------------
;;; char-equal
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-equal))
;;; ------------------

;;; ------------------
;;; char-greaterp
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-greaterp))
;;; ------------------

;;; ------------------
;;; char-int
;;; ------------------
;;; (declaim (ftype (function (character) (values (mod 1114112) &optional)) char-int))
;;; ------------------

;;; ------------------
;;; char-lessp
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-lessp))
;;; ------------------

;;; ------------------
;;; char-name
;;; ------------------
;;; (declaim (ftype (function (character) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) char-name))
;;; ------------------

;;; ------------------
;;; char-not-equal
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-not-equal))
;;; ------------------

;;; ------------------
;;; char-not-greaterp
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-not-greaterp))
;;; ------------------

;;; ------------------
;;; char-not-lessp
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char-not-lessp))
;;; ------------------

;;; ------------------
;;; char-upcase
;;; ------------------
;;; (declaim (ftype (function (character) (values character &optional)) char-upcase))
;;; ------------------

;;; ------------------
;;; char/=
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char/=))
;;; ------------------

;;; ------------------
;;; char<
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char<))
;;; ------------------

;;; ------------------
;;; char<=
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char<=))
;;; ------------------

;;; ------------------
;;; char=
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char=))
;;; ------------------

;;; ------------------
;;; char>
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char>))
;;; ------------------

;;; ------------------
;;; char>=
;;; ------------------
;;; (declaim (ftype (function (character &rest character) (values (member t nil) &optional)) char>=))
;;; ------------------

;;; ------------------
;;; character
;;; ------------------
;;; (declaim (ftype (function (t) (values character &optional)) character))
;;; ------------------

;;; ------------------
;;; characterp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) characterp))
;;; ------------------

;;; ------------------
;;; circular-list
;;; ------------------
(declaim (ftype (function * (values list &optional)) tao:circular-list))
;;; ------------------

;;; ------------------
;;; cis
;;; ------------------
;;; (declaim (ftype (function (real) (values (or (complex single-float) (complex double-float)) &optional)) cis))
;;; ------------------

;;; ------------------
;;; class-name
;;; ------------------
;;; (declaim (ftype (function (class) (values symbol &optional)) class-name))
;;; ------------------

;;; ------------------
;;; class-of
;;; ------------------
;;; (declaim (ftype (function (t) (values class &optional)) class-of))
;;; ------------------

;;; ------------------
;;; clear-input
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values null &optional)) clear-input))
;;; ------------------

;;; ------------------
;;; clear-output
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values null &optional)) clear-output))
;;; ------------------

;;; ------------------
;;; close
;;; ------------------
;;; (declaim (ftype (function (t &key (:abort t)) *) close))
;;; ------------------

;;; ------------------
;;; clrhash
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values hash-table &optional)) clrhash))
;;; ------------------

;;; ------------------
;;; code-char
;;; ------------------
;;; (declaim (ftype (function ((mod 1114112)) (values character &optional)) code-char))
;;; ------------------

;;; ------------------
;;; coerce
;;; ------------------
;;; (declaim (ftype (function (t (or cons symbol sb-kernel:instance)) (values t &optional)) coerce))
;;; ------------------

;;; ------------------
;;; compile
;;; ------------------
;;; (declaim (ftype (function ((or symbol cons) &optional (or cons function null)) (values (or function symbol cons) (member . #0=(t nil)) (member . #0#) &optional)) compile))
;;; ------------------

;;; ------------------
;;; compile-file
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:output-file (or (vector character) (vector nil) base-string pathname file-stream (member t nil))) (:verbose t) (:print t) (:external-format (or keyword (cons keyword t))) (:trace-file t) (:block-compile t) (:emit-cfasl t)) (values (or pathname null) (member . #0=(t nil)) (member . #0#) &optional)) compile-file))
;;; ------------------

;;; ------------------
;;; compile-file-pathname
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:output-file (or (vector character) (vector nil) base-string pathname file-stream (member t nil))) &allow-other-keys) (values pathname &optional)) compile-file-pathname))
;;; ------------------

;;; ------------------
;;; compiled-function-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) compiled-function-p))
;;; ------------------

;;; ------------------
;;; compiler-macro-function
;;; ------------------
;;; (declaim (ftype (function (t &optional (or sb-kernel:lexenv null)) (values (or function null) &optional)) compiler-macro-function))
;;; ------------------

;;; ------------------
;;; complement
;;; ------------------
;;; (declaim (ftype (function (function) (values function &optional)) complement))
;;; ------------------

;;; ------------------
;;; complex
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values number &optional)) complex))
;;; ------------------

;;; ------------------
;;; complexp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) complexp))
;;; ------------------

;;; ------------------
;;; compute-applicable-methods
;;; ------------------
;;; (declaim (ftype (function (t t) *) compute-applicable-methods))
;;; ------------------

;;; ------------------
;;; compute-restarts
;;; ------------------
;;; (declaim (ftype (function (&optional (or condition null)) (values list &optional)) compute-restarts))
;;; ------------------

;;; ------------------
;;; concatenate
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) &rest sequence) (values (or (simple-array * (*)) cons null sequence) &optional)) concatenate))
;;; ------------------

;;; ------------------
;;; concatenated-stream-streams
;;; ------------------
;;; (declaim (ftype (function (concatenated-stream) (values list &optional)) concatenated-stream-streams))
;;; ------------------

;;; ------------------
;;; conjugate
;;; ------------------
;;; (declaim (ftype (function (number) (values number &optional)) conjugate))
;;; ------------------

;;; ------------------
;;; cons
;;; ------------------
;;; (declaim (ftype (function (t t) (values cons &optional)) cons))
;;; ------------------

;;; ------------------
;;; consp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) consp))
;;; ------------------

;;; ------------------
;;; constantly
;;; ------------------
;;; (declaim (ftype (function (t) (values function &optional)) constantly))
;;; ------------------

;;; ------------------
;;; constantp
;;; ------------------
;;; (declaim (ftype (function (t &optional (or sb-kernel:lexenv null)) (values (member t nil) &optional)) constantp))
;;; ------------------

;;; ------------------
;;; continue
;;; ------------------
;;; (declaim (ftype (function (&optional (or condition null)) (values null &optional)) continue))
;;; ------------------

;;; ------------------
;;; copy
;;; ------------------
(declaim (ftype (function (t) (values t &optional)) tao:copy))
;;; ------------------

;;; ------------------
;;; copy-alist
;;; ------------------
;;; (declaim (ftype (function (list) (values list &optional)) copy-alist))
;;; ------------------

;;; ------------------
;;; copy-list
;;; ------------------
;;; (declaim (ftype (function (list) (values list &optional)) copy-list))
;;; ------------------

;;; ------------------
;;; copy-pprint-dispatch
;;; ------------------
;;; (declaim (ftype (function (&optional (or sb-pretty:pprint-dispatch-table null)) (values sb-pretty:pprint-dispatch-table &optional)) copy-pprint-dispatch))
;;; ------------------

;;; ------------------
;;; copy-readtable
;;; ------------------
;;; (declaim (ftype (function (&optional (or readtable null) (or readtable null)) (values readtable &optional)) copy-readtable))
;;; ------------------

;;; ------------------
;;; copy-seq
;;; ------------------
;;; (declaim (ftype (function (sequence) (values (or (simple-array * (*)) cons null sequence) &optional)) copy-seq))
;;; ------------------

;;; ------------------
;;; copy-structure
;;; ------------------
;;; (declaim (ftype (function (structure-object) (values structure-object &optional)) copy-structure))
;;; ------------------

;;; ------------------
;;; copy-symbol
;;; ------------------
;;; (declaim (ftype (function (symbol &optional t) (values symbol &optional)) copy-symbol))
;;; ------------------

;;; ------------------
;;; copy-tree
;;; ------------------
;;; (declaim (ftype (function (t) (values t &optional)) copy-tree))
;;; ------------------

;;; ------------------
;;; cos
;;; ------------------
;;; (declaim (ftype (function (number) (values (or (single-float -1.0 1.0) (double-float -1.0d0 1.0d0) (complex single-float) (complex double-float)) &optional)) cos))
;;; ------------------

;;; ------------------
;;; cosh
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) cosh))
;;; ------------------

;;; ------------------
;;; count
;;; ------------------
;;; (declaim (ftype (function (t sequence &rest t &key (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:from-end t) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (mod 4611686018427387901) &optional)) count))
;;; ------------------

;;; ------------------
;;; count-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (mod 4611686018427387901) &optional)) count-if))
;;; ------------------

;;; ------------------
;;; count-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (mod 4611686018427387901) &optional)) count-if-not))
;;; ------------------

;;; ------------------
;;; crlf
;;; ------------------
(declaim (ftype (function (&optional t) (values (member t) &optional)) tao:crlf))
;;; ------------------

;;; ------------------
;;; current-dir
;;; ------------------
(declaim (ftype (function nil *) tao:current-dir))
;;; ------------------

;;; ------------------
;;; day-of-week-string
;;; ------------------
(declaim (ftype (function (t) (values (or null (simple-array character (3))) &optional)) tao:day-of-week-string))
;;; ------------------

;;; ------------------
;;; decnum
;;; ------------------
(declaim (ftype (function (t) (values t &optional)) tao:decnum))
;;; ------------------

;;; ------------------
;;; decode-float
;;; ------------------
(declaim (ftype (function (t) (values float (integer 2 2) float &optional)) tao:decode-float))
;;; ------------------

;;; ------------------
;;; decode-universal-time
;;; ------------------
(declaim (ftype (function (unsigned-byte &optional (or (rational -24 24) null)) (values (mod 60) (mod 60) (mod 24) (integer 1 31) (integer 1 12) unsigned-byte (mod 7) (member t nil) (rational -24 24) &optional)) tao:decode-universal-time))
;;; ------------------

;;; ------------------
;;; del
;;; ------------------
(declaim (ftype (function (t t list &optional t) (values list &optional)) tao:del))
;;; ------------------

;;; ------------------
;;; del-alist
;;; ------------------
(declaim (ftype (function (t t) (values sequence &optional)) tao:del-alist))
;;; ------------------

;;; ------------------
;;; del-if
;;; ------------------
(declaim (ftype function tao:del-if))
;;; ------------------

;;; ------------------
;;; del-if-not
;;; ------------------
(declaim (ftype function tao:del-if-not))
;;; ------------------

;;; ------------------
;;; delete
;;; ------------------
(declaim (ftype (function (t sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:end (or (mod 4611686018427387901) null)) (:count (or integer null)) (:key (or function symbol))) (values sequence &optional)) tao:delete))
;;; ------------------

;;; ------------------
;;; delete-duplicates
;;; ------------------
(declaim (ftype (function (sequence &rest t &key (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:end (or (mod 4611686018427387901) null)) (:from-end t) (:key (or function symbol))) (values sequence &optional)) tao:delete-duplicates))
;;; ------------------

;;; ------------------
;;; delete-file
;;; ------------------
(declaim (ftype (function ((or (vector character) (vector nil) base-string pathname stream)) (values (member t) &optional)) tao:delete-file))
;;; ------------------

;;; ------------------
;;; delete-if
;;; ------------------
(declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:key (or function symbol)) (:end (or (mod 4611686018427387901) null)) (:count (or integer null))) (values sequence &optional)) tao:delete-if))
;;; ------------------

;;; ------------------
;;; delete-if-not
;;; ------------------
(declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or (mod 4611686018427387901) null)) (:key (or function symbol)) (:count (or integer null))) (values sequence &optional)) tao:delete-if-not))
;;; ------------------

;;; ------------------
;;; delete-package
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) delete-package))
;;; ------------------

;;; ------------------
;;; delq
;;; ------------------
(declaim (ftype function tao:delq))
;;; ------------------

;;; ------------------
;;; delql
;;; ------------------
(declaim (ftype function tao:delql))
;;; ------------------

;;; ------------------
;;; delqu
;;; ------------------
(declaim (ftype function tao:delqu))
;;; ------------------

;;; ------------------
;;; denominator
;;; ------------------
(declaim (ftype (function (rational) (values integer &optional)) tao:denominator))
;;; ------------------

;;; ------------------
;;; deposit-field
;;; ------------------
;;; (declaim (ftype (function (integer cons integer) (values integer &optional)) deposit-field))
;;; ------------------

;;; ------------------
;;; describe
;;; ------------------
(declaim (ftype (function (t &optional t) (values &optional)) tao:describe))
;;; ------------------

;;; ------------------
;;; describe-object
;;; ------------------
;;; (declaim (ftype (function (t t) *) describe-object))
;;; ------------------

;;; ------------------
;;; digit-char
;;; ------------------
(declaim (ftype (function (unsigned-byte &optional (integer 2 36)) (values (or character null) &optional)) tao:digit-char))
;;; ------------------

;;; ------------------
;;; digit-char-p
;;; ------------------
(declaim (ftype (function (character &optional (integer 2 36)) (values (or (mod 36) null) &optional)) tao:digit-char-p))
;;; ------------------

;;; ------------------
;;; directory
;;; ------------------
(declaim (ftype (function ((or (vector character) (vector nil) base-string pathname stream) &key (:resolve-symlinks t)) (values list &optional)) tao:directory))
;;; ------------------

;;; ------------------
;;; directory-namestring
;;; ------------------
(declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) tao:directory-namestring))
;;; ------------------

;;; ------------------
;;; disassemble
;;; ------------------
(declaim (ftype (function ((or function symbol cons) &key (:stream stream) (:use-labels (member nil t))) (values null &optional)) tao:disassemble))
;;; ------------------

;;; ------------------
;;; documentation
;;; ------------------
(declaim (ftype (function (t t) *) tao:documentation))
;;; ------------------

;;; ------------------
;;; dpb
;;; ------------------
;;; (declaim (ftype (function (integer cons integer) (values integer &optional)) dpb))
;;; ------------------

;;; ------------------
;;; dribble
;;; ------------------
(declaim (ftype (function (&optional (or (vector character) (vector nil) base-string pathname null) &key (:if-exists t)) (values &optional)) tao:dribble))
;;; ------------------

;;; ------------------
;;; echo-stream-input-stream
;;; ------------------
;;; (declaim (ftype (function (echo-stream) (values stream &optional)) echo-stream-input-stream))
;;; ------------------

;;; ------------------
;;; echo-stream-output-stream
;;; ------------------
;;; (declaim (ftype (function (echo-stream) (values stream &optional)) echo-stream-output-stream))
;;; ------------------

;;; ------------------
;;; ed
;;; ------------------
;;; (declaim (ftype (function (&optional (or symbol cons (vector character) (vector nil) base-string pathname)) (values t &optional)) ed))
;;; ------------------

;;; ------------------
;;; eighth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) eighth))
;;; ------------------

;;; ------------------
;;; elt
;;; ------------------
;;; (declaim (ftype (function (sequence (mod 4611686018427387901)) (values t &optional)) elt))
;;; ------------------

;;; ------------------
;;; encode-universal-time
;;; ------------------
;;; (declaim (ftype (function ((mod 60) (mod 60) (mod 24) (integer 1 31) (integer 1 12) unsigned-byte &optional (or null (rational -24 24))) (values unsigned-byte &optional)) encode-universal-time))
;;; ------------------

;;; ------------------
;;; endp
;;; ------------------
;;; (declaim (ftype (function (list) (values (member t nil) &optional)) endp))
;;; ------------------

;;; ------------------
;;; enough-namestring
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &optional (or (vector character) (vector nil) base-string pathname file-stream)) (values simple-string &optional)) enough-namestring))
;;; ------------------

;;; ------------------
;;; ensure-directories-exist
;;; ------------------
;;; (declaim (ftype (function (t &key (:verbose t) (:mode t)) (values t (member t nil) &optional)) ensure-directories-exist))
;;; ------------------

;;; ------------------
;;; ensure-generic-function
;;; ------------------
;;; (declaim (ftype (function (t &rest t &key (:environment t) (:definition-source t) &allow-other-keys) *) ensure-generic-function))
;;; ------------------

;;; ------------------
;;; eq
;;; ------------------
;;; (declaim (ftype (function (t t) (values (member t nil) &optional)) eq))
;;; ------------------

;;; ------------------
;;; eql
;;; ------------------
;;; (declaim (ftype (function (t t) (values (member t nil) &optional)) eql))
;;; ------------------

;;; ------------------
;;; equal
;;; ------------------
;;; (declaim (ftype (function (t t) (values (member t nil) &optional)) equal))
;;; ------------------

;;; ------------------
;;; equalp
;;; ------------------
;;; (declaim (ftype (function (t t) (values (member t nil) &optional)) equalp))
;;; ------------------

;;; ------------------
;;; error
;;; ------------------
(declaim (ftype (function (t &optional t t) nil) tao:error))
;;; ------------------

;;; ------------------
;;; eval
;;; ------------------
;;; (declaim (ftype (function (t) *) eval))
;;; ------------------

;;; ------------------
;;; evenp
;;; ------------------
;;; (declaim (ftype (function (integer) (values (member t nil) &optional)) evenp))
;;; ------------------

;;; ------------------
;;; every
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest sequence) (values (member t nil) &optional)) every))
;;; ------------------

;;; ------------------
;;; exp
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) exp))
;;; ------------------

;;; ------------------
;;; exploden
;;; ------------------
(declaim (ftype (function (t) (values list &optional)) tao:exploden))
;;; ------------------

;;; ------------------
;;; export
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) export))
;;; ------------------

;;; ------------------
;;; expt
;;; ------------------
;;; (declaim (ftype (function (number number) (values number &optional)) expt))
;;; ------------------

;;; ------------------
;;; fboundp
;;; ------------------
;;; (declaim (ftype (function ((or symbol cons)) (values (member t nil) &optional)) fboundp))
;;; ------------------

;;; ------------------
;;; fceiling
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values float real &optional)) fceiling))
;;; ------------------

;;; ------------------
;;; fdefinition
;;; ------------------
;;; (declaim (ftype (function ((or symbol cons)) (values function &optional)) fdefinition))
;;; ------------------

;;; ------------------
;;; ffloor
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values float real &optional)) ffloor))
;;; ------------------

;;; ------------------
;;; fifth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) fifth))
;;; ------------------

;;; ------------------
;;; file-author
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) file-author))
;;; ------------------

;;; ------------------
;;; file-error-pathname
;;; ------------------
;;; (declaim (ftype (function (t) *) file-error-pathname))
;;; ------------------

;;; ------------------
;;; file-length
;;; ------------------
;;; (declaim (ftype (function (stream) (values (or unsigned-byte null) &optional)) file-length))
;;; ------------------

;;; ------------------
;;; file-namestring
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) file-namestring))
;;; ------------------

;;; ------------------
;;; file-position
;;; ------------------
;;; (declaim (ftype (function (stream &optional (or unsigned-byte (member :start :end))) (values (or unsigned-byte (member t nil)) &optional)) file-position))
;;; ------------------

;;; ------------------
;;; file-string-length
;;; ------------------
;;; (declaim (ftype (function (sb-kernel:ansi-stream (or (vector character) (vector nil) base-string character)) (values (or unsigned-byte null) &optional)) file-string-length))
;;; ------------------

;;; ------------------
;;; file-write-date
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or unsigned-byte null) &optional)) file-write-date))
;;; ------------------

;;; ------------------
;;; fill
;;; ------------------
;;; (declaim (ftype (function (sequence t &rest t &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values sequence &optional)) fill))
;;; ------------------

;;; ------------------
;;; fill-pointer
;;; ------------------
;;; (declaim (ftype (function ((and vector (not simple-array))) (values (mod 4611686018427387901) &optional)) fill-pointer))
;;; ------------------

;;; ------------------
;;; find
;;; ------------------
;;; (declaim (ftype (function (t sequence &rest t &key (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:from-end t) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values t &optional)) find))
;;; ------------------

;;; ------------------
;;; find-all-symbols
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character)) (values list &optional)) find-all-symbols))
;;; ------------------

;;; ------------------
;;; find-class
;;; ------------------
;;; (declaim (ftype (function (symbol &optional t (or sb-kernel:lexenv null)) (values (or class null) &optional)) find-class))
;;; ------------------

;;; ------------------
;;; find-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values t &optional)) find-if))
;;; ------------------

;;; ------------------
;;; find-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values t &optional)) find-if-not))
;;; ------------------

;;; ------------------
;;; find-method
;;; ------------------
;;; (declaim (ftype (function (t t t &optional t) *) find-method))
;;; ------------------

;;; ------------------
;;; find-package
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package)) (values (or package null) &optional)) find-package))
;;; ------------------

;;; ------------------
;;; find-restart
;;; ------------------
;;; (declaim (ftype (function ((or (and symbol (not null)) restart) &optional (or condition null)) (values (or restart null) &optional)) find-restart))
;;; ------------------

;;; ------------------
;;; find-symbol
;;; ------------------
;;; (declaim (ftype (function (string &optional (or (vector character) (vector nil) base-string symbol character package)) (values symbol (member :internal :external :inherited nil) &optional)) find-symbol))
;;; ------------------

;;; ------------------
;;; finish-output
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values null &optional)) finish-output))
;;; ------------------

;;; ------------------
;;; first
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) first))
;;; ------------------

;;; ------------------
;;; flatsize
;;; ------------------
(declaim (ftype (function (t) (values (mod 4611686018427387901) &optional)) tao:flatsize))
;;; ------------------

;;; ------------------
;;; float
;;; ------------------
;;; (declaim (ftype (function (real &optional float) (values float &optional)) float))
;;; ------------------

;;; ------------------
;;; float-digits
;;; ------------------
;;; (declaim (ftype (function (float) (values (mod 54) &optional)) float-digits))
;;; ------------------

;;; ------------------
;;; float-precision
;;; ------------------
;;; (declaim (ftype (function (float) (values (mod 54) &optional)) float-precision))
;;; ------------------

;;; ------------------
;;; float-radix
;;; ------------------
;;; (declaim (ftype (function (float) (values (integer 2 2) &optional)) float-radix))
;;; ------------------

;;; ------------------
;;; float-sign
;;; ------------------
;;; (declaim (ftype (function (float &optional float) (values float &optional)) float-sign))
;;; ------------------

;;; ------------------
;;; floatp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) floatp))
;;; ------------------

;;; ------------------
;;; floor
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values integer real &optional)) floor))
;;; ------------------

;;; ------------------
;;; fmakunbound
;;; ------------------
;;; (declaim (ftype (function ((or symbol cons)) (values (or symbol cons) &optional)) fmakunbound))
;;; ------------------

;;; ------------------
;;; force-output
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values null &optional)) force-output))
;;; ------------------

;;; ------------------
;;; format
;;; ------------------
;;; (declaim (ftype (function ((or (member nil t) stream (vector character) (vector nil) base-string) (or (vector character) (vector nil) base-string function) &rest t) (values (or (vector character) (vector nil) base-string null) &optional)) format))
;;; ------------------

;;; ------------------
;;; fourth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) fourth))
;;; ------------------

;;; ------------------
;;; fresh-line
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values (member t nil) &optional)) fresh-line))
;;; ------------------

;;; ------------------
;;; fround
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values float real &optional)) fround))
;;; ------------------

;;; ------------------
;;; ftruncate
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values float real &optional)) ftruncate))
;;; ------------------

;;; ------------------
;;; funcall
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) &rest t) *) funcall))
;;; ------------------

;;; ------------------
;;; function-keywords
;;; ------------------
;;; (declaim (ftype (function (t) *) function-keywords))
;;; ------------------

;;; ------------------
;;; function-lambda-expression
;;; ------------------
;;; (declaim (ftype (function (function) (values t (member t nil) t &optional)) function-lambda-expression))
;;; ------------------

;;; ------------------
;;; functionp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) functionp))
;;; ------------------

;;; ------------------
;;; gcd
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values unsigned-byte &optional)) gcd))
;;; ------------------

;;; ------------------
;;; gensym
;;; ------------------
;;; (declaim (ftype (function (&optional (or (vector character) (vector nil) base-string unsigned-byte)) (values symbol &optional)) gensym))
;;; ------------------

;;; ------------------
;;; gentemp
;;; ------------------
;;; (declaim (ftype (function (&optional string (or (vector character) (vector nil) base-string symbol character package)) (values symbol &optional)) gentemp))
;;; ------------------

;;; ------------------
;;; get
;;; ------------------
;;; (declaim (ftype (function (symbol t &optional t) (values t &optional)) get))
;;; ------------------

;;; ------------------
;;; get-decoded-time
;;; ------------------
;;; (declaim (ftype (function nil (values (mod 60) (mod 60) (mod 24) (integer 1 31) (integer 1 12) unsigned-byte (mod 7) (member t nil) (rational -24 24) &optional)) get-decoded-time))
;;; ------------------

;;; ------------------
;;; get-dispatch-macro-character
;;; ------------------
;;; (declaim (ftype (function (character character &optional (or readtable null)) (values (or function symbol) &optional)) get-dispatch-macro-character))
;;; ------------------

;;; ------------------
;;; get-internal-real-time
;;; ------------------
;;; (declaim (ftype (function nil (values unsigned-byte &optional)) get-internal-real-time))
;;; ------------------

;;; ------------------
;;; get-internal-run-time
;;; ------------------
;;; (declaim (ftype (function nil (values unsigned-byte &optional)) get-internal-run-time))
;;; ------------------

;;; ------------------
;;; get-macro-character
;;; ------------------
;;; (declaim (ftype (function (character &optional (or readtable null)) (values (or function symbol) (member t nil) &optional)) get-macro-character))
;;; ------------------

;;; ------------------
;;; get-output-stream-string
;;; ------------------
;;; (declaim (ftype (function (stream) (values simple-string &optional)) get-output-stream-string))
;;; ------------------

;;; ------------------
;;; get-properties
;;; ------------------
;;; (declaim (ftype (function (list list) (values t t list &optional)) get-properties))
;;; ------------------

;;; ------------------
;;; get-setf-expansion
;;; ------------------
;;; (declaim (ftype (function (t &optional (or null sb-kernel:lexenv)) *) get-setf-expansion))
;;; ------------------

;;; ------------------
;;; get-universal-time
;;; ------------------
;;; (declaim (ftype (function nil (values unsigned-byte &optional)) get-universal-time))
;;; ------------------

;;; ------------------
;;; getf
;;; ------------------
;;; (declaim (ftype (function (list t &optional t) (values t &optional)) getf))
;;; ------------------

;;; ------------------
;;; gethash
;;; ------------------
;;; (declaim (ftype (function (t hash-table &optional t) (values t (member t nil) &optional)) gethash))
;;; ------------------

;;; ------------------
;;; graphic-char-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) graphic-char-p))
;;; ------------------

;;; ------------------
;;; greaterp
;;; ------------------
(declaim (ftype (function * (values (member nil t) &optional)) tao:greaterp))
;;; ------------------

;;; ------------------
;;; grep
;;; ------------------
(declaim (ftype (function (t t &optional t t t) (values list &optional)) tao:grep))
;;; ------------------

;;; ------------------
;;; hash-table-count
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values (mod 4611686018427387901) &optional)) hash-table-count))
;;; ------------------

;;; ------------------
;;; hash-table-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) hash-table-p))
;;; ------------------

;;; ------------------
;;; hash-table-rehash-size
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values (or (mod 4611686018427387901) (single-float (1.0))) &optional)) hash-table-rehash-size))
;;; ------------------

;;; ------------------
;;; hash-table-rehash-threshold
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values (single-float (0.0) 1.0) &optional)) hash-table-rehash-threshold))
;;; ------------------

;;; ------------------
;;; hash-table-size
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values (mod 4611686018427387901) &optional)) hash-table-size))
;;; ------------------

;;; ------------------
;;; hash-table-test
;;; ------------------
;;; (declaim (ftype (function (hash-table) (values symbol &optional)) hash-table-test))
;;; ------------------

;;; ------------------
;;; host-fullname
;;; ------------------
(declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) tao:host-fullname))
;;; ------------------

;;; ------------------
;;; host-namestring
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) host-namestring))
;;; ------------------

;;; ------------------
;;; identity
;;; ------------------
;;; (declaim (ftype (function (t) (values t &optional)) identity))
;;; ------------------

;;; ------------------
;;; ignore
;;; ------------------
(declaim (ftype (function * (values null &optional)) tao:ignore))
;;; ------------------

;;; ------------------
;;; imagpart
;;; ------------------
;;; (declaim (ftype (function (number) (values real &optional)) imagpart))
;;; ------------------

;;; ------------------
;;; import
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) import))
;;; ------------------

;;; ------------------
;;; index
;;; ------------------
(declaim (ftype (function (t t &optional t) (values list &optional)) tao:index))
;;; ------------------

;;; ------------------
;;; initialize-instance
;;; ------------------
;;; (declaim (ftype (function (t &rest t &key &allow-other-keys) *) initialize-instance))
;;; ------------------

;;; ------------------
;;; input-stream-p
;;; ------------------
;;; (declaim (ftype (function (t) *) input-stream-p))
;;; ------------------

;;; ------------------
;;; inspect
;;; ------------------
;;; (declaim (ftype (function (t) (values &optional)) inspect))
;;; ------------------

;;; ------------------
;;; integer-decode-float
;;; ------------------
;;; (declaim (ftype (function (float) (values integer (integer -1127 971) (or (integer -1 -1) (integer 1 1)) &optional)) integer-decode-float))
;;; ------------------

;;; ------------------
;;; integer-length
;;; ------------------
;;; (declaim (ftype (function (integer) (values (unsigned-byte 62) &optional)) integer-length))
;;; ------------------

;;; ------------------
;;; integerp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) integerp))
;;; ------------------

;;; ------------------
;;; interactive-stream-p
;;; ------------------
;;; (declaim (ftype (function (t) *) interactive-stream-p))
;;; ------------------

;;; ------------------
;;; intern
;;; ------------------
;;; (declaim (ftype (function (string &optional (or (vector character) (vector nil) base-string symbol character package)) (values symbol (member :internal :external :inherited nil) &optional)) intern))
;;; ------------------

;;; ------------------
;;; intersection
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) intersection))
;;; ------------------

;;; ------------------
;;; intersectionq
;;; ------------------
(declaim (ftype (function (t &rest t) (values sequence &optional)) tao:intersectionq))
;;; ------------------

;;; ------------------
;;; invalid-method-error
;;; ------------------
;;; (declaim (ftype (function (t (or (vector character) (vector nil) base-string function) &rest t) *) invalid-method-error))
;;; ------------------

;;; ------------------
;;; invoke-debugger
;;; ------------------
;;; (declaim (ftype (function (condition) nil) invoke-debugger))
;;; ------------------

;;; ------------------
;;; invoke-restart
;;; ------------------
;;; (declaim (ftype (function ((or (and symbol (not null)) restart) &rest t) *) invoke-restart))
;;; ------------------

;;; ------------------
;;; invoke-restart-interactively
;;; ------------------
;;; (declaim (ftype (function ((or (and symbol (not null)) restart)) *) invoke-restart-interactively))
;;; ------------------

;;; ------------------
;;; isqrt
;;; ------------------
;;; (declaim (ftype (function (unsigned-byte) (values unsigned-byte &optional)) isqrt))
;;; ------------------

;;; ------------------
;;; keywordp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) keywordp))
;;; ------------------

;;; ------------------
;;; last
;;; ------------------
;;; (declaim (ftype (function (list &optional unsigned-byte) (values t &optional)) last))
;;; ------------------

;;; ------------------
;;; lcm
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values unsigned-byte &optional)) lcm))
;;; ------------------

;;; ------------------
;;; ldb
;;; ------------------
;;; (declaim (ftype (function (cons integer) (values integer &optional)) ldb))
;;; ------------------

;;; ------------------
;;; ldb-test
;;; ------------------
;;; (declaim (ftype (function (cons integer) (values (member t nil) &optional)) ldb-test))
;;; ------------------

;;; ------------------
;;; ldiff
;;; ------------------
;;; (declaim (ftype (function (list t) (values list &optional)) ldiff))
;;; ------------------

;;; ------------------
;;; leap-year-p
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:leap-year-p))
;;; ------------------

;;; ------------------
;;; length
;;; ------------------
(declaim (ftype (function (t) (values (or null number) &optional)) tao:length))
;;; ------------------

;;; ------------------
;;; lessp
;;; ------------------
(declaim (ftype (function * (values (member nil t) &optional)) tao:lessp))
;;; ------------------

;;; ------------------
;;; lins
;;; ------------------
(declaim (ftype (function (t t) (values t &optional)) tao:lins))
;;; ------------------

;;; ------------------
;;; lisp-implementation-type
;;; ------------------
(declaim (ftype (function nil (values (simple-base-string 4) &optional)) tao:lisp-implementation-type))
;;; ------------------

;;; ------------------
;;; lisp-implementation-version
;;; ------------------
(declaim (ftype (function nil (values (simple-base-string 6) &optional)) tao:lisp-implementation-version))
;;; ------------------

;;; ------------------
;;; list
;;; ------------------
(declaim (ftype (function * (values list &optional)) tao:list))
;;; ------------------

;;; ------------------
;;; list*
;;; ------------------
;;; (declaim (ftype (function (t &rest t) (values t &optional)) list*))
;;; ------------------

;;; ------------------
;;; list-all-packages
;;; ------------------
;;; (declaim (ftype (function nil (values list &optional)) list-all-packages))
;;; ------------------

;;; ------------------
;;; list-length
;;; ------------------
;;; (declaim (ftype (function (list) (values (or (mod 4611686018427387901) null) &optional)) list-length))
;;; ------------------

;;; ------------------
;;; listen
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values (member t nil) &optional)) listen))
;;; ------------------

;;; ------------------
;;; listp
;;; ------------------
(declaim (ftype (function (t) (values list &optional)) tao:listp))
;;; ------------------

;;; ------------------
;;; load
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname stream) &key (:verbose t) (:print t) (:if-does-not-exist t) (:external-format (or keyword (cons keyword t)))) (values (member t nil) &optional)) load))
;;; ------------------

;;; ------------------
;;; load-if-non-existent
;;; ------------------
(declaim (ftype (function (t t) (values (member t nil) &optional)) tao:load-if-non-existent))
;;; ------------------

;;; ------------------
;;; load-logical-pathname-translations
;;; ------------------
;;; (declaim (ftype (function (string) (values t &optional)) load-logical-pathname-translations))
;;; ------------------

;;; ------------------
;;; log
;;; ------------------
;;; (declaim (ftype (function (number &optional real) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) log))
;;; ------------------

;;; ------------------
;;; logand
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values integer &optional)) logand))
;;; ------------------

;;; ------------------
;;; logandc1
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) logandc1))
;;; ------------------

;;; ------------------
;;; logandc2
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) logandc2))
;;; ------------------

;;; ------------------
;;; logbitp
;;; ------------------
;;; (declaim (ftype (function (unsigned-byte integer) (values (member t nil) &optional)) logbitp))
;;; ------------------

;;; ------------------
;;; logcount
;;; ------------------
;;; (declaim (ftype (function (integer) (values (unsigned-byte 62) &optional)) logcount))
;;; ------------------

;;; ------------------
;;; logeqv
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values integer &optional)) logeqv))
;;; ------------------

;;; ------------------
;;; logical-pathname
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values logical-pathname &optional)) logical-pathname))
;;; ------------------

;;; ------------------
;;; logical-pathname-translations
;;; ------------------
;;; (declaim (ftype (function ((or sb-kernel:host (vector character) (vector nil) base-string)) (values list &optional)) logical-pathname-translations))
;;; ------------------

;;; ------------------
;;; logior
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values integer &optional)) logior))
;;; ------------------

;;; ------------------
;;; lognand
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) lognand))
;;; ------------------

;;; ------------------
;;; lognor
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) lognor))
;;; ------------------

;;; ------------------
;;; lognot
;;; ------------------
;;; (declaim (ftype (function (integer) (values integer &optional)) lognot))
;;; ------------------

;;; ------------------
;;; logorc1
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) logorc1))
;;; ------------------

;;; ------------------
;;; logorc2
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values integer &optional)) logorc2))
;;; ------------------

;;; ------------------
;;; logtest
;;; ------------------
;;; (declaim (ftype (function (integer integer) (values (member t nil) &optional)) logtest))
;;; ------------------

;;; ------------------
;;; logxor
;;; ------------------
;;; (declaim (ftype (function (&rest integer) (values integer &optional)) logxor))
;;; ------------------

;;; ------------------
;;; long-site-name
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) long-site-name))
;;; ------------------

;;; ------------------
;;; lower-case-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) lower-case-p))
;;; ------------------

;;; ------------------
;;; machine-instance
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) machine-instance))
;;; ------------------

;;; ------------------
;;; machine-type
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) machine-type))
;;; ------------------

;;; ------------------
;;; machine-version
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) machine-version))
;;; ------------------

;;; ------------------
;;; macro-function
;;; ------------------
;;; (declaim (ftype (function (symbol &optional (or sb-kernel:lexenv null)) (values (or function null) &optional)) macro-function))
;;; ------------------

;;; ------------------
;;; macroexpand
;;; ------------------
;;; (declaim (ftype (function (t &optional (or sb-kernel:lexenv null)) (values t &optional (member t nil))) macroexpand))
;;; ------------------

;;; ------------------
;;; macroexpand-1
;;; ------------------
;;; (declaim (ftype (function (t &optional (or sb-kernel:lexenv null)) (values t &optional (member t nil))) macroexpand-1))
;;; ------------------

;;; ------------------
;;; make-array
;;; ------------------
;;; (declaim (ftype (function ((or (mod 4611686018427387901) cons null) &key (:element-type (or cons symbol sb-kernel:instance)) (:initial-element t) (:initial-contents t) (:adjustable t) (:fill-pointer t) (:displaced-to (or array null)) (:displaced-index-offset (mod 4611686018427387901))) (values array &optional)) make-array))
;;; ------------------

;;; ------------------
;;; make-broadcast-stream
;;; ------------------
;;; (declaim (ftype (function (&rest stream) (values stream &optional)) make-broadcast-stream))
;;; ------------------

;;; ------------------
;;; make-concatenated-stream
;;; ------------------
;;; (declaim (ftype (function (&rest stream) (values stream &optional)) make-concatenated-stream))
;;; ------------------

;;; ------------------
;;; make-condition
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) &rest t) (values condition &optional)) make-condition))
;;; ------------------

;;; ------------------
;;; make-dispatch-macro-character
;;; ------------------
;;; (declaim (ftype (function (character &optional t readtable) (values (member t) &optional)) make-dispatch-macro-character))
;;; ------------------

;;; ------------------
;;; make-echo-stream
;;; ------------------
;;; (declaim (ftype (function (stream stream) (values stream &optional)) make-echo-stream))
;;; ------------------

;;; ------------------
;;; make-hash-table
;;; ------------------
;;; (declaim (ftype (function (&key (:test (or function symbol)) (:size unsigned-byte) (:rehash-size (or (integer 1) (single-float (1.0)) (double-float (1.0d0)))) (:rehash-threshold (or (single-float 0.0 1.0) (double-float 0.0d0 1.0d0) (rational 0 1))) (:hash-function (or function symbol)) (:weakness (member nil :key :value :key-and-value :key-or-value)) (:synchronized t)) (values hash-table &optional)) make-hash-table))
;;; ------------------

;;; ------------------
;;; make-instance
;;; ------------------
;;; (declaim (ftype (function (t &rest t &key &allow-other-keys) *) make-instance))
;;; ------------------

;;; ------------------
;;; make-instances-obsolete
;;; ------------------
;;; (declaim (ftype (function (t) *) make-instances-obsolete))
;;; ------------------

;;; ------------------
;;; make-list
;;; ------------------
;;; (declaim (ftype (function ((mod 4611686018427387901) &key (:initial-element t)) (values list &optional)) make-list))
;;; ------------------

;;; ------------------
;;; make-load-form
;;; ------------------
;;; (declaim (ftype (function (t &optional t) *) make-load-form))
;;; ------------------

;;; ------------------
;;; make-load-form-saving-slots
;;; ------------------
;;; (declaim (ftype (function (t &key (:slot-names t) (:environment t)) (values cons cons &optional)) make-load-form-saving-slots))
;;; ------------------

;;; ------------------
;;; make-package
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) &key (:use list) (:nicknames list) (:internal-symbols (mod 4611686018427387901)) (:external-symbols (mod 4611686018427387901))) (values package &optional)) make-package))
;;; ------------------

;;; ------------------
;;; make-pathname
;;; ------------------
;;; (declaim (ftype (function (&key (:defaults (or (vector character) (vector nil) base-string pathname file-stream)) (:host (or (vector character) (vector nil) base-string sb-kernel:host null)) (:device (or (vector character) (vector nil) base-string (member nil :unspecific :unc))) (:directory (or cons (vector character) (vector nil) base-string (member :wild nil))) (:name (or sb-impl::pattern (vector character) (vector nil) base-string (member . #0=(nil :unspecific :wild)))) (:type (or sb-impl::pattern (vector character) (vector nil) base-string (member . #0#))) (:version (or integer (member nil :newest :wild :unspecific))) (:case (member :local :common))) (values pathname &optional)) make-pathname))
;;; ------------------

;;; ------------------
;;; make-random-state
;;; ------------------
;;; (declaim (ftype (function (&optional (or (member nil t) random-state unsigned-byte (simple-array (unsigned-byte 8) . #0=((*))) (simple-array (unsigned-byte 32) . #0#))) (values random-state &optional)) make-random-state))
;;; ------------------

;;; ------------------
;;; make-sequence
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) (mod 4611686018427387901) &key (:initial-element t)) (values (or (simple-array * (*)) cons null sequence) &optional)) make-sequence))
;;; ------------------

;;; ------------------
;;; make-string
;;; ------------------
(declaim (ftype (function (t &optional t) (values (simple-array character (*)) &optional)) tao:make-string))
;;; ------------------

;;; ------------------
;;; make-string-input-stream
;;; ------------------
;;; (declaim (ftype (function (string &optional (mod 4611686018427387901) (or null (mod 4611686018427387901))) (values stream &optional)) make-string-input-stream))
;;; ------------------

;;; ------------------
;;; make-string-output-stream
;;; ------------------
;;; (declaim (ftype (function (&key (:element-type (or cons symbol sb-kernel:instance))) (values sb-impl::string-output-stream &optional)) make-string-output-stream))
;;; ------------------

;;; ------------------
;;; make-string-with-fill-pointer
;;; ------------------
(declaim (ftype (function (t t t) (values (array character) &optional)) tao:make-string-with-fill-pointer))
;;; ------------------

;;; ------------------
;;; make-symbol
;;; ------------------
;;; (declaim (ftype (function (string) (values symbol &optional)) make-symbol))
;;; ------------------

;;; ------------------
;;; make-synonym-stream
;;; ------------------
;;; (declaim (ftype (function (symbol) (values synonym-stream &optional)) make-synonym-stream))
;;; ------------------

;;; ------------------
;;; make-two-way-stream
;;; ------------------
;;; (declaim (ftype (function (stream stream) (values stream &optional)) make-two-way-stream))
;;; ------------------

;;; ------------------
;;; makunbound
;;; ------------------
;;; (declaim (ftype (function (symbol) (values symbol &optional)) makunbound))
;;; ------------------

;;; ------------------
;;; map
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) (or function symbol) sequence &rest sequence) (values (or (simple-array * (*)) cons null sequence) &optional)) map))
;;; ------------------

;;; ------------------
;;; map-into
;;; ------------------
;;; (declaim (ftype (function (sequence (or function symbol) &rest sequence) (values sequence &optional)) map-into))
;;; ------------------

;;; ------------------
;;; mapatoms
;;; ------------------
(declaim (ftype (function (t &optional t) (values null &optional)) tao:mapatoms))
;;; ------------------

;;; ------------------
;;; mapc
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values list &optional)) mapc))
;;; ------------------

;;; ------------------
;;; mapcan
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values t &optional)) mapcan))
;;; ------------------

;;; ------------------
;;; mapcar
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values list &optional)) mapcar))
;;; ------------------

;;; ------------------
;;; mapcon
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values t &optional)) mapcon))
;;; ------------------

;;; ------------------
;;; maphash
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) hash-table) (values null &optional)) maphash))
;;; ------------------

;;; ------------------
;;; mapl
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values list &optional)) mapl))
;;; ------------------

;;; ------------------
;;; maplist
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &rest list) (values list &optional)) maplist))
;;; ------------------

;;; ------------------
;;; mask-field
;;; ------------------
;;; (declaim (ftype (function (cons integer) (values integer &optional)) mask-field))
;;; ------------------

;;; ------------------
;;; max
;;; ------------------
;;; (declaim (ftype (function (real &rest real) (values real &optional)) max))
;;; ------------------

;;; ------------------
;;; max2
;;; ------------------
(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) tao:max2))
;;; ------------------

;;; ------------------
;;; mem
;;; ------------------
(declaim (ftype (function (t t t) (values list &optional)) tao:mem))
;;; ------------------

;;; ------------------
;;; member
;;; ------------------
;;; (declaim (ftype (function (t list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) member))
;;; ------------------

;;; ------------------
;;; member-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) member-if))
;;; ------------------

;;; ------------------
;;; member-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) member-if-not))
;;; ------------------

;;; ------------------
;;; memq
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:memq))
;;; ------------------

;;; ------------------
;;; memql
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:memql))
;;; ------------------

;;; ------------------
;;; memqu
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:memqu))
;;; ------------------

;;; ------------------
;;; merge
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) sequence sequence (or function symbol) &key (:key (or function symbol))) (values sequence &optional)) merge))
;;; ------------------

;;; ------------------
;;; merge-pathnames
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &optional (or (vector character) (vector nil) base-string pathname file-stream) (or integer (member nil :newest :wild :unspecific))) (values pathname &optional)) merge-pathnames))
;;; ------------------

;;; ------------------
;;; method-combination-error
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string function) &rest t) *) method-combination-error))
;;; ------------------

;;; ------------------
;;; method-qualifiers
;;; ------------------
;;; (declaim (ftype function method-qualifiers))
;;; ------------------

;;; ------------------
;;; min
;;; ------------------
;;; (declaim (ftype (function (real &rest real) (values real &optional)) min))
;;; ------------------

;;; ------------------
;;; minusp
;;; ------------------
;;; (declaim (ftype (function (real) (values (member t nil) &optional)) minusp))
;;; ------------------

;;; ------------------
;;; mismatch
;;; ------------------
;;; (declaim (ftype (function (sequence sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (or (mod 4611686018427387901) null) &optional)) mismatch))
;;; ------------------

;;; ------------------
;;; mod
;;; ------------------
;;; (declaim (ftype (function (real real) (values real &optional)) mod))
;;; ------------------

;;; ------------------
;;; month-string
;;; ------------------
(declaim (ftype (function (t) (values t &optional)) tao:month-string))
;;; ------------------

;;; ------------------
;;; muffle-warning
;;; ------------------
;;; (declaim (ftype (function (&optional (or condition null)) nil) muffle-warning))
;;; ------------------

;;; ------------------
;;; name-char
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character)) (values (or character null) &optional)) name-char))
;;; ------------------

;;; ------------------
;;; namestring
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) namestring))
;;; ------------------

;;; ------------------
;;; nbutlast
;;; ------------------
;;; (declaim (ftype (function (list &optional unsigned-byte) (values list &optional)) nbutlast))
;;; ------------------

;;; ------------------
;;; nconc
;;; ------------------
(declaim (ftype (function * (values t &optional)) tao:nconc))
;;; ------------------

;;; ------------------
;;; ncons
;;; ------------------
(declaim (ftype (function (t) (values cons &optional)) tao:ncons))
;;; ------------------

;;; ------------------
;;; neg
;;; ------------------
(declaim (ftype (function (t) (values number &optional)) tao:neg))
;;; ------------------

;;; ------------------
;;; neq
;;; ------------------
(declaim (ftype (function (t t) (values (member t nil) &optional)) tao:neq))
;;; ------------------

;;; ------------------
;;; nintersection
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) nintersection))
;;; ------------------

;;; ------------------
;;; ninth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) ninth))
;;; ------------------

;;; ------------------
;;; nleft
;;; ------------------
(declaim (ftype (function (t t &optional t) (values t &optional)) tao:nleft))
;;; ------------------

;;; ------------------
;;; nlistp
;;; ------------------
(declaim (ftype (function (t) (values (member t nil) &optional)) tao:nlistp))
;;; ------------------

;;; ------------------
;;; no-applicable-method
;;; ------------------
;;; (declaim (ftype function no-applicable-method))
;;; ------------------

;;; ------------------
;;; no-next-method
;;; ------------------
;;; (declaim (ftype function no-next-method))
;;; ------------------

;;; ------------------
;;; not
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) not))
;;; ------------------

;;; ------------------
;;; notany
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest sequence) (values (member t nil) &optional)) notany))
;;; ------------------

;;; ------------------
;;; notevery
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest sequence) (values (member t nil) &optional)) notevery))
;;; ------------------

;;; ------------------
;;; nreconc
;;; ------------------
;;; (declaim (ftype (function (list t) (values t &optional)) nreconc))
;;; ------------------

;;; ------------------
;;; nreverse
;;; ------------------
(declaim (ftype function tao:nreverse))
;;; ------------------

;;; ------------------
;;; nset-difference
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) nset-difference))
;;; ------------------

;;; ------------------
;;; nset-exclusive-or
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) nset-exclusive-or))
;;; ------------------

;;; ------------------
;;; nstring-capitalize
;;; ------------------
;;; (declaim (ftype (function (string &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values string &optional)) nstring-capitalize))
;;; ------------------

;;; ------------------
;;; nstring-downcase
;;; ------------------
;;; (declaim (ftype (function (string &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values string &optional)) nstring-downcase))
;;; ------------------

;;; ------------------
;;; nstring-upcase
;;; ------------------
;;; (declaim (ftype (function (string &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values string &optional)) nstring-upcase))
;;; ------------------

;;; ------------------
;;; nsublis
;;; ------------------
;;; (declaim (ftype (function (list t &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values t &optional)) nsublis))
;;; ------------------

;;; ------------------
;;; nsubst
;;; ------------------
;;; (declaim (ftype (function (t t t &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values t &optional)) nsubst))
;;; ------------------

;;; ------------------
;;; nsubst-if
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) t &key (:key (or function symbol))) (values t &optional)) nsubst-if))
;;; ------------------

;;; ------------------
;;; nsubst-if-not
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) t &key (:key (or function symbol))) (values t &optional)) nsubst-if-not))
;;; ------------------

;;; ------------------
;;; nsubstitute
;;; ------------------
;;; (declaim (ftype (function (t t sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values sequence &optional)) nsubstitute))
;;; ------------------

;;; ------------------
;;; nsubstitute-if
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values sequence &optional)) nsubstitute-if))
;;; ------------------

;;; ------------------
;;; nsubstitute-if-not
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values sequence &optional)) nsubstitute-if-not))
;;; ------------------

;;; ------------------
;;; nth
;;; ------------------
;;; (declaim (ftype (function (unsigned-byte list) (values t &optional)) nth))
;;; ------------------

;;; ------------------
;;; nthcdr
;;; ------------------
;;; (declaim (ftype (function (unsigned-byte list) (values t &optional)) nthcdr))
;;; ------------------

;;; ------------------
;;; null
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) null))
;;; ------------------

;;; ------------------
;;; null-string
;;; ------------------
(declaim (ftype (function (t) (values (or null (simple-array character (0))) &optional)) tao:null-string))
;;; ------------------

;;; ------------------
;;; numberp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) numberp))
;;; ------------------

;;; ------------------
;;; numerator
;;; ------------------
;;; (declaim (ftype (function (rational) (values integer &optional)) numerator))
;;; ------------------

;;; ------------------
;;; nunion
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) nunion))
;;; ------------------

;;; ------------------
;;; oddp
;;; ------------------
;;; (declaim (ftype (function (integer) (values (member t nil) &optional)) oddp))
;;; ------------------

;;; ------------------
;;; open
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:direction (member :input :output :io :probe)) (:element-type (or cons symbol sb-kernel:instance)) (:if-exists (member :error :new-version :rename :rename-and-delete :overwrite :append :supersede nil)) (:if-does-not-exist (member :error :create nil)) (:external-format (or keyword (cons keyword t)))) (values (or stream null) &optional)) open))
;;; ------------------

;;; ------------------
;;; open-stream-p
;;; ------------------
;;; (declaim (ftype (function (t) *) open-stream-p))
;;; ------------------

;;; ------------------
;;; output-stream-p
;;; ------------------
;;; (declaim (ftype (function (t) *) output-stream-p))
;;; ------------------

;;; ------------------
;;; package-error-package
;;; ------------------
;;; (declaim (ftype (function (t) *) package-error-package))
;;; ------------------

;;; ------------------
;;; package-name
;;; ------------------
(declaim (ftype (function (t) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string cons null) &optional)) tao:package-name))
;;; ------------------

;;; ------------------
;;; package-nicknames
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package)) (values list &optional)) package-nicknames))
;;; ------------------

;;; ------------------
;;; package-shadowing-symbols
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package)) (values list &optional)) package-shadowing-symbols))
;;; ------------------

;;; ------------------
;;; package-use-list
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package)) (values list &optional)) package-use-list))
;;; ------------------

;;; ------------------
;;; package-used-by-list
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package)) (values list &optional)) package-used-by-list))
;;; ------------------

;;; ------------------
;;; packagep
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) packagep))
;;; ------------------

;;; ------------------
;;; pairlis
;;; ------------------
;;; (declaim (ftype (function (t t &optional t) (values list &optional)) pairlis))
;;; ------------------

;;; ------------------
;;; parse-integer
;;; ------------------
;;; (declaim (ftype (function (string &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:radix (integer 2 36)) (:junk-allowed t)) (values (or integer null) (mod 4611686018427387901) &optional)) parse-integer))
;;; ------------------

;;; ------------------
;;; parse-namestring
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &optional (or cons sb-kernel:host (vector character) (vector nil) base-string (member :unspecific nil)) (or (vector character) (vector nil) base-string pathname file-stream) &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:junk-allowed t)) (values (or pathname null) (or null (mod 4611686018427387901)) &optional)) parse-namestring))
;;; ------------------

;;; ------------------
;;; pathname
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values pathname &optional)) pathname))
;;; ------------------

;;; ------------------
;;; pathname-device
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:case (member :local :common))) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string (member nil :unspecific :unc)) &optional)) pathname-device))
;;; ------------------

;;; ------------------
;;; pathname-directory
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:case (member :local :common))) (values list &optional)) pathname-directory))
;;; ------------------

;;; ------------------
;;; pathname-host
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:case (member :local :common))) (values (or sb-kernel:host null) &optional)) pathname-host))
;;; ------------------

;;; ------------------
;;; pathname-match-p
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) (or (vector character) (vector nil) base-string pathname file-stream)) (values t &optional)) pathname-match-p))
;;; ------------------

;;; ------------------
;;; pathname-name
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:case (member :local :common))) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string sb-impl::pattern (member nil :unspecific :wild)) &optional)) pathname-name))
;;; ------------------

;;; ------------------
;;; pathname-type
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key (:case (member :local :common))) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string sb-impl::pattern (member nil :unspecific :wild)) &optional)) pathname-type))
;;; ------------------

;;; ------------------
;;; pathname-version
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or integer (member nil :newest :wild :unspecific)) &optional)) pathname-version))
;;; ------------------

;;; ------------------
;;; pathnamep
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) pathnamep))
;;; ------------------

;;; ------------------
;;; peek-char
;;; ------------------
;;; (declaim (ftype (function (&optional (or character (member . #0=(nil t))) (or stream (member . #0#)) t t t) (values t &optional)) peek-char))
;;; ------------------

;;; ------------------
;;; phase
;;; ------------------
;;; (declaim (ftype (function (number) (values number &optional)) phase))
;;; ------------------

;;; ------------------
;;; plist
;;; ------------------
(declaim (ftype (function (symbol) (values list &optional)) tao:plist))
;;; ------------------

;;; ------------------
;;; plus
;;; ------------------
(declaim (ftype (function * (values number &optional)) tao:plus))
;;; ------------------

;;; ------------------
;;; plusp
;;; ------------------
;;; (declaim (ftype (function (real) (values (member t nil) &optional)) plusp))
;;; ------------------

;;; ------------------
;;; pname
;;; ------------------
(declaim (ftype (function (t) *) tao:pname))
;;; ------------------

;;; ------------------
;;; pname-of-time
;;; ------------------
(declaim (ftype (function (t) (values (or (vector character) (vector nil) base-string null) &optional)) tao:pname-of-time))
;;; ------------------

;;; ------------------
;;; position
;;; ------------------
;;; (declaim (ftype (function (t sequence &rest t &key (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:from-end t) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (or (mod 4611686018427387901) null) &optional)) position))
;;; ------------------

;;; ------------------
;;; position-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (or (mod 4611686018427387901) null) &optional)) position-if))
;;; ------------------

;;; ------------------
;;; position-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (or (mod 4611686018427387901) null) &optional)) position-if-not))
;;; ------------------

;;; ------------------
;;; pprint
;;; ------------------
;;; (declaim (ftype (function (t &optional (or stream (member nil t))) (values &optional)) pprint))
;;; ------------------

;;; ------------------
;;; pprint-dispatch
;;; ------------------
;;; (declaim (ftype (function (t &optional (or sb-pretty:pprint-dispatch-table null)) (values (or function symbol) (member t nil) &optional)) pprint-dispatch))
;;; ------------------

;;; ------------------
;;; pprint-fill
;;; ------------------
;;; (declaim (ftype (function ((or stream (member nil t)) t &optional t t) (values null &optional)) pprint-fill))
;;; ------------------

;;; ------------------
;;; pprint-indent
;;; ------------------
;;; (declaim (ftype (function ((member :block :current) real &optional (or stream (member nil t))) (values null &optional)) pprint-indent))
;;; ------------------

;;; ------------------
;;; pprint-linear
;;; ------------------
;;; (declaim (ftype (function ((or stream (member nil t)) t &optional t t) (values null &optional)) pprint-linear))
;;; ------------------

;;; ------------------
;;; pprint-newline
;;; ------------------
;;; (declaim (ftype (function ((member :linear :fill :miser :mandatory) &optional (or stream (member nil t))) (values null &optional)) pprint-newline))
;;; ------------------

;;; ------------------
;;; pprint-tab
;;; ------------------
;;; (declaim (ftype (function ((member :line :section :line-relative :section-relative) unsigned-byte unsigned-byte &optional (or stream (member nil t))) (values null &optional)) pprint-tab))
;;; ------------------

;;; ------------------
;;; pprint-tabular
;;; ------------------
;;; (declaim (ftype (function ((or stream (member nil t)) t &optional t t unsigned-byte) (values null &optional)) pprint-tabular))
;;; ------------------

;;; ------------------
;;; prin1
;;; ------------------
;;; (declaim (ftype (function (t &optional (or stream (member nil t))) (values t &optional)) prin1))
;;; ------------------

;;; ------------------
;;; prin1-to-string
;;; ------------------
;;; (declaim (ftype (function (t) (values simple-string &optional)) prin1-to-string))
;;; ------------------

;;; ------------------
;;; princ
;;; ------------------
;;; (declaim (ftype (function (t &optional (or stream (member nil t))) (values t &optional)) princ))
;;; ------------------

;;; ------------------
;;; princ-to-string
;;; ------------------
;;; (declaim (ftype (function (t) (values simple-string &optional)) princ-to-string))
;;; ------------------

;;; ------------------
;;; prins
;;; ------------------
(declaim (ftype (function (t &optional t) (values t &optional)) tao:prins))
;;; ------------------

;;; ------------------
;;; print
;;; ------------------
;;; (declaim (ftype (function (t &optional (or stream (member nil t))) (values t &optional)) print))
;;; ------------------

;;; ------------------
;;; print-not-readable-object
;;; ------------------
;;; (declaim (ftype (function (t) *) print-not-readable-object))
;;; ------------------

;;; ------------------
;;; print-object
;;; ------------------
;;; (declaim (ftype (function (t t) *) print-object))
;;; ------------------

;;; ------------------
;;; probe-file
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values (or pathname null) &optional)) probe-file))
;;; ------------------

;;; ------------------
;;; proclaim
;;; ------------------
;;; (declaim (ftype (function (list) (values &optional)) proclaim))
;;; ------------------

;;; ------------------
;;; provide
;;; ------------------
(declaim (ftype (function (t) (values (member t) &optional)) tao:provide))
;;; ------------------

;;; ------------------
;;; put-alist
;;; ------------------
(declaim (ftype (function (t t t &key (:test t)) (values list &optional)) tao:put-alist))
;;; ------------------

;;; ------------------
;;; put-toga
;;; ------------------
(declaim (ftype (function (t) (values cons &optional)) tao:put-toga))
;;; ------------------

;;; ------------------
;;; putplist
;;; ------------------
(declaim (ftype (function (t t t) (values list &optional)) tao:putplist))
;;; ------------------

;;; ------------------
;;; putprop
;;; ------------------
(declaim (ftype (function (t t t) (values t &optional)) tao:putprop))
;;; ------------------

;;; ------------------
;;; random
;;; ------------------
;;; (declaim (ftype (function ((or (single-float (0.0)) (double-float (0.0d0)) (integer 1)) &optional random-state) (values (or (single-float 0.0) (double-float 0.0d0) unsigned-byte) &optional)) random))
;;; ------------------

;;; ------------------
;;; random-state-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) random-state-p))
;;; ------------------

;;; ------------------
;;; rass
;;; ------------------
(declaim (ftype (function (t t t) (values list &optional)) tao:rass))
;;; ------------------

;;; ------------------
;;; rassoc
;;; ------------------
;;; (declaim (ftype (function (t list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) rassoc))
;;; ------------------

;;; ------------------
;;; rassoc-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) rassoc-if))
;;; ------------------

;;; ------------------
;;; rassoc-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) list &key (:key (or function symbol))) (values list &optional)) rassoc-if-not))
;;; ------------------

;;; ------------------
;;; rassq
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:rassq))
;;; ------------------

;;; ------------------
;;; rassql
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:rassql))
;;; ------------------

;;; ------------------
;;; rassqu
;;; ------------------
(declaim (ftype (function (t t) (values list &optional)) tao:rassqu))
;;; ------------------

;;; ------------------
;;; rational
;;; ------------------
;;; (declaim (ftype (function (real) (values rational &optional)) rational))
;;; ------------------

;;; ------------------
;;; rationalize
;;; ------------------
;;; (declaim (ftype (function (real) (values rational &optional)) rationalize))
;;; ------------------

;;; ------------------
;;; rationalp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) rationalp))
;;; ------------------

;;; ------------------
;;; read
;;; ------------------
(declaim (ftype (function (&optional t) (values t &optional)) tao:read))
;;; ------------------

;;; ------------------
;;; read-byte
;;; ------------------
;;; (declaim (ftype (function (stream &optional t t) (values t &optional)) read-byte))
;;; ------------------

;;; ------------------
;;; read-char
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t)) t t t) (values t &optional)) read-char))
;;; ------------------

;;; ------------------
;;; read-char-no-hang
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t)) t t t) (values t &optional)) read-char-no-hang))
;;; ------------------

;;; ------------------
;;; read-delimited-list
;;; ------------------
;;; (declaim (ftype (function (character &optional (or stream (member nil t)) t) (values list &optional)) read-delimited-list))
;;; ------------------

;;; ------------------
;;; read-from-string
;;; ------------------
;;; (declaim (ftype (function (string &optional t t &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:preserve-whitespace t)) (values t (mod 4611686018427387901) &optional)) read-from-string))
;;; ------------------

;;; ------------------
;;; read-line
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t)) t t t) (values t (member t nil) &optional)) read-line))
;;; ------------------

;;; ------------------
;;; read-preserving-whitespace
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t)) t t t) (values t &optional)) read-preserving-whitespace))
;;; ------------------

;;; ------------------
;;; read-sequence
;;; ------------------
;;; (declaim (ftype (function (sequence stream &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values (mod 4611686018427387901) &optional)) read-sequence))
;;; ------------------

;;; ------------------
;;; readl
;;; ------------------
(declaim (ftype (function (&optional t t) (values list &optional)) tao:readl))
;;; ------------------

;;; ------------------
;;; readline
;;; ------------------
(declaim (ftype (function (&optional t t) *) tao:readline))
;;; ------------------

;;; ------------------
;;; readline-to-string
;;; ------------------
(declaim (ftype (function (&optional t) (values t (member t nil) &optional)) tao:readline-to-string))
;;; ------------------

;;; ------------------
;;; reads
;;; ------------------
(declaim (ftype (function (&optional t) (values t (member t nil) &optional)) tao:reads))
;;; ------------------

;;; ------------------
;;; readtable-case
;;; ------------------
;;; (declaim (ftype (function (t) (values (member :invert :preserve :downcase :upcase) &optional)) readtable-case))
;;; ------------------

;;; ------------------
;;; readtablep
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) readtablep))
;;; ------------------

;;; ------------------
;;; realp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) realp))
;;; ------------------

;;; ------------------
;;; realpart
;;; ------------------
;;; (declaim (ftype (function (number) (values real &optional)) realpart))
;;; ------------------

;;; ------------------
;;; reduce
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:initial-value t) (:key (or function symbol))) (values t &optional)) reduce))
;;; ------------------

;;; ------------------
;;; reinitialize-instance
;;; ------------------
;;; (declaim (ftype (function (t &rest t &key &allow-other-keys) *) reinitialize-instance))
;;; ------------------

;;; ------------------
;;; rem
;;; ------------------
(declaim (ftype (function (t t t &optional t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:rem))
;;; ------------------

;;; ------------------
;;; rem-if
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:rem-if))
;;; ------------------

;;; ------------------
;;; rem-if-not
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:rem-if-not))
;;; ------------------

;;; ------------------
;;; remhash
;;; ------------------
;;; (declaim (ftype (function (t hash-table) (values (member t nil) &optional)) remhash))
;;; ------------------

;;; ------------------
;;; remove
;;; ------------------
;;; (declaim (ftype (function (t sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) remove))
;;; ------------------

;;; ------------------
;;; remove-duplicates
;;; ------------------
;;; (declaim (ftype (function (sequence &rest t &key (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:from-end t) (:end (or null (mod 4611686018427387901))) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) remove-duplicates))
;;; ------------------

;;; ------------------
;;; remove-if
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) remove-if))
;;; ------------------

;;; ------------------
;;; remove-if-not
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) remove-if-not))
;;; ------------------

;;; ------------------
;;; remove-method
;;; ------------------
;;; (declaim (ftype (function (t t) *) remove-method))
;;; ------------------

;;; ------------------
;;; remprop
;;; ------------------
;;; (declaim (ftype (function (symbol t) (values t &optional)) remprop))
;;; ------------------

;;; ------------------
;;; remq
;;; ------------------
(declaim (ftype (function (t t &optional t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:remq))
;;; ------------------

;;; ------------------
;;; remql
;;; ------------------
(declaim (ftype (function (t t &optional t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:remql))
;;; ------------------

;;; ------------------
;;; remqu
;;; ------------------
(declaim (ftype (function (t t &optional t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:remqu))
;;; ------------------

;;; ------------------
;;; rename-file
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) (or (vector character) (vector nil) base-string pathname)) (values pathname pathname pathname &optional)) rename-file))
;;; ------------------

;;; ------------------
;;; rename-package
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character package) (or (vector character) (vector nil) base-string symbol character package) &optional list) (values package &optional)) rename-package))
;;; ------------------

;;; ------------------
;;; replace
;;; ------------------
;;; (declaim (ftype (function (sequence sequence &rest t &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values sequence &optional)) replace))
;;; ------------------

;;; ------------------
;;; require
;;; ------------------
;;; (declaim (ftype (function (t &optional t) (values list &optional)) require))
;;; ------------------

;;; ------------------
;;; rest
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) rest))
;;; ------------------

;;; ------------------
;;; restart-name
;;; ------------------
;;; (declaim (ftype (function (restart) (values symbol &optional)) restart-name))
;;; ------------------

;;; ------------------
;;; revappend
;;; ------------------
;;; (declaim (ftype (function (list t) (values t &optional)) revappend))
;;; ------------------

;;; ------------------
;;; reverse
;;; ------------------
;;; (declaim (ftype (function (sequence) (values (or (simple-array * (*)) cons null sequence) &optional)) reverse))
;;; ------------------

;;; ------------------
;;; room
;;; ------------------
;;; (declaim (ftype (function (&optional (member t nil :default)) (values &optional)) room))
;;; ------------------

;;; ------------------
;;; round
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values integer real &optional)) round))
;;; ------------------

;;; ------------------
;;; row-major-aref
;;; ------------------
;;; (declaim (ftype (function (array (mod 4611686018427387901)) (values t &optional)) row-major-aref))
;;; ------------------

;;; ------------------
;;; rplaca
;;; ------------------
;;; (declaim (ftype (function (cons t) (values cons &optional)) rplaca))
;;; ------------------

;;; ------------------
;;; rplacd
;;; ------------------
;;; (declaim (ftype (function (cons t) (values cons &optional)) rplacd))
;;; ------------------

;;; ------------------
;;; sass
;;; ------------------
(declaim (ftype (function (t t t t) *) tao:sass))
;;; ------------------

;;; ------------------
;;; sassq
;;; ------------------
(declaim (ftype (function (t t t) *) tao:sassq))
;;; ------------------

;;; ------------------
;;; sassql
;;; ------------------
(declaim (ftype (function (t t t) *) tao:sassql))
;;; ------------------

;;; ------------------
;;; sassqu
;;; ------------------
(declaim (ftype (function (t t t) *) tao:sassqu))
;;; ------------------

;;; ------------------
;;; sbit
;;; ------------------
;;; (declaim (ftype (function ((simple-array bit) &rest (mod 4611686018427387901)) (values bit &optional)) sbit))
;;; ------------------

;;; ------------------
;;; scale-float
;;; ------------------
;;; (declaim (ftype (function (float integer) (values float &optional)) scale-float))
;;; ------------------

;;; ------------------
;;; schar
;;; ------------------
(declaim (ftype (function (t t) (values (simple-array character (*)) &optional)) tao:schar))
;;; ------------------

;;; ------------------
;;; sconc
;;; ------------------
(declaim (ftype (function * (values (simple-array character (*)) &optional)) tao:sconc))
;;; ------------------

;;; ------------------
;;; search
;;; ------------------
(declaim (ftype (function (sequence sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start1 (mod 4611686018427387901)) (:end1 (or (mod 4611686018427387901) null)) (:start2 (mod 4611686018427387901)) (:end2 (or (mod 4611686018427387901) null)) (:key (or function symbol))) (values (or (mod 4611686018427387901) null) &optional)) tao:search))
;;; ------------------

;;; ------------------
;;; second
;;; ------------------
(declaim (ftype (function (list) (values t &optional)) tao:second))
;;; ------------------

;;; ------------------
;;; self-eval-form-p
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:self-eval-form-p))
;;; ------------------

;;; ------------------
;;; selfass-cons
;;; ------------------
(declaim (ftype (function (t t) (values cons &optional)) tao:selfass-cons))
;;; ------------------

;;; ------------------
;;; selfass-list
;;; ------------------
(declaim (ftype (function * (values cons &optional)) tao:selfass-list))
;;; ------------------

;;; ------------------
;;; selfassp
;;; ------------------
(declaim (ftype (function (t) (values list &optional)) tao:selfassp))
;;; ------------------

;;; ------------------
;;; sequal
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) tao:sequal))
;;; ------------------

;;; ------------------
;;; sequencep
;;; ------------------
(declaim (ftype (function (t) (values (member t nil) &optional)) tao:sequencep))
;;; ------------------

;;; ------------------
;;; set
;;; ------------------
(declaim (ftype (function (symbol t) (values t &optional)) tao:set))
;;; ------------------

;;; ------------------
;;; set-difference
;;; ------------------
(declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) tao:set-difference))
;;; ------------------

;;; ------------------
;;; set-differenceq
;;; ------------------
(declaim (ftype (function (t &rest t) (values list &optional)) tao:set-differenceq))
;;; ------------------

;;; ------------------
;;; set-dispatch-macro-character
;;; ------------------
;;; (declaim (ftype (function (character character (or function symbol) &optional (or readtable null)) (values (member t) &optional)) set-dispatch-macro-character))
;;; ------------------

;;; ------------------
;;; set-exclusive-or
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) set-exclusive-or))
;;; ------------------

;;; ------------------
;;; set-macro-character
;;; ------------------
;;; (declaim (ftype (function (character (or function symbol) &optional t (or readtable null)) (values (member t) &optional)) set-macro-character))
;;; ------------------

;;; ------------------
;;; set-pprint-dispatch
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) (or function symbol) &optional real sb-pretty:pprint-dispatch-table) (values null &optional)) set-pprint-dispatch))
;;; ------------------

;;; ------------------
;;; set-syntax-from-char
;;; ------------------
;;; (declaim (ftype (function (character character &optional readtable (or readtable null)) (values (member t) &optional)) set-syntax-from-char))
;;; ------------------

;;; ------------------
;;; seventh
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) seventh))
;;; ------------------

;;; ------------------
;;; shadow
;;; ------------------
;;; (declaim (ftype (function ((or character (vector character) (vector nil) base-string cons symbol) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) shadow))
;;; ------------------

;;; ------------------
;;; shadowing-import
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) shadowing-import))
;;; ------------------

;;; ------------------
;;; shared-initialize
;;; ------------------
;;; (declaim (ftype (function (t t &rest t &key &allow-other-keys) *) shared-initialize))
;;; ------------------

;;; ------------------
;;; shead
;;; ------------------
(declaim (ftype (function (t &optional t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:shead))
;;; ------------------

;;; ------------------
;;; short-site-name
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) short-site-name))
;;; ------------------

;;; ------------------
;;; signal
;;; ------------------
;;; (declaim (ftype (function (t &rest t) (values null &optional)) signal))
;;; ------------------

;;; ------------------
;;; signum
;;; ------------------
;;; (declaim (ftype (function (number) (values number &optional)) signum))
;;; ------------------

;;; ------------------
;;; simple-bit-vector-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) simple-bit-vector-p))
;;; ------------------

;;; ------------------
;;; simple-condition-format-arguments
;;; ------------------
;;; (declaim (ftype (function (t) *) simple-condition-format-arguments))
;;; ------------------

;;; ------------------
;;; simple-condition-format-control
;;; ------------------
;;; (declaim (ftype (function (t) *) simple-condition-format-control))
;;; ------------------

;;; ------------------
;;; simple-string-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) simple-string-p))
;;; ------------------

;;; ------------------
;;; simple-vector-p
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) simple-vector-p))
;;; ------------------

;;; ------------------
;;; sin
;;; ------------------
;;; (declaim (ftype (function (number) (values (or (single-float -1.0 1.0) (double-float -1.0d0 1.0d0) (complex single-float) (complex double-float)) &optional)) sin))
;;; ------------------

;;; ------------------
;;; sinh
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) sinh))
;;; ------------------

;;; ------------------
;;; sixth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) sixth))
;;; ------------------

;;; ------------------
;;; sleep
;;; ------------------
;;; (declaim (ftype (function ((or (single-float 0.0) (double-float 0.0d0) (rational 0))) (values null &optional)) sleep))
;;; ------------------

;;; ------------------
;;; slength
;;; ------------------
(declaim (ftype (function (string) (values (mod 4611686018427387901) &optional)) tao:slength))
;;; ------------------

;;; ------------------
;;; slex
;;; ------------------
(declaim (ftype (function (t t) (values (or (vector character) (vector nil) base-string null) &optional)) tao:slex))
;;; ------------------

;;; ------------------
;;; slot-boundp
;;; ------------------
;;; (declaim (ftype (function (t symbol) (values (member t nil) &optional)) slot-boundp))
;;; ------------------

;;; ------------------
;;; slot-exists-p
;;; ------------------
;;; (declaim (ftype (function (t symbol) (values (member t nil) &optional)) slot-exists-p))
;;; ------------------

;;; ------------------
;;; slot-makunbound
;;; ------------------
;;; (declaim (ftype (function (t t) (values t &optional)) slot-makunbound))
;;; ------------------

;;; ------------------
;;; slot-missing
;;; ------------------
;;; (declaim (ftype (function (t t t t &optional t) *) slot-missing))
;;; ------------------

;;; ------------------
;;; slot-unbound
;;; ------------------
;;; (declaim (ftype (function (t t t) *) slot-unbound))
;;; ------------------

;;; ------------------
;;; slot-value
;;; ------------------
;;; (declaim (ftype (function (t symbol) (values t &optional)) slot-value))
;;; ------------------

;;; ------------------
;;; smemq
;;; ------------------
(declaim (ftype (function (t t) *) tao:smemq))
;;; ------------------

;;; ------------------
;;; smemq-case
;;; ------------------
(declaim (ftype (function (t t) *) tao:smemq-case))
;;; ------------------

;;; ------------------
;;; snull
;;; ------------------
(declaim (ftype (function (t) (values (or (simple-array character (0)) null) &optional)) tao:snull))
;;; ------------------

;;; ------------------
;;; software-type
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) software-type))
;;; ------------------

;;; ------------------
;;; software-version
;;; ------------------
;;; (declaim (ftype (function nil (values (or (simple-array character . #0=((*))) (simple-array nil . #0#) simple-base-string null) &optional)) software-version))
;;; ------------------

;;; ------------------
;;; some
;;; ------------------
;;; (declaim (ftype (function ((or function symbol) sequence &rest sequence) (values t &optional)) some))
;;; ------------------

;;; ------------------
;;; sort
;;; ------------------
(declaim (ftype (function (t t) (values sequence &optional)) tao:sort))
;;; ------------------

;;; ------------------
;;; sortcar
;;; ------------------
(declaim (ftype (function (t t) (values sequence &optional)) tao:sortcar))
;;; ------------------

;;; ------------------
;;; special-operator-p
;;; ------------------
;;; (declaim (ftype (function (symbol) (values t &optional)) special-operator-p))
;;; ------------------

;;; ------------------
;;; sqrt
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) sqrt))
;;; ------------------

;;; ------------------
;;; sreverse
;;; ------------------
(declaim (ftype function tao:sreverse))
;;; ------------------

;;; ------------------
;;; stable-sort
;;; ------------------
;;; (declaim (ftype (function (sequence (or function symbol) &rest t &key (:key (or function symbol))) (values sequence &optional)) stable-sort))
;;; ------------------

;;; ------------------
;;; stail
;;; ------------------
(declaim (ftype (function (t &optional t) (values sequence &optional)) tao:stail))
;;; ------------------

;;; ------------------
;;; standard-char-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) standard-char-p))
;;; ------------------

;;; ------------------
;;; standard-read
;;; ------------------
(declaim (ftype (function (&optional t) (values t &optional)) tao:standard-read))
;;; ------------------

;;; ------------------
;;; store-value
;;; ------------------
;;; (declaim (ftype (function (t &optional (or condition null)) (values null &optional)) store-value))
;;; ------------------

;;; ------------------
;;; stream-element-type
;;; ------------------
;;; (declaim (ftype (function (t) *) stream-element-type))
;;; ------------------

;;; ------------------
;;; stream-error-stream
;;; ------------------
;;; (declaim (ftype (function (t) *) stream-error-stream))
;;; ------------------

;;; ------------------
;;; stream-external-format
;;; ------------------
;;; (declaim (ftype (function (stream) (values t &optional)) stream-external-format))
;;; ------------------

;;; ------------------
;;; streamp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) streamp))
;;; ------------------

;;; ------------------
;;; strh-to-char
;;; ------------------
(declaim (ftype (function (t) (values character &optional)) tao:strh-to-char))
;;; ------------------

;;; ------------------
;;; string
;;; ------------------
(declaim (ftype (function (t) (values string &optional)) tao:string))
;;; ------------------

;;; ------------------
;;; string-append
;;; ------------------
(declaim (ftype (function * (values (or (simple-array * (*)) cons null sequence) &optional)) tao:string-append))
;;; ------------------

;;; ------------------
;;; string-byte-count
;;; ------------------
(declaim (ftype (function (t) (values (mod 4611686018427387901) &optional)) tao:string-byte-count))
;;; ------------------

;;; ------------------
;;; string-capitalize
;;; ------------------
(declaim (ftype (function (t &optional t t) (values simple-string &optional)) tao:string-capitalize))
;;; ------------------

;;; ------------------
;;; string-char-p
;;; ------------------
(declaim (ftype (function (t) (values (or (vector character) (vector nil) base-string character) &optional)) tao:string-char-p))
;;; ------------------

;;; ------------------
;;; string-compare
;;; ------------------
(declaim (ftype (function (t t) *) tao:string-compare))
;;; ------------------

;;; ------------------
;;; string-compare-case
;;; ------------------
(declaim (ftype (function (t t) *) tao:string-compare-case))
;;; ------------------

;;; ------------------
;;; string-downcase
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values simple-string &optional)) string-downcase))
;;; ------------------

;;; ------------------
;;; string-equal
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (member t nil) &optional)) string-equal))
;;; ------------------

;;; ------------------
;;; string-fill
;;; ------------------
(declaim (ftype (function (t t &optional t t) (values sequence &optional)) tao:string-fill))
;;; ------------------

;;; ------------------
;;; string-fill-pointer
;;; ------------------
(declaim (ftype (function (t) (values (or null (mod 4611686018427387901)) &optional)) tao:string-fill-pointer))
;;; ------------------

;;; ------------------
;;; string-greater-or-equal
;;; ------------------
(declaim (ftype (function (t t) (values (or (mod 4611686018427387901) (member t nil)) &optional)) tao:string-greater-or-equal))
;;; ------------------

;;; ------------------
;;; string-greaterp
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string-greaterp))
;;; ------------------

;;; ------------------
;;; string-left-trim
;;; ------------------
;;; (declaim (ftype (function (sequence (or (vector character) (vector nil) base-string symbol character)) (values string &optional)) string-left-trim))
;;; ------------------

;;; ------------------
;;; string-length
;;; ------------------
(declaim (ftype (function (t) *) tao:string-length))
;;; ------------------

;;; ------------------
;;; string-less-or-equal
;;; ------------------
(declaim (ftype (function (t t) (values (or (mod 4611686018427387901) (member t nil)) &optional)) tao:string-less-or-equal))
;;; ------------------

;;; ------------------
;;; string-lessp
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string-lessp))
;;; ------------------

;;; ------------------
;;; string-not-equal
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string-not-equal))
;;; ------------------

;;; ------------------
;;; string-not-greaterp
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string-not-greaterp))
;;; ------------------

;;; ------------------
;;; string-not-lessp
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string-not-lessp))
;;; ------------------

;;; ------------------
;;; string-replace
;;; ------------------
(declaim (ftype (function (t t &optional t t) (values sequence &optional)) tao:string-replace))
;;; ------------------

;;; ------------------
;;; string-reverse
;;; ------------------
(declaim (ftype (function (t) *) tao:string-reverse))
;;; ------------------

;;; ------------------
;;; string-reverse-search
;;; ------------------
(declaim (ftype (function (t t &optional t) *) tao:string-reverse-search))
;;; ------------------

;;; ------------------
;;; string-reverse-search-case
;;; ------------------
(declaim (ftype (function (t t &optional t) *) tao:string-reverse-search-case))
;;; ------------------

;;; ------------------
;;; string-right-trim
;;; ------------------
;;; (declaim (ftype (function (sequence (or (vector character) (vector nil) base-string symbol character)) (values string &optional)) string-right-trim))
;;; ------------------

;;; ------------------
;;; string-search
;;; ------------------
(declaim (ftype (function (t t &optional t) *) tao:string-search))
;;; ------------------

;;; ------------------
;;; string-search-case
;;; ------------------
(declaim (ftype (function (t t &optional t) *) tao:string-search-case))
;;; ------------------

;;; ------------------
;;; string-trim
;;; ------------------
(declaim (ftype (function (string string) (values string &optional)) tao:string-trim))
;;; ------------------

;;; ------------------
;;; string-upcase
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values simple-string &optional)) string-upcase))
;;; ------------------

;;; ------------------
;;; string/=
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string/=))
;;; ------------------

;;; ------------------
;;; string<
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string<))
;;; ------------------

;;; ------------------
;;; string<=
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string<=))
;;; ------------------

;;; ------------------
;;; string=
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (member t nil) &optional)) string=))
;;; ------------------

;;; ------------------
;;; string>
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string>))
;;; ------------------

;;; ------------------
;;; string>=
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string symbol character) (or (vector character) (vector nil) base-string symbol character) &key (:start1 (mod 4611686018427387901)) (:end1 (or null (mod 4611686018427387901))) (:start2 (mod 4611686018427387901)) (:end2 (or null (mod 4611686018427387901)))) (values (or (mod 4611686018427387901) null) &optional)) string>=))
;;; ------------------

;;; ------------------
;;; stringp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) stringp))
;;; ------------------

;;; ------------------
;;; sublis
;;; ------------------
;;; (declaim (ftype (function (list t &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values t &optional)) sublis))
;;; ------------------

;;; ------------------
;;; sublisq
;;; ------------------
(declaim (ftype (function (t t) (values t &optional)) tao:sublisq))
;;; ------------------

;;; ------------------
;;; sublisq-copy
;;; ------------------
(declaim (ftype (function (t t) (values t &optional)) tao:sublisq-copy))
;;; ------------------

;;; ------------------
;;; sublisql
;;; ------------------
(declaim (ftype (function (t t) (values t &optional)) tao:sublisql))
;;; ------------------

;;; ------------------
;;; subpackages
;;; ------------------
(declaim (ftype (function (&optional t) (values list &optional)) tao:subpackages))
;;; ------------------

;;; ------------------
;;; subseq
;;; ------------------
;;; (declaim (ftype (function (sequence (mod 4611686018427387901) &optional (or null (mod 4611686018427387901))) (values (or (simple-array * (*)) cons null sequence) &optional)) subseq))
;;; ------------------

;;; ------------------
;;; subset
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:subset))
;;; ------------------

;;; ------------------
;;; subset-not
;;; ------------------
(declaim (ftype (function (t t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:subset-not))
;;; ------------------

;;; ------------------
;;; subsetp
;;; ------------------
(declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values (member t nil) &optional)) tao:subsetp))
;;; ------------------

;;; ------------------
;;; subst
;;; ------------------
;;; (declaim (ftype (function (t t t &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values t &optional)) subst))
;;; ------------------

;;; ------------------
;;; subst-if
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) t &key (:key (or function symbol))) (values t &optional)) subst-if))
;;; ------------------

;;; ------------------
;;; subst-if-not
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) t &key (:key (or function symbol))) (values t &optional)) subst-if-not))
;;; ------------------

;;; ------------------
;;; substitute
;;; ------------------
;;; (declaim (ftype (function (t t sequence &rest t &key (:from-end t) (:test (or function symbol)) (:test-not (or function symbol)) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) substitute))
;;; ------------------

;;; ------------------
;;; substitute-if
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) substitute-if))
;;; ------------------

;;; ------------------
;;; substitute-if-not
;;; ------------------
;;; (declaim (ftype (function (t (or function symbol) sequence &rest t &key (:from-end t) (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901))) (:count (or null integer)) (:key (or function symbol))) (values (or (simple-array * (*)) cons null sequence) &optional)) substitute-if-not))
;;; ------------------

;;; ------------------
;;; substql
;;; ------------------
(declaim (ftype (function (t t t) (values t &optional)) tao:substql))
;;; ------------------

;;; ------------------
;;; substqu
;;; ------------------
(declaim (ftype (function (t t t) (values t &optional)) tao:substqu))
;;; ------------------

;;; ------------------
;;; substring
;;; ------------------
(declaim (ftype (function (t t &optional t) (values (or cons null sequence (simple-array * (*))) &optional)) tao:substring))
;;; ------------------

;;; ------------------
;;; subtypep
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) (or cons symbol sb-kernel:instance) &optional (or sb-kernel:lexenv null)) (values (member . #0=(t nil)) (member . #0#) &optional)) subtypep))
;;; ------------------

;;; ------------------
;;; svref
;;; ------------------
;;; (declaim (ftype (function (simple-vector (mod 4611686018427387901)) (values t &optional)) svref))
;;; ------------------

;;; ------------------
;;; sxhash
;;; ------------------
;;; (declaim (ftype (function (t) (values (unsigned-byte 62) &optional)) sxhash))
;;; ------------------

;;; ------------------
;;; symbol-function
;;; ------------------
;;; (declaim (ftype (function (symbol) (values function &optional)) symbol-function))
;;; ------------------

;;; ------------------
;;; symbol-name
;;; ------------------
;;; (declaim (ftype (function (symbol) (values simple-string &optional)) symbol-name))
;;; ------------------

;;; ------------------
;;; symbol-package
;;; ------------------
;;; (declaim (ftype (function (symbol) (values (or package null) &optional)) symbol-package))
;;; ------------------

;;; ------------------
;;; symbol-plist
;;; ------------------
;;; (declaim (ftype (function (symbol) (values list &optional)) symbol-plist))
;;; ------------------

;;; ------------------
;;; symbol-value
;;; ------------------
;;; (declaim (ftype (function (symbol) (values t &optional)) symbol-value))
;;; ------------------

;;; ------------------
;;; symbolp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) symbolp))
;;; ------------------

;;; ------------------
;;; symeval
;;; ------------------
(declaim (ftype (function (symbol) (values t &optional)) tao:symeval))
;;; ------------------

;;; ------------------
;;; synonym-stream-symbol
;;; ------------------
;;; (declaim (ftype (function (synonym-stream) (values symbol &optional)) synonym-stream-symbol))
;;; ------------------

;;; ------------------
;;; tailp
;;; ------------------
;;; (declaim (ftype (function (t list) (values (member t nil) &optional)) tailp))
;;; ------------------

;;; ------------------
;;; tan
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) tan))
;;; ------------------

;;; ------------------
;;; tanh
;;; ------------------
;;; (declaim (ftype (function (number) (values (or single-float double-float (complex single-float) (complex double-float)) &optional)) tanh))
;;; ------------------

;;; ------------------
;;; tenth
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) tenth))
;;; ------------------

;;; ------------------
;;; terpri
;;; ------------------
;;; (declaim (ftype (function (&optional (or stream (member nil t))) (values null &optional)) terpri))
;;; ------------------

;;; ------------------
;;; third
;;; ------------------
;;; (declaim (ftype (function (list) (values t &optional)) third))
;;; ------------------

;;; ------------------
;;; togap
;;; ------------------
(declaim (ftype (function (t) (values (member nil t) &optional)) tao:togap))
;;; ------------------

;;; ------------------
;;; translate-logical-pathname
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &key) (values pathname &optional)) translate-logical-pathname))
;;; ------------------

;;; ------------------
;;; translate-pathname
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) (or (vector character) (vector nil) base-string pathname file-stream) (or (vector character) (vector nil) base-string pathname file-stream) &key) (values pathname &optional)) translate-pathname))
;;; ------------------

;;; ------------------
;;; tree-equal
;;; ------------------
;;; (declaim (ftype (function (t t &key (:test (or function symbol)) (:test-not (or function symbol))) (values (member t nil) &optional)) tree-equal))
;;; ------------------

;;; ------------------
;;; truename
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream)) (values pathname &optional)) truename))
;;; ------------------

;;; ------------------
;;; truncate
;;; ------------------
;;; (declaim (ftype (function (real &optional real) (values integer real &optional)) truncate))
;;; ------------------

;;; ------------------
;;; two-way-stream-input-stream
;;; ------------------
;;; (declaim (ftype (function (two-way-stream) (values stream &optional)) two-way-stream-input-stream))
;;; ------------------

;;; ------------------
;;; two-way-stream-output-stream
;;; ------------------
;;; (declaim (ftype (function (two-way-stream) (values stream &optional)) two-way-stream-output-stream))
;;; ------------------

;;; ------------------
;;; type-error-datum
;;; ------------------
;;; (declaim (ftype (function (t) *) type-error-datum))
;;; ------------------

;;; ------------------
;;; type-error-expected-type
;;; ------------------
;;; (declaim (ftype (function (t) *) type-error-expected-type))
;;; ------------------

;;; ------------------
;;; type-of
;;; ------------------
;;; (declaim (ftype (function (t) (values t &optional)) type-of))
;;; ------------------

;;; ------------------
;;; typep
;;; ------------------
;;; (declaim (ftype (function (t (or cons symbol sb-kernel:instance) &optional (or sb-kernel:lexenv null)) (values t &optional)) typep))
;;; ------------------

;;; ------------------
;;; unbound-slot-instance
;;; ------------------
;;; (declaim (ftype (function (t) *) unbound-slot-instance))
;;; ------------------

;;; ------------------
;;; unexport
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) unexport))
;;; ------------------

;;; ------------------
;;; unintern
;;; ------------------
;;; (declaim (ftype (function (symbol &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t nil) &optional)) unintern))
;;; ------------------

;;; ------------------
;;; union
;;; ------------------
;;; (declaim (ftype (function (list list &key (:key (or function symbol)) (:test (or function symbol)) (:test-not (or function symbol))) (values list &optional)) union))
;;; ------------------

;;; ------------------
;;; unionq
;;; ------------------
(declaim (ftype (function (t &rest t) (values (or (simple-array * (*)) cons null sequence) &optional)) tao:unionq))
;;; ------------------

;;; ------------------
;;; unquote
;;; ------------------
(declaim (ftype (function (t) (values t &optional)) tao:unquote))
;;; ------------------

;;; ------------------
;;; unread-char
;;; ------------------
;;; (declaim (ftype (function (character &optional (or stream (member nil t))) (values t &optional)) unread-char))
;;; ------------------

;;; ------------------
;;; unuse-package
;;; ------------------
;;; (declaim (ftype (function ((or cons (vector character) (vector nil) base-string symbol character package) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) unuse-package))
;;; ------------------

;;; ------------------
;;; update-instance-for-different-class
;;; ------------------
;;; (declaim (ftype (function (t t &rest t) *) update-instance-for-different-class))
;;; ------------------

;;; ------------------
;;; update-instance-for-redefined-class
;;; ------------------
;;; (declaim (ftype (function (t t t t &rest t) *) update-instance-for-redefined-class))
;;; ------------------

;;; ------------------
;;; upgraded-array-element-type
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) &optional (or sb-kernel:lexenv null)) (values (or cons symbol sb-kernel:instance) &optional)) upgraded-array-element-type))
;;; ------------------

;;; ------------------
;;; upgraded-complex-part-type
;;; ------------------
;;; (declaim (ftype (function ((or cons symbol sb-kernel:instance) &optional (or sb-kernel:lexenv null)) (values (or cons symbol sb-kernel:instance) &optional)) upgraded-complex-part-type))
;;; ------------------

;;; ------------------
;;; upper-case-p
;;; ------------------
;;; (declaim (ftype (function (character) (values (member t nil) &optional)) upper-case-p))
;;; ------------------

;;; ------------------
;;; use-package
;;; ------------------
;;; (declaim (ftype (function ((or cons (vector character) (vector nil) base-string symbol character package) &optional (or (vector character) (vector nil) base-string symbol character package)) (values (member t) &optional)) use-package))
;;; ------------------

;;; ------------------
;;; use-value
;;; ------------------
;;; (declaim (ftype (function (t &optional (or condition null)) (values null &optional)) use-value))
;;; ------------------

;;; ------------------
;;; user-homedir-pathname
;;; ------------------
;;; (declaim (ftype (function (&optional t) (values pathname &optional)) user-homedir-pathname))
;;; ------------------

;;; ------------------
;;; value
;;; ------------------
(declaim (ftype (function (t) (values t &optional)) tao:value))
;;; ------------------

;;; ------------------
;;; values
;;; ------------------
;;; (declaim (ftype function values))
;;; ------------------

;;; ------------------
;;; values-list
;;; ------------------
;;; (declaim (ftype (function (list) *) values-list))
;;; ------------------

;;; ------------------
;;; vector
;;; ------------------
;;; (declaim (ftype (function * (values simple-vector &optional)) vector))
;;; ------------------

;;; ------------------
;;; vector-pop
;;; ------------------
;;; (declaim (ftype (function ((and vector (not simple-array))) (values t &optional)) vector-pop))
;;; ------------------

;;; ------------------
;;; vector-push
;;; ------------------
;;; (declaim (ftype (function (t (and vector (not simple-array))) (values (or (mod 4611686018427387901) null) &optional)) vector-push))
;;; ------------------

;;; ------------------
;;; vector-push-extend
;;; ------------------
;;; (declaim (ftype (function (t (and vector (not simple-array)) &optional (integer 1 4611686018427387900)) (values (mod 4611686018427387901) &optional)) vector-push-extend))
;;; ------------------

;;; ------------------
;;; vectorp
;;; ------------------
;;; (declaim (ftype (function (t) (values (member t nil) &optional)) vectorp))
;;; ------------------

;;; ------------------
;;; vprint
;;; ------------------
(declaim (ftype (function (t &optional t) (values t (mod 4611686018427387901) &optional)) tao:vprint))
;;; ------------------

;;; ------------------
;;; warn
;;; ------------------
;;; (declaim (ftype (function (t &rest t) (values null &optional)) warn))
;;; ------------------

;;; ------------------
;;; wild-pathname-p
;;; ------------------
;;; (declaim (ftype (function ((or (vector character) (vector nil) base-string pathname file-stream) &optional (member nil :host :device :directory :name :type :version)) (values t &optional)) wild-pathname-p))
;;; ------------------

;;; ------------------
;;; write
;;; ------------------
;;; (declaim (ftype (function (t &key (:stream (or stream (member nil t))) (:escape t) (:radix t) (:base (integer 2 36)) (:circle t) (:pretty t) (:level (or unsigned-byte null)) (:readably t) (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t) (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null)) (:miser-width (or unsigned-byte null)) (:pprint-dispatch t) (:suppress-errors t)) (values t &optional)) write))
;;; ------------------

;;; ------------------
;;; write-byte
;;; ------------------
;;; (declaim (ftype (function (integer stream) (values integer &optional)) write-byte))
;;; ------------------

;;; ------------------
;;; write-char
;;; ------------------
;;; (declaim (ftype (function (character &optional (or stream (member nil t))) (values character &optional)) write-char))
;;; ------------------

;;; ------------------
;;; write-line
;;; ------------------
;;; (declaim (ftype (function (string &optional (or stream (member nil t)) &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values string &optional)) write-line))
;;; ------------------

;;; ------------------
;;; write-sequence
;;; ------------------
;;; (declaim (ftype (function (sequence stream &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values sequence &optional)) write-sequence))
;;; ------------------

;;; ------------------
;;; write-string
;;; ------------------
;;; (declaim (ftype (function (string &optional (or stream (member nil t)) &key (:start (mod 4611686018427387901)) (:end (or null (mod 4611686018427387901)))) (values string &optional)) write-string))
;;; ------------------

;;; ------------------
;;; write-to-string
;;; ------------------
;;; (declaim (ftype (function (t &key (:escape t) (:radix t) (:base (integer 2 36)) (:readably t) (:circle t) (:pretty t) (:level (or unsigned-byte null)) (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t) (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null)) (:miser-width (or unsigned-byte null)) (:pprint-dispatch t)) (values simple-string &optional)) write-to-string))
;;; ------------------

;;; ------------------
;;; xcons
;;; ------------------
(declaim (ftype (function (t t) (values cons &optional)) tao:xcons))
;;; ------------------

;;; ------------------
;;; y-or-n-p
;;; ------------------
;;; (declaim (ftype (function (&optional string &rest t) (values (member t nil) &optional)) y-or-n-p))
;;; ------------------

;;; ------------------
;;; yes-or-no-p
;;; ------------------
;;; (declaim (ftype (function (&optional string &rest t) (values (member t nil) &optional)) yes-or-no-p))
;;; ------------------

;;; ------------------
;;; zerop
;;; ------------------
(declaim (ftype (function (t) (values (or (integer 0 0) null) &optional)) tao:zerop))
;;; ------------------

;;; ------------------
;;; \
;;; ------------------
(declaim (ftype (function (t t) (values number &optional)) tao:|\\|))
;;; ------------------
