(in-package :trophy)

;;;; APIs

(declaim
 (ftype (function (symbol) (values (or null dictionary) &optional))
        symbol-dictionary)
 (ftype (function (symbol) (values (integer 0 *) &optional)) symbol-times)
 (ftype (function (keyword &optional boolean)
         (values (or null dictionary) &optional))
        find-dictionary)
 (ftype (function (keyword) (values boolean &optional)) delete-dictionary)
 (ftype (function (keyword dictionary) (values dictionary &optional))
        add-dictionary)
 (ftype (function (keyword) (values list &optional)) dictionary-symbols)
 (ftype (function (dictionary) (values boolean &optional))
        dictionary-complete-p))

;;;; DICTIONARY

(defparameter *dictionaries* (make-hash-table :test #'eq))

(defstruct dictionary name table)

(defun symbol-dictionary (symbol) (get symbol 'dictionary))

(defun (setf symbol-dictionary) (dictionary symbol)
  (check-type dictionary dictionary)
  (setf (get symbol 'dictionary) dictionary))

(defun symbol-times (symbol)
  (let ((dictionary (symbol-dictionary symbol)))
    (if dictionary
        (values (gethash symbol (dictionary-table dictionary)))
        0)))

(defun find-dictionary (name &optional (errorp t))
  (or (gethash name *dictionaries*)
      (and errorp (error "Missing dictionary: ~S" name))))

(defun delete-dictionary (name) (remhash name *dictionaries*))

(defun add-dictionary (name dictionary)
  (setf (gethash name *dictionaries*) dictionary))

(defun dictionary-symbols (name)
  (loop :for symbol :being :each :hash-key :of
             (dictionary-table (find-dictionary name))
        :collect symbol))

(defun dictionary-complete-p (dictionary)
  (loop :for count :being :each :hash-value :of (dictionary-table dictionary)
        :never (zerop count)))

(defmacro defdictionary (name symbols)
  `(progn
    (add-dictionary ',name
                    (loop :for elt :in ',symbols
                          :with dict := (make-dictionary :name ',name)
                          :with ht := (make-hash-table :test #'eq)
                          :do (setf (gethash elt ht) 0
                                    (symbol-dictionary elt) dict)
                          :finally (setf (dictionary-table dict) ht)
                                   (return dict)))
    ',name))

(set-pprint-dispatch '(cons (member defdictionary))
                     (formatter
                      "~:<~^~W~^ ~1I~@_~@{~:/pprint-fill/~^ ~:_~}~:>"))

(defdictionary :evaluation-and-compile
  (lambda compile eval eval-when load-time-value quote compiler-macro-function
   define-compiler-macro defmacro macro-function macroexpand macroexpand-1
   define-symbol-macro symbol-macrolet *macroexpand-hook* proclaim declaim
   declare ignore ignorable dynamic-extent type inline notinline ftype
   declaration optimize special locally the special-operator-p constantp))

(defdictionary :types-and-classes
  (apply defun fdefinition fboundp fmakunbound flet labels macrolet funcall
   function function-lambda-expression functionp compiled-function-p
   call-arguments-limit lambda-list-keywords lambda-parameters-limit
   defconstant defparameter defvar destructuring-bind let let* progv setq psetq
   block catch go return-from return tagbody throw unwind-protect nil not t eq
   eql equal equalp identity complement constantly every some notevery notany
   and cond if or when unless case ccase ecase typecase ctypecase etypecase
   multiple-value-bind multiple-value-call multiple-value-list
   multiple-value-prog1 multiple-value-setq values values-list
   multiple-values-limit nth-value prog prog* prog1 prog2 progn
   define-modify-macro defsetf define-setf-expander get-setf-expansion setf
   psetf shiftf rotatef control-error program-error undefined-function))

(defdictionary :iteration (do do* dotimes dolist loop loop-finish))

(defdictionary :objects
  (function-keywords ensure-generic-function allocate-instance
   reinitialize-instance shared-initialize update-instance-for-different-class
   update-instance-for-redefined-class change-class slot-boundp slot-exists-p
   slot-makunbound slot-missing slot-unbound slot-value method-qualifiers
   no-applicable-method no-next-method remove-method make-instance
   make-instances-obsolete make-load-form make-load-form-saving-slots
   with-accessors with-slots defclass defgeneric defmethod find-class
   next-method-p call-method make-method call-next-method
   compute-applicable-methods define-method-combination find-method add-method
   initialize-instance class-name class-of unbound-slot unbound-slot-instance))

(defdictionary :structure (defstruct copy-structure))

(defdictionary :conditions
  (condition warning style-warning serious-condition error cell-error
   cell-error-name parse-error storage-condition assert error cerror check-type
   simple-error invalid-method-error method-combination-error signal
   simple-condition simple-condition-format-control
   simple-condition-format-arguments warn simple-warning invoke-debugger break
   *debugger-hook* *break-on-signals* handler-bind handler-case ignore-errors
   define-condition make-condition restart compute-restarts find-restart
   invoke-restart invoke-restart-interactively restart-bind restart-case
   restart-name with-condition-restarts with-simple-restart abort continue
   muffle-warning store-value use-value abort continue muffle-warning
   store-value use-value))

(defdictionary :packages
  (package export find-symbol find-package find-all-symbols import
   list-all-packages rename-package shadow shadowing-import delete-package
   make-package with-package-iterator unexport unintern in-package
   unuse-package use-package defpackage do-symbols do-external-symbols
   do-all-symbols intern package-name package-nicknames
   package-shadowing-symbols package-use-list package-used-by-list packagep
   *package* package-error package-error-package))

(defdictionary :numbers
  (number complex real float short-float single-float double-float long-float
   rational ratio integer signed-byte unsigned-byte mod bit fixnum bignum = /=
   < > <= >= max min minusp plusp zerop floor ffloor ceiling fceiling truncate
   ftruncate round fround sin cos tan asin acos atan pi sinh cosh tanh asinh
   acosh atanh * + - / 1+ 1- abs evenp oddp exp expt gcd incf decf lcm log mod
   rem signum sqrt isqrt random-state make-random-state random random-state-p
   *random-state* numberp cis complex complexp conjugate phase realpart
   imagpart upgraded-complex-part-type realp numerator denominator rational
   rationalize rationalp ash integer-length integerp parse-integer boole
   boole-1 boole-2 boole-and boole-andc1 boole-andc2 boole-c1 boole-c2
   boole-clr boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2
   boole-set boole-xor logand logandc1 logandc2 logeqv logior lognand lognor
   lognot logorc1 logorc2 logxor logbitp logcount logtest byte byte-size
   byte-position deposit-field dpb ldb ldb-test mask-field most-positive-fixnum
   most-negative-fixnum decode-float scale-float float-radix float-sign
   float-digits float-precision integer-decode-float float floatp
   most-positive-short-float least-positive-short-float
   least-positive-normalized-short-float most-positive-double-float
   least-positive-double-float least-positive-normalized-double-float
   most-positive-long-float least-positive-long-float
   least-positive-normalized-long-float most-positive-single-float
   least-positive-single-float least-positive-normalized-single-float
   most-negative-short-float least-negative-short-float
   least-negative-normalized-short-float most-negative-single-float
   least-negative-single-float least-negative-normalized-single-float
   most-negative-double-float least-negative-double-float
   least-negative-normalized-double-float most-negative-long-float
   least-negative-long-float least-negative-normalized-long-float
   short-float-epsilon short-float-negative-epsilon single-float-epsilon
   single-float-negative-epsilon double-float-epsilon
   double-float-negative-epsilon long-float-epsilon long-float-negative-epsilon
   arithmetic-error arithmetic-error-operands arithmetic-error-operation
   division-by-zero floating-point-invalid-operation floating-point-inexact
   floating-point-overflow floating-point-underflow))

(defdictionary :characters
  (character base-char standard-char extended-char char= char/= char< char>
   char<= char>= char-equal char-not-equal char-lessp char-greaterp
   char-not-greaterp char-not-lessp character characterp alpha-char-p
   alphanumericp digit-char digit-char-p graphic-char-p standard-char-p
   char-upcase char-downcase upper-case-p lower-case-p both-case-p char-code
   char-int code-char char-code-limit char-name name-char))

(defdictionary :conses
  (list null cons atom cons consp atom rplaca rplacd car cdr caar cadr cdar
   cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar
   caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr
   cdddar cddddr copy-tree sublis nsublis subst subst-if subst-if-not nsubst
   nsubst-if nsubst-if-not tree-equal copy-list list list* list-length listp
   make-list push pop first second third fourth fifth sixth seventh eighth
   ninth tenth nth endp null nconc append revappend nreconc butlast nbutlast
   last ldiff tailp nthcdr rest member member-if member-if-not mapc mapcar
   mapcan mapl maplist mapcon acons assoc assoc-if assoc-if-not copy-alist
   pairlis rassoc rassoc-if rassoc-if-not get-properties getf remf intersection
   nintersection adjoin pushnew set-difference nset-difference set-exclusive-or
   nset-exclusive-or subsetp union nunion))

(defdictionary :arrays
  (array simple-array vector simple-vector bit-vector simple-bit-vector
   make-array adjust-array adjustable-array-p aref array-dimension
   array-dimensions array-element-type array-has-fill-pointer-p
   array-displacement array-in-bounds-p array-rank array-row-major-index
   array-total-size arrayp fill-pointer row-major-aref
   upgraded-array-element-type array-dimension-limit array-rank-limit
   array-total-size-limit simple-vector-p svref vector vector-pop vector-push
   vector-push-extend vectorp bit sbit bit-and bit-andc1 bit-andc2 bit-eqv
   bit-ior bit-nand bit-nor bit-not bit-orc1 bit-orc2 bit-xor bit-vector-p
   simple-bit-vector-p))

(defdictionary :strings
  (string base-string simple-string simple-base-string simple-string-p char
   schar string string-upcase string-downcase string-capitalize nstring-upcase
   nstring-downcase nstring-capitalize string-trim string-left-trim
   string-right-trim string= string/= string< string> string<= string>=
   string-equal string-not-equal string-lessp string-greaterp
   string-not-greaterp string-not-lessp stringp make-string))

(defdictionary :sequences
  (sequence copy-seq elt fill make-sequence subseq map map-into reduce count
   count-if count-if-not length reverse nreverse sort stable-sort find find-if
   find-if-not position position-if position-if-not search mismatch replace
   substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
   nsubstitute-if-not concatenate merge remove remove-if remove-if-not delete
   delete-if delete-if-not remove-duplicates delete-duplicates))

(defdictionary :hash-tables
  (hash-table make-hash-table hash-table-p hash-table-count
   hash-table-rehash-size hash-table-rehash-threshold hash-table-size
   hash-table-test gethash remhash maphash with-hash-table-iterator clrhash
   sxhash))

(defdictionary :pathnames
  (pathname logical-pathname pathname make-pathname pathnamep pathname-host
   pathname-device pathname-directory pathname-name pathname-type
   pathname-version load-logical-pathname-translations
   logical-pathname-translations logical-pathname *default-pathname-defaults*
   namestring file-namestring directory-namestring host-namestring
   enough-namestring parse-namestring wild-pathname-p pathname-match-p
   translate-logical-pathname translate-pathname merge-pathnames))

(defdictionary :files
  (directory probe-file ensure-directories-exist truename file-author
   file-write-date rename-file delete-file file-error file-error-pathname))

(defdictionary :streams
  (stream broadcast-stream concatenated-stream echo-stream file-stream
   string-stream synonym-stream two-way-stream input-stream-p output-stream-p
   interactive-stream-p open-stream-p stream-element-type streamp read-byte
   write-byte peek-char read-char read-char-no-hang terpri fresh-line
   unread-char write-char read-line write-string write-line read-sequence
   write-sequence file-length file-position file-string-length open
   stream-external-format with-open-file close with-open-stream listen
   clear-input finish-output force-output clear-output y-or-n-p yes-or-no-p
   make-synonym-stream synonym-stream-symbol broadcast-stream-streams
   make-broadcast-stream make-two-way-stream two-way-stream-input-stream
   two-way-stream-output-stream echo-stream-input-stream
   echo-stream-output-stream make-echo-stream concatenated-stream-streams
   make-concatenated-stream get-output-stream-string make-string-input-stream
   make-string-output-stream with-input-from-string with-output-to-string
   *debug-io* *error-output* *query-io* *standard-input* *standard-output*
   *trace-output* *terminal-io* stream-error stream-error-stream end-of-file))

(defdictionary :printer
  (copy-pprint-dispatch formatter pprint-dispatch pprint-exit-if-list-exhausted
   pprint-fill pprint-linear pprint-tabular pprint-indent pprint-logical-block
   pprint-newline pprint-pop pprint-tab print-object print-unreadable-object
   set-pprint-dispatch write prin1 print pprint princ write-to-string
   prin1-to-string princ-to-string *print-array* *print-base* *print-radix*
   *print-case* *print-circle* *print-escape* *print-gensym* *print-level*
   *print-length* *print-lines* *print-miser-width* *print-pprint-dispatch*
   *print-pretty* *print-readably* *print-right-margin* print-not-readable
   print-not-readable-object format))

(defdictionary :reader
  (readtable copy-readtable make-dispatch-macro-character read
   read-preserving-whitespace read-delimited-list read-from-string
   readtable-case readtablep set-dispatch-macro-character
   get-dispatch-macro-character set-macro-character get-macro-character
   set-syntax-from-char with-standard-io-syntax *read-base*
   *read-default-float-format* *read-eval* *read-suppress* *readtable*
   reader-error))

(defdictionary :system-constructions
  (compile-file compile-file-pathname load with-compilation-unit *features*
   *compile-file-pathname* *compile-file-truename* *load-pathname*
   *load-truename* *compile-print* *compile-verbose* *load-print*
   *load-verbose* *modules* provide require))

(defdictionary :environment
  (decode-universal-time encode-universal-time get-universal-time
   get-decoded-time sleep apropos apropos-list describe describe-object trace
   untrace step time internal-time-units-per-second get-internal-real-time
   get-internal-run-time disassemble documentation room ed inspect dribble - +
   ++ +++ * ** *** / // /// lisp-implementation-type
   lisp-implementation-version short-site-name long-site-name machine-instance
   machine-type machine-version software-type software-version
   user-homedir-pathname))
