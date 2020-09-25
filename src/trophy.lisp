(in-package :cl-user)

(defpackage :trophy
  (:use :cl)
  (:export))

(in-package :trophy)

;;;; *TROPHY-OUTPUT*

(defvar *trophy-output* *standard-output*)

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
        (gethash symbol (dictionary-table dictionary))
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
                    (loop :for elt :in ,symbols
                          :with dict := (make-dictionary :name ',name)
                          :with ht := (make-hash-table :test #'eq)
                          :do (setf (gethash elt ht) 0
                                    (symbol-dictionary elt) dict)
                          :finally (setf (dictionary-table dict) ht)
                                   (return dict)))
    ',name))

(set-pprint-dispatch '(cons (member defdictionary))
                     (formatter "~:<~^~W~^ ~1I~@_~@{~W~^ ~:_~}~:>"))

(defdictionary :evaluation-and-compile
  '(lambda compile
     eval
     eval-when
     load-time-value
     quote
     compiler-macro-function
     define-compiler-macro
     defmacro
     macro-function
     macroexpand
     macroexpand-1
     define-symbol-macro
     symbol-macrolet
     *macroexpand-hook*
     proclaim
     declaim
     declare
     ignore
     ignorable
     dynamic-extent
     type
     inline
     notinline
     ftype
     declaration
     optimize
     special
     locally
     the
     special-operator-p
     constantp))

(defdictionary :types-and-classes
  '(apply defun fdefinition fboundp fmakunbound flet labels macrolet funcall
          function function-lambda-expression functionp compiled-function-p
          call-arguments-limit lambda-list-keywords lambda-parameters-limit
          defconstant defparameter defvar destructuring-bind let let* progv
          setq psetq block catch go return-from return tagbody throw
          unwind-protect nil not t eq eql equal equalp identity complement
          constantly every some notevery notany and cond if or when unless case
          ccase ecase typecase ctypecase etypecase multiple-value-bind
          multiple-value-call multiple-value-list multiple-value-prog1
          multiple-value-setq values values-list multiple-values-limit
          nth-value prog prog* prog1 prog2 progn define-modify-macro defsetf
          define-setf-expander get-setf-expansion setf psetf shiftf rotatef
          control-error program-error undefined-function))

(defdictionary :iteration '(do do* dotimes dolist loop loop-finish))

(defdictionary :objects
  '(function-keywords ensure-generic-function allocate-instance
                      reinitialize-instance shared-initialize
                      update-instance-for-different-class
                      update-instance-for-redefined-class change-class
                      slot-boundp slot-exists-p slot-makunbound slot-missing
                      slot-unbound slot-value method-qualifiers
                      no-applicable-method no-next-method remove-method
                      make-instance make-instances-obsolete make-load-form
                      make-load-form-saving-slots with-accessors with-slots
                      defclass defgeneric defmethod find-class next-method-p
                      call-method make-method call-next-method
                      compute-applicable-methods define-method-combination
                      find-method add-method initialize-instance class-name
                      class-of unbound-slot unbound-slot-instance))

(defdictionary :structure '(defstruct copy-structure))

(defdictionary :conditions
  '(condition warning style-warning serious-condition error cell-error
    cell-error-name parse-error storage-condition assert error cerror
    check-type simple-error invalid-method-error method-combination-error
    signal simple-condition simple-condition-format-control
    simple-condition-format-arguments warn simple-warning invoke-debugger break
    *debugger-hook* *break-on-signals* handler-bind handler-case ignore-errors
    define-condition make-condition restart compute-restarts find-restart
    invoke-restart invoke-restart-interactively restart-bind restart-case
    restart-name with-condition-restarts with-simple-restart abort continue
    muffle-warning store-value use-value abort continue muffle-warning
    store-value use-value))

(defdictionary :packages
  '(package export find-symbol find-package find-all-symbols import
    list-all-packages rename-package shadow shadowing-import delete-package
    make-package with-package-iterator unexport unintern in-package
    unuse-package use-package defpackage do-symbols do-external-symbols
    do-all-symbols intern package-name package-nicknames
    package-shadowing-symbols package-use-list package-used-by-list packagep
    *package* package-error package-error-package))

(defdictionary :numbers
  '(number complex real float short-float single-float double-float long-float
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
    byte-position deposit-field dpb ldb ldb-test mask-field
    most-positive-fixnum most-negative-fixnum decode-float scale-float
    float-radix float-sign float-digits float-precision integer-decode-float
    float floatp most-positive-short-float least-positive-short-float
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
    double-float-negative-epsilon long-float-epsilon
    long-float-negative-epsilon arithmetic-error arithmetic-error-operands
    arithmetic-error-operation division-by-zero
    floating-point-invalid-operation floating-point-inexact
    floating-point-overflow floating-point-underflow))

(defdictionary :characters
  '(character base-char standard-char extended-char char= char/= char< char>
              char<= char>= char-equal char-not-equal char-lessp char-greaterp
              char-not-greaterp char-not-lessp character characterp
              alpha-char-p alphanumericp digit-char digit-char-p graphic-char-p
              standard-char-p char-upcase char-downcase upper-case-p
              lower-case-p both-case-p char-code char-int code-char
              char-code-limit char-name name-char))

(defdictionary :conses
  '(list null cons atom cons consp atom rplaca rplacd car cdr caar cadr cdar
         cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr
         caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
         cddaar cddadr cdddar cddddr copy-tree sublis nsublis subst subst-if
         subst-if-not nsubst nsubst-if nsubst-if-not tree-equal copy-list list
         list* list-length listp make-list push pop first second third fourth
         fifth sixth seventh eighth ninth tenth nth endp null nconc append
         revappend nreconc butlast nbutlast last ldiff tailp nthcdr rest member
         member-if member-if-not mapc mapcar mapcan mapl maplist mapcon acons
         assoc assoc-if assoc-if-not copy-alist pairlis rassoc rassoc-if
         rassoc-if-not get-properties getf remf intersection nintersection
         adjoin pushnew set-difference nset-difference set-exclusive-or
         nset-exclusive-or subsetp union nunion))

(defdictionary :arrays
  '(array simple-array vector simple-vector bit-vector simple-bit-vector
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
  '(string base-string simple-string simple-base-string simple-string-p char
           schar string string-upcase string-downcase string-capitalize
           nstring-upcase nstring-downcase nstring-capitalize string-trim
           string-left-trim string-right-trim string= string/= string< string>
           string<= string>= string-equal string-not-equal string-lessp
           string-greaterp string-not-greaterp string-not-lessp stringp
           make-string))

(defdictionary :sequences
  '(sequence copy-seq elt fill make-sequence subseq map map-into reduce count
    count-if count-if-not length reverse nreverse sort stable-sort find find-if
    find-if-not position position-if position-if-not search mismatch replace
    substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
    nsubstitute-if-not concatenate merge remove remove-if remove-if-not delete
    delete-if delete-if-not remove-duplicates delete-duplicates))

(defdictionary :hash-tables
  '(hash-table make-hash-table hash-table-p hash-table-count
    hash-table-rehash-size hash-table-rehash-threshold hash-table-size
    hash-table-test gethash remhash maphash with-hash-table-iterator clrhash
    sxhash))

(defdictionary :pathnames
  '(pathname logical-pathname pathname make-pathname pathnamep pathname-host
             pathname-device pathname-directory pathname-name pathname-type
             pathname-version load-logical-pathname-translations
             logical-pathname-translations logical-pathname
             *default-pathname-defaults* namestring file-namestring
             directory-namestring host-namestring enough-namestring
             parse-namestring wild-pathname-p pathname-match-p
             translate-logical-pathname translate-pathname merge-pathnames))

(defdictionary :files
  '(directory probe-file ensure-directories-exist truename file-author
              file-write-date rename-file delete-file file-error
              file-error-pathname))

(defdictionary :streams
  '(stream broadcast-stream concatenated-stream echo-stream file-stream
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
  '(copy-pprint-dispatch formatter pprint-dispatch
                         pprint-exit-if-list-exhausted pprint-fill
                         pprint-linear pprint-tabular pprint-indent
                         pprint-logical-block pprint-newline pprint-pop
                         pprint-tab print-object print-unreadable-object
                         set-pprint-dispatch write prin1 print pprint princ
                         write-to-string prin1-to-string princ-to-string
                         *print-array* *print-base* *print-radix* *print-case*
                         *print-circle* *print-escape* *print-gensym*
                         *print-level* *print-length* *print-lines*
                         *print-miser-width* *print-pprint-dispatch*
                         *print-pretty* *print-readably* *print-right-margin*
                         print-not-readable print-not-readable-object format))

(defdictionary :reader
  '(readtable copy-readtable make-dispatch-macro-character read
    read-preserving-whitespace read-delimited-list read-from-string
    readtable-case readtablep set-dispatch-macro-character
    get-dispatch-macro-character set-macro-character get-macro-character
    set-syntax-from-char with-standard-io-syntax *read-base*
    *read-default-float-format* *read-eval* *read-suppress* *readtable*
    reader-error))

(defdictionary :system-constructions
  '(compile-file compile-file-pathname load with-compilation-unit *features*
                 *compile-file-pathname* *compile-file-truename*
                 *load-pathname* *load-truename* *compile-print*
                 *compile-verbose* *load-print* *load-verbose* *modules*
                 provide require))

(defdictionary :environment
  '(decode-universal-time encode-universal-time get-universal-time
                          get-decoded-time sleep apropos apropos-list describe
                          describe-object trace untrace step time
                          internal-time-units-per-second get-internal-real-time
                          get-internal-run-time disassemble documentation room
                          ed inspect dribble - + ++ +++ * ** *** / // ///
                          lisp-implementation-type lisp-implementation-version
                          short-site-name long-site-name machine-instance
                          machine-type machine-version software-type
                          software-version user-homedir-pathname))

;;;; ACHIEVEMENT

(unless (boundp '+users-directory+)
  (defconstant +users-directory+
    (ensure-directories-exist
      (merge-pathnames "users/"
                       (asdf:system-source-directory
                         (asdf:find-system :trophy))))))

(defparameter *achievements* (make-hash-table :test #'eq))

(defstruct achievement name message completed?)

(defun symbol-achievements (symbol) (get symbol 'achievement))

(defun (setf symbol-achievements) (achievement symbol)
  (check-type achievement achievement)
  (let ((achievements
         (remove (achievement-name achievement) (symbol-achievements symbol)
                 :key #'achievement-name
                 :test #'eq)))
    (setf (get symbol 'achievement) (cons achievement achievements)))
  achievement)

;;;; READABLE PRINTERS

(defun print-readable-hash-table (stream exp)
  (write
    `(setf ,@(loop :for k :being :each :hash-key :of exp :using (:hash-value v)
                   :collect `(gethash ',k *achievements*)
                   :collect v))
    :stream stream))

(defun kv-args (object)
  (loop :for slot :in (c2mop:class-slots (class-of object))
        :for name := (c2mop:slot-definition-name slot)
        :for value := (slot-value object name)
        :collect (intern (string name) :keyword)
        :if (or (symbolp value) (listp value))
          :collect `',value
        :else
          :collect value))

(defun print-readable-complete-dictionary (stream exp)
  (write
    `(let ((achievement
            (,(intern (format nil "MAKE-~A" (type-of exp))) ,@(kv-args exp))))
       (loop :for symbol :being :each :hash-key :of
                  (dictionary-table (find-dictionary ',(achievement-name exp)))
             :do (setf (symbol-achievements symbol) achievement))
       achievement)
    :stream stream))

(defun print-readable-complete-ranking (stream exp)
  (write
    `(let ((achievement
            (,(intern (format nil "MAKE-~A" (type-of exp))) ,@(kv-args exp))))
       (loop :for symbol :in ',(complete-ranking-symbols exp)
             :do (setf (symbol-achievements symbol) achievement))
       achievement)
    :stream stream))

(defun print-readable-first-time (stream exp)
  (write
    `(let ((achievement (make-first-time ,@(kv-args exp))))
       (setf (symbol-achievements ',(achievement-name exp)) achievement))
    :stream stream))

(defun print-readable-times (stream exp)
  (write
    `(let ((achievement (make-times ,@(kv-args exp))))
       (setf (symbol-achievements ',(times-symbol exp)) achievement))
    :stream stream))

(defun print-readable-dictionary (stream exp)
  (write
    `(let ((ht (make-hash-table :test #'eq)))
       (setf ,@(loop :for k :being :each :hash-key :of
                          (dictionary-table exp) :using (:hash-value v)
                     :collect `(gethash ',k ht)
                     :collect v))
       (setf (gethash ',(dictionary-name exp) *dictionaries*)
               (make-dictionary :name ',(dictionary-name exp) :table ht)))
    :stream stream))

(defun print-readable-dispatch ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'hash-table 'print-readable-hash-table)
    (set-pprint-dispatch 'complete-dictionary
                         'print-readable-complete-dictionary)
    (set-pprint-dispatch 'complete-ranking 'print-readable-complete-ranking)
    (set-pprint-dispatch 'first-time 'print-readable-first-time)
    (set-pprint-dispatch 'dictionary 'print-readable-dictionary)
    (set-pprint-dispatch 'times 'print-readable-times)
    *print-pprint-dispatch*))

(defun debug-printer ()
  (let ((*print-pprint-dispatch* (print-readable-dispatch)))
    (print `(in-package :trophy))
    (print *achievements*)))

(defun save (user-name)
  (with-open-file (*standard-output* (ensure-directories-exist
                                       (merge-pathnames
                                         (string-downcase user-name)
                                         +users-directory+))
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (debug-printer)))

(defun load-user (user-name)
  (load (merge-pathnames (string-downcase user-name) +users-directory+)))

;;;; COMPLETE-RANKING

(defstruct (complete-ranking (:include achievement)) symbols)

(macrolet ((deftrophy (name message list)
             `(let* ((list ,list)
                     (achievement
                      (setf (gethash ',name *achievements*)
                              (make-complete-ranking :name ',name
                                                     :message ,message
                                                     :symbols list))))
                (loop :for symbol :in ,list
                      :do (setf (symbol-achievements symbol) achievement)))))
  (deftrophy :symbol-usage-level1 "シンボルの使用頻度ランキングトップ10のシンボルをコンプリートしました。"
   '(quote setf defun type t let function stream if *))
  (deftrophy :symbol-usage-level2 "シンボルの使用頻度ランキングトップ10〜20のシンボルをコンプリートしました。"
   '(defmethod when declare list string or and loop length =))
  (deftrophy :symbol-usage-level3 "シンボルの使用頻度ランキングトップ20〜30のシンボルをコンプリートしました。"
   '(lambda format
      &key
      signed-byte
      aref
      +
      double-float
      simple-array
      values
      in-package))
  (deftrophy :symbol-usage-level4 "シンボルの使用頻度ランキングトップ30〜40のシンボルをコンプリートしました。"
   '(- &optional error cond &rest let* unless not defgeneric null))
  (deftrophy :symbol-usage-level5 "シンボルの使用頻度ランキングトップ40〜50のシンボルをコンプリートしました。"
   '(ignore do funcall defmacro car eq defconstant eql cons first))
  (deftrophy :symbol-usage-level6 "シンボルの使用頻度ランキングトップ50〜60のシンボルをコンプリートしました。"
   '(cdr gethash single-float defclass make-instance defvar progn
         multiple-value-bind push mapcar))
  (deftrophy :symbol-usage-level7 "シンボルの使用頻度ランキングトップ60〜70のシンボルをコンプリートしました。"
   '(export symbol inline < > apply / &body char setq))
  (deftrophy :symbol-usage-level8 "シンボルの使用頻度ランキングトップ70〜80のシンボルをコンプリートしました。"
   '(return slot-value incf vector class fixnum count the assert rest))
  (deftrophy :symbol-usage-level9 "シンボルの使用頻度ランキングトップ80〜90のシンボルをコンプリートしました。"
   '(complex 1+ make-array declaim equal ldb 1- sequence defparameter second))
  (deftrophy :symbol-usage-level10 "シンボルの使用頻度ランキングトップ90〜100のシンボルをコンプリートしました。"
   '(integer unsigned-byte member dolist condition array zerop typep <= getf))
  (deftrophy :symbol-usage-level11 "シンボルの使用頻度ランキングトップ100〜110のシンボルをコンプリートしました。"
   '(position defpackage with-slots max optimize subseq gensym append speed
              return-from check-type))
  (deftrophy :symbol-usage-level12 "シンボルの使用頻度ランキングトップ110〜120のシンボルをコンプリートしました。"
   '(elt float flet number byte &allow-other-keys string= min pathname
         define-condition destructuring-bind))
  (deftrophy :symbol-usage-level13 "シンボルの使用頻度ランキングトップ120〜130のシンボルをコンプリートしました。"
   '(map case go safety make-hash-table labels coerce >= method package))
  (deftrophy :symbol-usage-level14 "シンボルの使用頻度ランキングトップ130〜140のシンボルをコンプリートしました。"
   '(char= dotimes find call-next-method symbol-name listp ecase tagbody assoc
           debug))
  (deftrophy :symbol-usage-level15 "シンボルの使用頻度ランキングトップ140〜150のシンボルをコンプリートしました。"
   '(defstruct character
      concatenate
      consp
      eval-when
      etypecase
      ignorable
      logxor
      ash
      write-string))
  (deftrophy :symbol-usage-level16 "シンボルの使用頻度ランキングトップ150〜160のシンボルをコンプリートしました。"
   '(svref boolean symbolp set intern abs nreverse expt time plusp))
  (deftrophy :symbol-usage-level17 "シンボルの使用頻度ランキングトップ160〜170のシンボルをコンプリートしました。"
   '(pop deftype print-object documentation cadr stringp mod identity nth
         cddr))
  (deftrophy :symbol-usage-level18 "シンボルの使用頻度ランキングトップ170〜180のシンボルをコンプリートしました。"
   '(logand write-char list* reduce equalp last handler-case class-name decf
            real floor /=))
  (deftrophy :symbol-usage-level19 "シンボルの使用頻度ランキングトップ180〜190のシンボルをコンプリートしました。"
   '(typecase princ
      print-unreadable-object
      unwind-protect
      multiple-value-list
      char-code
      string-equal
      every
      initialize-instance
      otherwise))
  (deftrophy :symbol-usage-level20 "シンボルの使用頻度ランキングトップ190〜200のシンボルをコンプリートしました。"
   '(block keyword
      warn
      prog1
      reverse
      parse-integer
      logior
      macrolet
      *standard-output*
      with-output-to-string))
  (deftrophy :symbol-usage-level21 "シンボルの使用頻度ランキングトップ200〜210のシンボルをコンプリートしました。"
   '(find-package ftype remove dynamic-extent with-open-file log variable sort
                  find-class hash-table code-char))
  (deftrophy :symbol-usage-level22 "シンボルの使用頻度ランキングトップ210〜220のシンボルをコンプリートしました。"
   '(third atom *package* string-upcase class-of string-downcase slot-boundp
           pushnew truncate step))
  (deftrophy :symbol-usage-level23 "シンボルの使用頻度ランキングトップ220〜230のシンボルをコンプリートしました。"
   '(read simple-string round read-char merge-pathnames simple-vector random
          &aux replace directory get))
  (deftrophy :symbol-usage-level24 "シンボルの使用頻度ランキングトップ230〜240のシンボルをコンプリートしました。"
   '(numberp namestring space nconc eval type-of ignore-errors sqrt remove-if
             exp minusp))
  (deftrophy :symbol-usage-level25 "シンボルの使用頻度ランキングトップ240〜250のシンボルをコンプリートしました。"
   '(array-dimension package-name with-accessors locally vector-push-extend
                     princ-to-string mapc terpri &whole integerp subtypep
                     write-byte make-pathname sin))
  (deftrophy :symbol-usage-level26 "シンボルの使用頻度ランキングトップ250〜260のシンボルをコンプリートしました。"
   '(define-compiler-macro search nth-value
      remove-if-not
      pi
      special
      keywordp
      symbol-package
      read-byte
      fill-pointer
      write-sequence
      remhash))
  (deftrophy :symbol-usage-level27 "シンボルの使用頻度ランキングトップ260〜270のシンボルをコンプリートしました。"
   '(bit cos copy-list caar handler-bind &environment get-universal-time some
         constantp find-symbol close))
  (deftrophy :symbol-usage-level28 "シンボルの使用頻度ランキングトップ270〜280のシンボルをコンプリートしました。"
   '(copy-seq logbitp probe-file realpart find-if file-position
              multiple-value-setq endp make-string evenp))
  (deftrophy :symbol-usage-level29 "シンボルの使用頻度ランキングトップ280〜290のシンボルをコンプリートしました。"
   '(maphash *readtable* ceiling schar read-from-string read-sequence
             symbol-macrolet imagpart rotatef restart-case))
  (deftrophy :symbol-usage-level30 "シンボルの使用頻度ランキングトップ290〜300のシンボルをコンプリートしました。"
   '(mapcan phase rational read-line peek-char finish-output *error-output*
            boundp array-dimensions readtable *features* fboundp))
  (deftrophy :symbol-usage-level31 "シンボルの使用頻度ランキングトップ300〜310のシンボルをコンプリートしました。"
   '(symbol-value remove-duplicates hash-table-count array-element-type delete
                  values-list constantly write most-positive-fixnum union
                  short-float functionp))
  (deftrophy :symbol-usage-level32 "シンボルの使用頻度ランキングトップ310〜320のシンボルをコンプリートしました。"
   '(prog throw
     position-if
     digit-char-p
     fill
     do*
     butlast
     load-time-value
     array-total-size
     set-difference
     nthcdr
     make-list
     restart
     characterp))
  (deftrophy :symbol-usage-level33 "シンボルの使用頻度ランキングトップ320〜330のシンボルをコンプリートしました。"
   '(long-float pathname-directory intersection multiple-value-call mismatch
     string-trim row-major-aref pathname-type fdefinition open make-condition
     force-output))
  (deftrophy :symbol-usage-level34 "シンボルの使用頻度ランキングトップ330〜340のシンボルをコンプリートしました。"
   '(rem symbol-function pathname-name complement abort multiple-value-prog1
         char-equal remf string< signal *print-pretty* continue standard-class
         cdar caddr))
  (deftrophy :symbol-usage-level35 "シンボルの使用頻度ランキングトップ340〜350のシンボルをコンプリートしました。"
   '(arrayp catch reinitialize-instance sleep print compilation-speed cerror
            warning get-internal-real-time substitute notinline
            get-output-stream-string bignum))
  (deftrophy :symbol-usage-level36 "シンボルの使用頻度ランキングトップ350〜360のシンボルをコンプリートしました。"
   '(*trace-output* write-line char<= fresh-line standard-object change-class
     ratio subst ensure-directories-exist clrhash simple-error
     with-input-from-string satisfies))
  (deftrophy :symbol-usage-level37 "シンボルの使用頻度ランキングトップ360〜370のシンボルをコンプリートしました。"
   '(fourth prin1-to-string oddp end-of-file invoke-restart *query-io*
            *debug-io* set-macro-character shared-initialize dpb vectorp
            count-if compile truename make-load-form make-string-output-stream
            *standard-input*))
  (deftrophy :symbol-usage-level38 "シンボルの使用頻度ランキングトップ370〜380のシンボルをコンプリートしました。"
   '(integer-length byte-size declaration macro-function defsetf
                    double-float-epsilon copy-readtable denominator map-into
                    define-modify-macro write-to-string formatter type-error
                    decode-universal-time with-standard-io-syntax char/=
                    internal-time-units-per-second file-length))
  (deftrophy :symbol-usage-level39 "シンボルの使用頻度ランキングトップ380〜390のシンボルをコンプリートしました。"
   '(numerator rplacd prin1 unread-char char-upcase position-if-not
               adjust-array pprint-logical-block upper-case-p base-char
               set-dispatch-macro-character))
  (deftrophy :symbol-usage-level40 "シンボルの使用頻度ランキングトップ390〜400のシンボルをコンプリートしました。"
   '(alphanumericp bit-vector *print-readably* realp signum simple-vector-p
                   loop-finish file-write-date logtest file-namestring
                   readtable-case make-symbol structure alpha-char-p atan
                   *default-pathname-defaults* char-downcase progv
                   macroexpand-1))
  (deftrophy :symbol-usage-level41 "シンボルの使用頻度ランキングトップ400〜410のシンボルをコンプリートしました。"
   '(delete-file define-symbol-macro pathnamep encode-universal-time
                 with-simple-restart break describe cdddr muffle-warning cadar
                 string/= generic-function streamp find-restart
                 most-positive-double-float *random-state* find-if-not trace
                 lambda-list-keywords *read-default-float-format*
                 *print-length* user-homedir-pathname macroexpand
                 slot-makunbound))
  (deftrophy :symbol-usage-level42 "シンボルの使用頻度ランキングトップ410〜420のシンボルをコンプリートしました。"
   '(load acons *print-level* *read-eval* make-broadcast-stream
          stream-element-type fmakunbound shiftf get-setf-expansion tan
          get-macro-character import lower-case-p merge substitute-if
          stable-sort lognot read-delimited-list fifth string-right-trim
          array-rank notany *print-circle* style-warning))
  (deftrophy :symbol-usage-level43 "シンボルの使用頻度ランキングトップ420〜430のシンボルをコンプリートしました。"
   '(pprint-newline vector-push *print-escape* hash-table-p sbit
                    simple-bit-vector delete-duplicates open-stream-p
                    parse-error inspect delete-if *terminal-io* use-value
                    adjoin get-internal-run-time hash-table-test
                    structure-object make-string-input-stream logandc2
                    make-load-form-saving-slots with-open-stream conjugate
                    directory-namestring *read-suppress*))
  (deftrophy :symbol-usage-level44 "シンボルの使用頻度ランキングトップ430〜440のシンボルをコンプリートしました。"
   '(compiler-macro-function rplaca string-capitalize string-lessp count-if-not
                             string-left-trim require *print-case*
                             describe-object next-method-p simple-condition
                             set-pprint-dispatch proclaim simple-base-string
                             do-external-symbols file-stream make-sequence
                             rassoc make-random-state pprint
                             most-negative-double-float
                             array-has-fill-pointer-p sxhash serious-condition
                             invoke-debugger define-setf-expander
                             parse-namestring call-method char-code-limit
                             pprint-pop simple-type-error list-length copy-tree
                             floatp))
  (deftrophy :symbol-usage-level45 "シンボルの使用頻度ランキングトップ440〜450のシンボルをコンプリートしました。"
   '(adjustable-array-p simple-condition-format-control simple-string-p psetq
                        subsetp make-package acos do-symbols compiler-macro
                        substitute-if-not prog2 find-method psetf cdadr
                        pathname-device wild-pathname-p
                        simple-condition-format-arguments pathname-host
                        pprint-exit-if-list-exhausted asin most-negative-fixnum
                        lisp-implementation-type gentemp
                        with-hash-table-iterator *debugger-hook* caadr
                        package-nicknames array-dimension-limit tanh
                        base-string restart-name enough-namestring
                        array-in-bounds-p simple-warning *print-right-margin*
                        method-combination random-state allocate-instance cosh
                        get-decoded-time))
  (deftrophy :symbol-usage-level46 "シンボルの使用頻度ランキングトップ450〜460のシンボルをコンプリートしました。"
   '(sinh ed slot-exists-p lisp-implementation-version sixth cis
          list-all-packages string-stream read-char-no-hang
          translate-logical-pathname array-total-size-limit listen use-package
          stream-error shadow unbound-slot *print-base* ccase sublis
          special-operator-p graphic-char-p digit-char remove-method
          slot-unbound notevery provide compile-file vector-pop isqrt
          make-synonym-stream read-preserving-whitespace type-error-datum
          decode-float logcount standard-generic-function file-error
          float-radix member-if complexp delete-package
          most-positive-single-float ldiff standard-method symbol-plist char<
          nsubstitute makunbound upgraded-array-element-type mask-field
          set-syntax-from-char nstring-upcase compute-applicable-methods
          packagep rationalize arithmetic-error boole cadddr string>
          float-digits hash-table-size nreconc copy-structure unintern
          rename-file compiled-function machine-instance array-displacement
          package-use-list mapl ctypecase nstring-downcase cddddr
          make-concatenated-stream shadowing-import
          update-instance-for-different-class standard-char
          floating-point-underflow pprint-indent))
  (deftrophy :symbol-usage-level47 "シンボルの使用頻度ランキングトップ460〜470のシンボルをコンプリートしました。"
   '(seventh *macroexpand-hook* delete-if-not *compile-verbose* lcm scale-float
             program-error rationalp stream-error-stream compute-restarts
             reader-error no-applicable-method interactive-stream-p
             restart-bind caaar nsubst unbound-variable hash-table-rehash-size
             define-method-combination get-properties method-qualifiers
             get-dispatch-macro-character prog* cadadr single-float-epsilon
             most-positive-long-float set-exclusive-or machine-type nunion room
             unexport slot-missing cddar least-positive-double-float
             make-method char-name maplist *print-pprint-dispatch*
             hash-table-rehash-threshold function-lambda-expression string<=
             structure-class pathname-version y-or-n-p *print-array* cdaaar
             cdaar store-value invoke-restart-interactively nset-difference
             boole-and standard remprop *read-base* revappend
             long-float-epsilon untrace acosh rename-package assoc-if
             ensure-generic-function cell-error-name char> input-stream-p
             char-int asinh package-error logical-pathname boole-ior tree-equal
             pairlis ++ least-positive-normalized-double-float
             type-error-expected-type add-method *load-pathname*
             *load-truename* *print-lines* *print-radix* copy-pprint-dispatch
             atanh nsubstitute-if ** output-stream-p ninth gcd
             undefined-function standard-char-p ftruncate copy-symbol
             make-two-way-stream most-negative-long-float software-type tenth
             do-all-symbols with-package-iterator extended-char logandc1
             logorc2 array-row-major-index float-sign copy-alist ldb-test
             nsublis subst-if most-negative-single-float machine-version
             make-dispatch-macro-character cdadar short-float-epsilon
             both-case-p cell-error nintersection boole-1
             logical-pathname-translations built-in-class cadaar string>=
             package-used-by-list division-by-zero ffloor
             least-positive-normalized-single-float boole-xor nset-exclusive-or
             char>= nbutlast fround most-positive-short-float
             upgraded-complex-part-type no-next-method member-if-not
             make-instances-obsolete eighth random-state-p *load-verbose*
             nsubstitute-if-not software-version caaaar
             broadcast-stream-streams control-error readtablep //
             pathname-match-p compile-file-pathname dribble
             most-negative-short-float float-precision apropos-list caddar
             clear-input package-shadowing-symbols *compile-file-pathname*))
  (deftrophy :symbol-usage-level48 "シンボルの使用頻度ランキングトップ470〜480のシンボルをコンプリートしました。"
   '(synonym-stream *compile-file-truename* *modules* rassoc-if
     make-echo-stream package-error-package char-not-equal compiled-function-p
     two-way-stream-output-stream least-positive-normalized-long-float ***
     clear-output byte-position synonym-stream-symbol *gensym-counter*
     two-way-stream mapcon *break-on-signals* unuse-package yes-or-no-p
     with-compilation-unit two-way-stream-input-stream lambda-parameters-limit
     integer-decode-float bit-ior *compile-print* name-char /// char-lessp
     floating-point-overflow with-condition-restarts nsubst-if
     print-not-readable string-not-equal +++ *print-gensym* fceiling
     broadcast-stream lognand long-float-negative-epsilon bit-xor bit-not
     lognor unbound-slot-instance pprint-linear string-greaterp
     translate-pathname tailp boole-andc2 bit-vector-p apropos
     *print-miser-width* char-not-greaterp array-rank-limit logorc1
     assoc-if-not caaddr logeqv storage-condition simple-bit-vector-p caadar
     pprint-fill bit-and boole-2 cdaadr nsubst-if-not echo-stream boole-c1
     boole-orc2 file-error-pathname find-all-symbols
     least-negative-normalized-short-float short-float-negative-epsilon
     rassoc-if-not subst-if-not boole-nor boole-orc1 multiple-values-limit
     host-namestring least-positive-short-float
     least-negative-normalized-single-float
     least-negative-normalized-long-float call-arguments-limit boole-nand
     least-positive-normalized-short-float boole-set
     update-instance-for-redefined-class double-float-negative-epsilon
     disassemble single-float-negative-epsilon pprint-tab boole-clr
     least-negative-normalized-double-float cddaar cddadr boole-andc1 bit-andc2
     boole-eqv file-author print-not-readable-object boole-c2
     nstring-capitalize cdddar cdaddr caaadr char-greaterp file-string-length
     floating-point-inexact deposit-field least-negative-long-float bit-orc2
     arithmetic-error-operation load-logical-pathname-translations bit-nor
     echo-stream-output-stream long-site-name concatenated-stream-streams
     string-not-lessp floating-point-invalid-operation char-not-lessp
     concatenated-stream method-combination-error string-not-greaterp
     pprint-tabular *load-print* arithmetic-error-operands bit-nand
     invalid-method-error function-keywords least-negative-single-float
     least-negative-double-float bit-orc1 stream-external-format bit-eqv
     least-positive-long-float short-site-name least-negative-short-float
     bit-andc1 pprint-dispatch least-positive-single-float
     echo-stream-input-stream)))

;;;; COMPLETE-DICTIONARY

(defstruct (complete-dictionary (:include achievement)) released? dictionary)

(macrolet ((deftrophy (name message)
             `(let* ((dictionary (find-dictionary ',name))
                     (achievement
                      (setf (gethash ',name *achievements*)
                              (make-complete-dictionary :name ',name
                                                        :message ,message
                                                        :dictionary dictionary))))
                (loop :for symbol :being :each :hash-key :of
                           (dictionary-table dictionary)
                      :do (setf (symbol-achievements symbol) achievement)))))
  (deftrophy :evaluation-and-compile
   "Evaluation and compile dictionaryをコンプリートしました。")
  (deftrophy :types-and-classes "Types and classes dictionaryをコンプリートしました。")
  (deftrophy :iteration "Iteration dictionaryをコンプリートしました。")
  (deftrophy :objects "Objects dictionaryをコンプリートしました。")
  (deftrophy :structure "Structure dictionaryをコンプリートしました。")
  (deftrophy :conditions "Conditions dictionaryをコンプリートしました。")
  (deftrophy :packages "Packages dictionaryをコンプリートしました。")
  (deftrophy :numbers "Numbers dictionaryをコンプリートしました。")
  (deftrophy :characters "Characters dictionaryをコンプリートしました。")
  (deftrophy :conses "Conses dictionaryをコンプリートしました。")
  (deftrophy :arrays "Arrays dictionaryをコンプリートしました。")
  (deftrophy :strings "Strings dictionaryをコンプリートしました。")
  (deftrophy :sequences "Sequences dictionaryをコンプリートしました。")
  (deftrophy :hash-tables "Hash tables dictionaryをコンプリートしました。")
  (deftrophy :pathnames "Pathnames dictionaryをコンプリートしました。")
  (deftrophy :files "Files dictionaryをコンプリートしました。")
  (deftrophy :streams "Streams dictionaryをコンプリートしました。")
  (deftrophy :printer "Printer dictionaryをコンプリートしました。")
  (deftrophy :reader "Reader dictionaryをコンプリートしました。")
  (deftrophy :system-constructions
   "System constructions dictionaryをコンプリートしました。")
  (deftrophy :environment "Environment dictionaryをコンプリートしました。"))

;;;; FIRST-TIME

(defstruct (first-time (:include achievement)))

(macrolet ((deftrophy (name message)
             `(let ((achievement
                     (setf (gethash ',name *achievements*)
                             (make-first-time :name ',name :message ,message))))
                (setf (symbol-achievements ',name) achievement))))
  (deftrophy :first-sexp "初めてのS式を取得しました。")
  (deftrophy :first-error "初めてのエラーを取得しました。")
  (deftrophy :first-macro "初めてのマクロを取得しました。")
  (deftrophy :first-special-operator "初めての特殊形式を取得しました。"))

;;;; TIMES

(defstruct (times (:include achievement)) count symbol)

(macrolet ((def (name op count message)
             `(let ((achievement
                     (setf (gethash ',name *achievements*)
                             (make-times :name ',name
                                         :message ,message
                                         :count ,count
                                         :symbol ',op))))
                (setf (symbol-achievements ',op) achievement))))
  ;; level1
  (def :first-time-defun defun 1 "初めての関数定義を取得しました。")
  (def :setf-philia setf 30 "称号：代入愛好者を取得しました。")
  ;; level2
  (def :first-time-declare declare 1 "初めての宣言を取得しました。")
  (def :declare-philia declare 30 "称号：宣言者を取得しました。")
  (def :loop-philia loop 30 "称号：LOOP愛好者を取得しました。")
  (def :tips-for-length length 30 "LIST-LENGTHはご存知ですか？")
  (def :first-time-defmethod defmethod 1 "初めてのメソッド定義を取得しました。")
  ;; level3
  (def :lambda-master lambda 30 "称号：無名関数マスターを取得しました。")
  (def :format-black-belt format 30 "称号：FORMAT黒帯を取得しました。")
  (def :first-time-&key &key 1 "初めてのキーワード引数を取得しました。")
  (def :first-time-values values 1 "初めての多値返却を取得しました。")
  ;; level4
  (def :first-time-&optional &optional 1 "初めてのオプショナル引数を取得しました。")
  (def :first-time-&rest &rest 1 "初めての可変長引数を取得しました。")
  (def :first-time-defgeneric defgeneric 1 "初めての総称関数定義を取得しました。")
  ;; level5
  (def :first-time-defmacro defmacro 1 "初めてのマクロ定義を取得しました。")
  (def :first-time-defconstant defconstant 1 "初めての定数定義を取得しました。")
  (def :alternate-car car 15 "FIRSTはご存知ですか？")
  ;; level6
  (def :alternate-cdr cdr 15 "RESTはご存知ですか？")
  (def :first-time-defclass defclass 1 "初めてのクラス定義を取得しました。")
  (def :first-time-defvar defvar 1 "初めてのvar定義を取得しました。")
  ;; level7
  (def :setq-principleists setq 30 "称号：SETQ原理主義者を取得しました。")
  ;; level9
  (def :first-time-defparameter defparameter 1 "初めてのparameter定義を取得しました。")
  ;; level11
  (def :first-time-defpackage defpackage 1 "初めてのパッケージ定義を取得しました。")
  (def :introduce-gensym-counter gensym 30 "*GENSYM-COUNTER*はご存知ですか？")
  ;; level12
  (def :first-time-define-condition define-condition 1 "初めてのコンディション定義を取得しました。")
  (def :allow-other-keys-tips &allow-other-keys 30 "呼び出し側で指定することもできるとご存知ですか？")
  ;; level14
  (def :alternate-find find 10 "NILを見つけたいときは代わりにMEMBERが使えます。")
  ;; level15
  (def :first-time-defstruct defstruct 1 "初めての構造体定義を取得しました。")
  (def :first-time-eval-when eval-when 1 "称号：マクロ中級者を取得しました。")
  ;; level17
  (def :first-time-deftype deftype 1 "初めての型定義を取得しました。")
  (def :alternate-cadr cadr 5 "SECONDはご存知ですか？")
  ;; level26
  (def :first-time-define-compiler-macro define-compiler-macro 1
   "初めてのコンパイラマクロ定義を取得しました。")
  ;;
  )

;;;; SPECIAL-COMMANDS

(defvar *special-commands* (make-hash-table))

(defun get-special-command (form) (cdr (gethash form *special-commands*)))

(defmacro define-special-command (command description &body body)
  `(progn
    (setf (gethash ',command *special-commands*)
            (cons ,description (lambda () ,@body)))
    ',command))

(define-special-command :q
    "Quit dribble repl, returning to top level."
  (throw 'quit (values)))

(define-special-command ?
    "Print descriptions of special commands."
  (let ((max
         (loop :for key :being :each :hash-key :of *special-commands*
               :maximize (length (prin1-to-string key)))))
    (loop :for (description . nil) :being :each :hash-value :of
               *special-commands* :using (:hash-key command)
          :do (format t "~%[~VS] :~A" max command description))
    (values)))

(define-special-command :a
    "Print archievements."
  (let ((size (hash-table-count *achievements*))
        (completed
         (loop :for achievement :being :each :hash-value :of *achievements*
               :if (achievement-completed? achievement)
                 :collect (achievement-name achievement))))
    (funcall
      (formatter
       "~<~&~:I~D/~D achievements are done.~:@_~/cl:pprint-tabular/~:>")
      *standard-output* (list (length completed) size completed)))
  (values))

;;;; REPL

(defun trophy-read (&optional (*standard-input* *query-io*))
  (let ((*standard-output* *query-io*))
    (format t "~%TROPHY> ")
    (force-output)
    (read)))

(defun trophy-eval (exp)
  (check-achievement :first-sexp)
  (let* ((*debugger-hook*
          (lambda (condition hook)
            (declare (ignore condition hook))
            (check-achievement :first-error)))
         (results
          (multiple-value-list
           (cond
            ((atom exp)
             (if (get-special-command exp)
                 (return-from trophy-eval (funcall (get-special-command exp)))
                 (eval exp)))
            ((and (symbolp (car exp)) (special-operator-p (car exp)))
             (check-achievement :first-special-operator)
             (check-achievement (car exp))
             (eval exp))
            ((and (symbolp (car exp)) (macro-function (car exp)))
             (check-achievement :first-macro)
             (check-achievement (car exp))
             ;; We do not treat expanded code as your achievements.
             (eval (macroexpand exp)))
            (t
             (destructuring-bind
                 (op . args)
                 exp
               (let ((args (mapcar #'trophy-eval args)))
                 (check-achievement op)
                 (apply op args))))))))
    (shiftf +++ ++ + exp)
    (shiftf *** ** * (car results))
    (shiftf /// // / results)
    (values-list results)))

(defun trophy-print (&rest values)
  (map nil #'print values)
  (force-output)
  (values))

(defun repl (user-name)
  (when (probe-file
          (merge-pathnames (string-downcase user-name) +users-directory+))
    (load-user user-name))
  (unwind-protect
      (catch 'quit
        (loop (restart-case (multiple-value-call #'trophy-print
                              (trophy-eval (trophy-read)))
                (abort () :report "Return to trophy repl."))))
    (save user-name)))

(defun trophy-walk (form)
  (check-achievement :first-sexp)
  (trestrul:traverse
    (lambda (x)
      (cond #+sbcl
            ((sb-int:comma-p x) (trophy-walk (sb-int:comma-expr x)))
            (t
             (cond
              ((and (symbolp x) (special-operator-p x))
               (check-achievement :first-special-operator))
              ((and (symbolp x) (macro-function x))
               (check-achievement :first-macro)
               (check-achievement x))
              ((symbolp x) (check-achievement x))
              (t x)))))
    form))

(defvar *trophy-package* (find-package :cl-user))

(defun macroexpand-hook (expander form env)
  (when (and (eq *package* *trophy-package*) (null env)) ; Top level form.
    (trophy-walk form))
  (funcall expander form env))

(defun debugger-hook (condition hook)
  (declare (ignore condition hook))
  (check-achievement :first-error))

(let ((name))
  (defun set-env (user-name)
    (if user-name
        (progn
         (setf name user-name)
         (when (probe-file
                 (merge-pathnames (string-downcase user-name)
                                  +users-directory+))
           (load-user user-name))
         (setf *trophy-package* *package*)
         (if (eq 'funcall *macroexpand-hook*)
             (setf *macroexpand-hook* 'macroexpand-hook)
             (unless (eq *macroexpand-hook* 'macroexpand-hook)
               (if (y-or-n-p
                     "~<*MACROEXPAND-HOOK* is already set:~^ ~:_~S~:@_Force to replace?~:>"
                     (list *macroexpand-hook*))
                   (setf *macroexpand-hook* 'macroexpand-hook))))
         (if (null *debugger-hook*)
             (setf *debugger-hook* 'debugger-hook)
             (unless (eq *debugger-hook* 'debugger-hook)
               (if (y-or-n-p
                     "~<*DEBUGGER-HOOK* is already set:~^ ~:_~S~:@_Force to replace?~:>"
                     (list *debugger-hook*))
                   (setf *debugger-hook* 'debugger-hook)))))
        (progn
         (setf *trophy-package* (find-package :cl-user)
               *macroexpand-hook* 'funcall
               *debugger-hook* nil)
         (save name)
         (setf name nil)))
    name))

(defgeneric check-achievement (arg &optional op)
  (:method ((arg symbol) &optional op)
    (assert (null op))
    (dolist (achievement (symbol-achievements arg))
      (check-achievement achievement arg)))
  (:method ((arg complete-dictionary) &optional op)
    (with-slots (released? name completed? dictionary)
        arg
      (unless released?
        (setf released? t)
        (format *trophy-output* "Dictionary is released! ~S" name))
      (unless completed?
        (incf (gethash op (dictionary-table dictionary)))
        (when (dictionary-complete-p dictionary)
          (setf completed? t)
          (format *trophy-output* "Dictionary is completed! ~S"
                  (dictionary-name dictionary))))))
  (:method ((arg complete-ranking) &optional op)
    (with-slots (symbols completed? message)
        arg
      (unless completed?
        (setf symbols (remove op symbols))
        (unless symbols
          (setf completed? t)
          (format *trophy-output* message)))))
  (:method ((arg first-time) &optional op)
    (declare (ignore op))
    (unless (first-time-completed? arg)
      (setf (first-time-completed? arg) t)
      (format *trophy-output* (achievement-message arg))))
  (:method ((arg times) &optional op)
    (with-slots (completed? count message)
        arg
      (unless completed?
        (when (= (symbol-times op) count)
          (setf completed? t)
          (format *trophy-output* message))))))
