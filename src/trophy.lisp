(in-package :trophy)

;;;; *TROPHY-OUTPUT*

(defvar *trophy-output* *standard-output*)

;;;; ACHIEVEMENT

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric check-achievement (arg &optional op)
    (:method ((arg symbol) &optional op)
      (assert (null op))
      (dolist (achievement (symbol-achievements arg))
        (check-achievement achievement arg))))
  (defvar *readable-types* nil))

(defmacro defachievement (name slots &body clauses)
  ;; trivial syntax check.
  (check-type name symbol)
  (assert (every #'listp clauses))
  (assert (null
            (set-exclusive-or '(:printer :checker :defmacro)
                              (mapcar #'car clauses))))
  ;; body
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; type
     (pushnew (defstruct (,name (:include achievement)) ,@slots)
              *readable-types*)
     ;; printer
     (defun ,(intern (format nil "PRINT-READABLE-~A" name))
            ,@(cdr (assoc :printer clauses)))
     ;; checker
     ,(destructuring-bind
          (lambda-list . body)
          (cdr (assoc :checker clauses))
        `(defmethod check-achievement
                    ((,(car lambda-list) ,name) ,@(cdr lambda-list))
           ,@body))
     ;; defmacro
     (defmacro ,@(cdr (assoc :defmacro clauses)))
     ',name))

(set-pprint-dispatch '(cons (member defachievement))
                     (formatter
                      #.(apply #'concatenate 'string
                               (alexandria:flatten
                                 (list "~:<" ; logical block.
                                       "~W~^ ~@_" ; op
                                       "~W~^ ~1I~@_" ; name
                                       (list "~:<" ; slots
                                             (list "~@{" ; each slot
                                                   "~:/pprint-fill/~^ ~:_"
                                                   "~}")
                                             "~:>~^ ~_")
                                       "~@{~:/pprint-fill/~^ ~_~}" ; clauses.
                                       "~:>")))))

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
    (set-pprint-dispatch 'dictionary 'print-readable-dictionary)
    (dolist (type *readable-types*)
      (set-pprint-dispatch type
                           (uiop:find-symbol*
                             (format nil "PRINT-READABLE-~A" type) :trophy)))
    *print-pprint-dispatch*))

(defun debug-printer ()
  (let ((*print-pprint-dispatch* (print-readable-dispatch)))
    (print `(in-package :trophy))
    (print *achievements*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+users-directory+)
    (defconstant +users-directory+
      (ensure-directories-exist
        (merge-pathnames "users/"
                         (asdf:system-source-directory
                           (asdf:find-system :trophy)))))))

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

(defachievement complete-ranking (symbols)
  (:printer (stream exp)
   (write
     `(let ((achievement
             (,(intern (format nil "MAKE-~A" (type-of exp))) ,@(kv-args exp))))
        (loop :for symbol :in ',(complete-ranking-symbols exp)
              :do (setf (symbol-achievements symbol) achievement))
        achievement)
     :stream stream))
  (:checker (arg &optional op)
   (with-slots (symbols completed? message)
       arg
     (unless completed?
       (setf symbols (remove op symbols))
       (unless symbols
         (setf completed? t)
         (terpri *trophy-output*)
         (format *trophy-output* message)))))
  (:defmacro defrank (name message list)
   `(let* ((list ,list)
           (achievement
            (setf (gethash ',name *achievements*)
                    (make-complete-ranking :name ',name
                                           :message ,message
                                           :symbols list))))
      (loop :for symbol :in ,list
            :do (setf (symbol-achievements symbol) achievement)))))

(defrank :symbol-usage-level1 "シンボルの使用頻度ランキングトップ10のシンボルをコンプリートしました。"
         '(quote setf defun type t let function stream if *))

(defrank :symbol-usage-level2 "シンボルの使用頻度ランキングトップ10〜20のシンボルをコンプリートしました。"
         '(defmethod when declare list string or and loop length =))

(defrank :symbol-usage-level3 "シンボルの使用頻度ランキングトップ20〜30のシンボルをコンプリートしました。"
         '(lambda format
            &key
            signed-byte
            aref
            +
            double-float
            simple-array
            values
            in-package))

(defrank :symbol-usage-level4 "シンボルの使用頻度ランキングトップ30〜40のシンボルをコンプリートしました。"
         '(- &optional error cond &rest let* unless not defgeneric null))

(defrank :symbol-usage-level5 "シンボルの使用頻度ランキングトップ40〜50のシンボルをコンプリートしました。"
         '(ignore do funcall defmacro car eq defconstant eql cons first))

(defrank :symbol-usage-level6 "シンボルの使用頻度ランキングトップ50〜60のシンボルをコンプリートしました。"
         '(cdr gethash single-float defclass make-instance defvar progn
               multiple-value-bind push mapcar))

(defrank :symbol-usage-level7 "シンボルの使用頻度ランキングトップ60〜70のシンボルをコンプリートしました。"
         '(export symbol inline < > apply / &body char setq))

(defrank :symbol-usage-level8 "シンボルの使用頻度ランキングトップ70〜80のシンボルをコンプリートしました。"
         '(return slot-value incf vector class fixnum count the assert rest))

(defrank :symbol-usage-level9 "シンボルの使用頻度ランキングトップ80〜90のシンボルをコンプリートしました。"
         '(complex 1+ make-array declaim equal ldb 1- sequence defparameter
                   second))

(defrank :symbol-usage-level10 "シンボルの使用頻度ランキングトップ90〜100のシンボルをコンプリートしました。"
         '(integer unsigned-byte member dolist condition array zerop typep <=
           getf))

(defrank :symbol-usage-level11 "シンボルの使用頻度ランキングトップ100〜110のシンボルをコンプリートしました。"
         '(position defpackage with-slots max optimize subseq gensym append
                    speed return-from check-type))

(defrank :symbol-usage-level12 "シンボルの使用頻度ランキングトップ110〜120のシンボルをコンプリートしました。"
         '(elt float flet number byte &allow-other-keys string= min pathname
               define-condition destructuring-bind))

(defrank :symbol-usage-level13 "シンボルの使用頻度ランキングトップ120〜130のシンボルをコンプリートしました。"
         '(map case go safety make-hash-table labels coerce >= method package))

(defrank :symbol-usage-level14 "シンボルの使用頻度ランキングトップ130〜140のシンボルをコンプリートしました。"
         '(char= dotimes find call-next-method symbol-name listp ecase tagbody
                 assoc debug))

(defrank :symbol-usage-level15 "シンボルの使用頻度ランキングトップ140〜150のシンボルをコンプリートしました。"
         '(defstruct character
            concatenate
            consp
            eval-when
            etypecase
            ignorable
            logxor
            ash
            write-string))

(defrank :symbol-usage-level16 "シンボルの使用頻度ランキングトップ150〜160のシンボルをコンプリートしました。"
         '(svref boolean symbolp set intern abs nreverse expt time plusp))

(defrank :symbol-usage-level17 "シンボルの使用頻度ランキングトップ160〜170のシンボルをコンプリートしました。"
         '(pop deftype print-object documentation cadr stringp mod identity nth
               cddr))

(defrank :symbol-usage-level18 "シンボルの使用頻度ランキングトップ170〜180のシンボルをコンプリートしました。"
         '(logand write-char list* reduce equalp last handler-case class-name
                  decf real floor /=))

(defrank :symbol-usage-level19 "シンボルの使用頻度ランキングトップ180〜190のシンボルをコンプリートしました。"
         '(typecase princ
            print-unreadable-object
            unwind-protect
            multiple-value-list
            char-code
            string-equal
            every
            initialize-instance
            otherwise))

(defrank :symbol-usage-level20 "シンボルの使用頻度ランキングトップ190〜200のシンボルをコンプリートしました。"
         '(block keyword
            warn
            prog1
            reverse
            parse-integer
            logior
            macrolet
            *standard-output*
            with-output-to-string))

(defrank :symbol-usage-level21 "シンボルの使用頻度ランキングトップ200〜210のシンボルをコンプリートしました。"
         '(find-package ftype remove dynamic-extent with-open-file log variable
                        sort find-class hash-table code-char))

(defrank :symbol-usage-level22 "シンボルの使用頻度ランキングトップ210〜220のシンボルをコンプリートしました。"
         '(third atom *package* string-upcase class-of string-downcase
                 slot-boundp pushnew truncate step))

(defrank :symbol-usage-level23 "シンボルの使用頻度ランキングトップ220〜230のシンボルをコンプリートしました。"
         '(read simple-string round read-char merge-pathnames simple-vector
                random &aux replace directory get))

(defrank :symbol-usage-level24 "シンボルの使用頻度ランキングトップ230〜240のシンボルをコンプリートしました。"
         '(numberp namestring space nconc eval type-of ignore-errors sqrt
                   remove-if exp minusp))

(defrank :symbol-usage-level25 "シンボルの使用頻度ランキングトップ240〜250のシンボルをコンプリートしました。"
         '(array-dimension package-name with-accessors locally
                           vector-push-extend princ-to-string mapc terpri
                           &whole integerp subtypep write-byte make-pathname
                           sin))

(defrank :symbol-usage-level26 "シンボルの使用頻度ランキングトップ250〜260のシンボルをコンプリートしました。"
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

(defrank :symbol-usage-level27 "シンボルの使用頻度ランキングトップ260〜270のシンボルをコンプリートしました。"
         '(bit cos copy-list caar handler-bind &environment get-universal-time
               some constantp find-symbol close))

(defrank :symbol-usage-level28 "シンボルの使用頻度ランキングトップ270〜280のシンボルをコンプリートしました。"
         '(copy-seq logbitp probe-file realpart find-if file-position
                    multiple-value-setq endp make-string evenp))

(defrank :symbol-usage-level29 "シンボルの使用頻度ランキングトップ280〜290のシンボルをコンプリートしました。"
         '(maphash *readtable* ceiling schar read-from-string read-sequence
                   symbol-macrolet imagpart rotatef restart-case))

(defrank :symbol-usage-level30 "シンボルの使用頻度ランキングトップ290〜300のシンボルをコンプリートしました。"
         '(mapcan phase rational read-line peek-char finish-output
                  *error-output* boundp array-dimensions readtable *features*
                  fboundp))

(defrank :symbol-usage-level31 "シンボルの使用頻度ランキングトップ300〜310のシンボルをコンプリートしました。"
         '(symbol-value remove-duplicates hash-table-count array-element-type
                        delete values-list constantly write
                        most-positive-fixnum union short-float functionp))

(defrank :symbol-usage-level32 "シンボルの使用頻度ランキングトップ310〜320のシンボルをコンプリートしました。"
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

(defrank :symbol-usage-level33 "シンボルの使用頻度ランキングトップ320〜330のシンボルをコンプリートしました。"
         '(long-float pathname-directory intersection multiple-value-call
           mismatch string-trim row-major-aref pathname-type fdefinition open
           make-condition force-output))

(defrank :symbol-usage-level34 "シンボルの使用頻度ランキングトップ330〜340のシンボルをコンプリートしました。"
         '(rem symbol-function pathname-name complement abort
               multiple-value-prog1 char-equal remf string< signal
               *print-pretty* continue standard-class cdar caddr))

(defrank :symbol-usage-level35 "シンボルの使用頻度ランキングトップ340〜350のシンボルをコンプリートしました。"
         '(arrayp catch reinitialize-instance sleep print compilation-speed
                  cerror warning get-internal-real-time substitute notinline
                  get-output-stream-string bignum))

(defrank :symbol-usage-level36 "シンボルの使用頻度ランキングトップ350〜360のシンボルをコンプリートしました。"
         '(*trace-output* write-line char<= fresh-line standard-object
           change-class ratio subst ensure-directories-exist clrhash
           simple-error with-input-from-string satisfies))

(defrank :symbol-usage-level37 "シンボルの使用頻度ランキングトップ360〜370のシンボルをコンプリートしました。"
         '(fourth prin1-to-string oddp end-of-file invoke-restart *query-io*
                  *debug-io* set-macro-character shared-initialize dpb vectorp
                  count-if compile truename make-load-form
                  make-string-output-stream *standard-input*))

(defrank :symbol-usage-level38 "シンボルの使用頻度ランキングトップ370〜380のシンボルをコンプリートしました。"
         '(integer-length byte-size declaration macro-function defsetf
                          double-float-epsilon copy-readtable denominator
                          map-into define-modify-macro write-to-string
                          formatter type-error decode-universal-time
                          with-standard-io-syntax char/=
                          internal-time-units-per-second file-length))

(defrank :symbol-usage-level39 "シンボルの使用頻度ランキングトップ380〜390のシンボルをコンプリートしました。"
         '(numerator rplacd prin1 unread-char char-upcase position-if-not
                     adjust-array pprint-logical-block upper-case-p base-char
                     set-dispatch-macro-character))

(defrank :symbol-usage-level40 "シンボルの使用頻度ランキングトップ390〜400のシンボルをコンプリートしました。"
         '(alphanumericp bit-vector *print-readably* realp signum
                         simple-vector-p loop-finish file-write-date logtest
                         file-namestring readtable-case make-symbol structure
                         alpha-char-p atan *default-pathname-defaults*
                         char-downcase progv macroexpand-1))

(defrank :symbol-usage-level41 "シンボルの使用頻度ランキングトップ400〜410のシンボルをコンプリートしました。"
         '(delete-file define-symbol-macro pathnamep encode-universal-time
                       with-simple-restart break describe cdddr muffle-warning
                       cadar string/= generic-function streamp find-restart
                       most-positive-double-float *random-state* find-if-not
                       trace lambda-list-keywords *read-default-float-format*
                       *print-length* user-homedir-pathname macroexpand
                       slot-makunbound))

(defrank :symbol-usage-level42 "シンボルの使用頻度ランキングトップ410〜420のシンボルをコンプリートしました。"
         '(load acons *print-level* *read-eval* make-broadcast-stream
                stream-element-type fmakunbound shiftf get-setf-expansion tan
                get-macro-character import lower-case-p merge substitute-if
                stable-sort lognot read-delimited-list fifth string-right-trim
                array-rank notany *print-circle* style-warning))

(defrank :symbol-usage-level43 "シンボルの使用頻度ランキングトップ420〜430のシンボルをコンプリートしました。"
         '(pprint-newline vector-push *print-escape* hash-table-p sbit
                          simple-bit-vector delete-duplicates open-stream-p
                          parse-error inspect delete-if *terminal-io* use-value
                          adjoin get-internal-run-time hash-table-test
                          structure-object make-string-input-stream logandc2
                          make-load-form-saving-slots with-open-stream
                          conjugate directory-namestring *read-suppress*))

(defrank :symbol-usage-level44 "シンボルの使用頻度ランキングトップ430〜440のシンボルをコンプリートしました。"
         '(compiler-macro-function rplaca string-capitalize string-lessp
                                   count-if-not string-left-trim require
                                   *print-case* describe-object next-method-p
                                   simple-condition set-pprint-dispatch
                                   proclaim simple-base-string
                                   do-external-symbols file-stream
                                   make-sequence rassoc make-random-state
                                   pprint most-negative-double-float
                                   array-has-fill-pointer-p sxhash
                                   serious-condition invoke-debugger
                                   define-setf-expander parse-namestring
                                   call-method char-code-limit pprint-pop
                                   simple-type-error list-length copy-tree
                                   floatp))

(defrank :symbol-usage-level45 "シンボルの使用頻度ランキングトップ440〜450のシンボルをコンプリートしました。"
         '(adjustable-array-p simple-condition-format-control simple-string-p
                              psetq subsetp make-package acos do-symbols
                              compiler-macro substitute-if-not prog2
                              find-method psetf cdadr pathname-device
                              wild-pathname-p simple-condition-format-arguments
                              pathname-host pprint-exit-if-list-exhausted asin
                              most-negative-fixnum lisp-implementation-type
                              gentemp with-hash-table-iterator *debugger-hook*
                              caadr package-nicknames array-dimension-limit
                              tanh base-string restart-name enough-namestring
                              array-in-bounds-p simple-warning
                              *print-right-margin* method-combination
                              random-state allocate-instance cosh
                              get-decoded-time))

(defrank :symbol-usage-level46 "シンボルの使用頻度ランキングトップ450〜460のシンボルをコンプリートしました。"
         '(sinh ed slot-exists-p lisp-implementation-version sixth cis
                list-all-packages string-stream read-char-no-hang
                translate-logical-pathname array-total-size-limit listen
                use-package stream-error shadow unbound-slot *print-base* ccase
                sublis special-operator-p graphic-char-p digit-char
                remove-method slot-unbound notevery provide compile-file
                vector-pop isqrt make-synonym-stream read-preserving-whitespace
                type-error-datum decode-float logcount
                standard-generic-function file-error float-radix member-if
                complexp delete-package most-positive-single-float ldiff
                standard-method symbol-plist char< nsubstitute makunbound
                upgraded-array-element-type mask-field set-syntax-from-char
                nstring-upcase compute-applicable-methods packagep rationalize
                arithmetic-error boole cadddr string> float-digits
                hash-table-size nreconc copy-structure unintern rename-file
                compiled-function machine-instance array-displacement
                package-use-list mapl ctypecase nstring-downcase cddddr
                make-concatenated-stream shadowing-import
                update-instance-for-different-class standard-char
                floating-point-underflow pprint-indent))

(defrank :symbol-usage-level47 "シンボルの使用頻度ランキングトップ460〜470のシンボルをコンプリートしました。"
         '(seventh *macroexpand-hook* delete-if-not *compile-verbose* lcm
                   scale-float program-error rationalp stream-error-stream
                   compute-restarts reader-error no-applicable-method
                   interactive-stream-p restart-bind caaar nsubst
                   unbound-variable hash-table-rehash-size
                   define-method-combination get-properties method-qualifiers
                   get-dispatch-macro-character prog* cadadr
                   single-float-epsilon most-positive-long-float
                   set-exclusive-or machine-type nunion room unexport
                   slot-missing cddar least-positive-double-float make-method
                   char-name maplist *print-pprint-dispatch*
                   hash-table-rehash-threshold function-lambda-expression
                   string<= structure-class pathname-version y-or-n-p
                   *print-array* cdaaar cdaar store-value
                   invoke-restart-interactively nset-difference boole-and
                   standard remprop *read-base* revappend long-float-epsilon
                   untrace acosh rename-package assoc-if
                   ensure-generic-function cell-error-name char> input-stream-p
                   char-int asinh package-error logical-pathname boole-ior
                   tree-equal pairlis ++ least-positive-normalized-double-float
                   type-error-expected-type add-method *load-pathname*
                   *load-truename* *print-lines* *print-radix*
                   copy-pprint-dispatch atanh nsubstitute-if ** output-stream-p
                   ninth gcd undefined-function standard-char-p ftruncate
                   copy-symbol make-two-way-stream most-negative-long-float
                   software-type tenth do-all-symbols with-package-iterator
                   extended-char logandc1 logorc2 array-row-major-index
                   float-sign copy-alist ldb-test nsublis subst-if
                   most-negative-single-float machine-version
                   make-dispatch-macro-character cdadar short-float-epsilon
                   both-case-p cell-error nintersection boole-1
                   logical-pathname-translations built-in-class cadaar string>=
                   package-used-by-list division-by-zero ffloor
                   least-positive-normalized-single-float boole-xor
                   nset-exclusive-or char>= nbutlast fround
                   most-positive-short-float upgraded-complex-part-type
                   no-next-method member-if-not make-instances-obsolete eighth
                   random-state-p *load-verbose* nsubstitute-if-not
                   software-version caaaar broadcast-stream-streams
                   control-error readtablep // pathname-match-p
                   compile-file-pathname dribble most-negative-short-float
                   float-precision apropos-list caddar clear-input
                   package-shadowing-symbols *compile-file-pathname*))

(defrank :symbol-usage-level48 "シンボルの使用頻度ランキングトップ470〜480のシンボルをコンプリートしました。"
         '(synonym-stream *compile-file-truename* *modules* rassoc-if
           make-echo-stream package-error-package char-not-equal
           compiled-function-p two-way-stream-output-stream
           least-positive-normalized-long-float *** clear-output byte-position
           synonym-stream-symbol *gensym-counter* two-way-stream mapcon
           *break-on-signals* unuse-package yes-or-no-p with-compilation-unit
           two-way-stream-input-stream lambda-parameters-limit
           integer-decode-float bit-ior *compile-print* name-char ///
           char-lessp floating-point-overflow with-condition-restarts nsubst-if
           print-not-readable string-not-equal +++ *print-gensym* fceiling
           broadcast-stream lognand long-float-negative-epsilon bit-xor bit-not
           lognor unbound-slot-instance pprint-linear string-greaterp
           translate-pathname tailp boole-andc2 bit-vector-p apropos
           *print-miser-width* char-not-greaterp array-rank-limit logorc1
           assoc-if-not caaddr logeqv storage-condition simple-bit-vector-p
           caadar pprint-fill bit-and boole-2 cdaadr nsubst-if-not echo-stream
           boole-c1 boole-orc2 file-error-pathname find-all-symbols
           least-negative-normalized-short-float short-float-negative-epsilon
           rassoc-if-not subst-if-not boole-nor boole-orc1
           multiple-values-limit host-namestring least-positive-short-float
           least-negative-normalized-single-float
           least-negative-normalized-long-float call-arguments-limit boole-nand
           least-positive-normalized-short-float boole-set
           update-instance-for-redefined-class double-float-negative-epsilon
           disassemble single-float-negative-epsilon pprint-tab boole-clr
           least-negative-normalized-double-float cddaar cddadr boole-andc1
           bit-andc2 boole-eqv file-author print-not-readable-object boole-c2
           nstring-capitalize cdddar cdaddr caaadr char-greaterp
           file-string-length floating-point-inexact deposit-field
           least-negative-long-float bit-orc2 arithmetic-error-operation
           load-logical-pathname-translations bit-nor echo-stream-output-stream
           long-site-name concatenated-stream-streams string-not-lessp
           floating-point-invalid-operation char-not-lessp concatenated-stream
           method-combination-error string-not-greaterp pprint-tabular
           *load-print* arithmetic-error-operands bit-nand invalid-method-error
           function-keywords least-negative-single-float
           least-negative-double-float bit-orc1 stream-external-format bit-eqv
           least-positive-long-float short-site-name least-negative-short-float
           bit-andc1 pprint-dispatch least-positive-single-float
           echo-stream-input-stream))

;;;; COMPLETE-DICTIONARY

(defachievement complete-dictionary (released? dictionary)
  (:printer (stream exp)
   (write
     `(let ((achievement
             (,(intern (format nil "MAKE-~A" (type-of exp))) ,@(kv-args exp))))
        (loop :for symbol :being :each :hash-key :of
                   (dictionary-table (find-dictionary ',(achievement-name exp)))
              :do (setf (symbol-achievements symbol) achievement))
        achievement)
     :stream stream))
  (:checker (arg &optional op)
   (with-slots (released? name completed? dictionary)
       arg
     (unless released?
       (setf released? t)
       (format *trophy-output* "~%~S Dictionary is released!" name))
     (unless completed?
       (incf (gethash op (dictionary-table dictionary)))
       (when (dictionary-complete-p dictionary)
         (setf completed? t)
         (format *trophy-output* "~%~S Dictionary is completed!"
                 (dictionary-name dictionary))))))
  (:defmacro defdict (name message)
   `(let* ((dictionary (find-dictionary ',name))
           (achievement
            (setf (gethash ',name *achievements*)
                    (make-complete-dictionary :name ',name
                                              :message ,message
                                              :dictionary dictionary))))
      (loop :for symbol :being :each :hash-key :of (dictionary-table dictionary)
            :do (setf (symbol-achievements symbol) achievement)))))

(defdict :evaluation-and-compile
         "Evaluation and compile dictionaryをコンプリートしました。")

(defdict :types-and-classes "Types and classes dictionaryをコンプリートしました。")

(defdict :iteration "Iteration dictionaryをコンプリートしました。")

(defdict :objects "Objects dictionaryをコンプリートしました。")

(defdict :structure "Structure dictionaryをコンプリートしました。")

(defdict :conditions "Conditions dictionaryをコンプリートしました。")

(defdict :packages "Packages dictionaryをコンプリートしました。")

(defdict :numbers "Numbers dictionaryをコンプリートしました。")

(defdict :characters "Characters dictionaryをコンプリートしました。")

(defdict :conses "Conses dictionaryをコンプリートしました。")

(defdict :arrays "Arrays dictionaryをコンプリートしました。")

(defdict :strings "Strings dictionaryをコンプリートしました。")

(defdict :sequences "Sequences dictionaryをコンプリートしました。")

(defdict :hash-tables "Hash tables dictionaryをコンプリートしました。")

(defdict :pathnames "Pathnames dictionaryをコンプリートしました。")

(defdict :files "Files dictionaryをコンプリートしました。")

(defdict :streams "Streams dictionaryをコンプリートしました。")

(defdict :printer "Printer dictionaryをコンプリートしました。")

(defdict :reader "Reader dictionaryをコンプリートしました。")

(defdict :system-constructions "System constructions dictionaryをコンプリートしました。")

(defdict :environment "Environment dictionaryをコンプリートしました。")

;;;; FIRST-TIME

(defachievement first-time ()
  (:printer (stream exp)
   (write
     `(let ((achievement (make-first-time ,@(kv-args exp))))
        (setf (symbol-achievements ',(achievement-name exp)) achievement))
     :stream stream))
  (:checker (arg &optional op) (declare (ignore op))
   (unless (first-time-completed? arg)
     (setf (first-time-completed? arg) t)
     (terpri *trophy-output*)
     (format *trophy-output* (achievement-message arg))))
  (:defmacro deffirst (name message)
   `(let ((achievement
           (setf (gethash ',name *achievements*)
                   (make-first-time :name ',name :message ,message))))
      (setf (symbol-achievements ',name) achievement))))

(deffirst :first-sexp "初めてのS式を取得しました。")

(deffirst :first-error "初めてのエラーを取得しました。")

(deffirst :first-macro "初めてのマクロを取得しました。")

(deffirst :first-special-operator "初めての特殊形式を取得しました。")

;;;; TIMES

(defachievement times (count symbol)
  (:printer (stream exp)
   (write
     `(let ((achievement (make-times ,@(kv-args exp))))
        (setf (symbol-achievements ',(times-symbol exp)) achievement))
     :stream stream))
  (:checker (arg &optional op)
   (with-slots (completed? count message)
       arg
     (unless completed?
       (when (= (symbol-times op) count)
         (setf completed? t)
         (terpri *trophy-output*)
         (format *trophy-output* message)))))
  (:defmacro deftimes (name op count message)
   `(let ((achievement
           (setf (gethash ',name *achievements*)
                   (make-times :name ',name
                               :message ,message
                               :count ,count
                               :symbol ',op))))
      (setf (symbol-achievements ',op) achievement))))

;; level1

(deftimes :first-time-defun defun 1 "初めての関数定義を取得しました。")

(deftimes :setf-philia setf 30 "称号：代入愛好者を取得しました。")

;; level2

(deftimes :first-time-declare declare 1 "初めての宣言を取得しました。")

(deftimes :declare-philia declare 30 "称号：宣言者を取得しました。")

(deftimes :loop-philia loop 30 "称号：LOOP愛好者を取得しました。")

(deftimes :tips-for-length length 30 "LIST-LENGTHはご存知ですか？")

(deftimes :first-time-defmethod defmethod 1 "初めてのメソッド定義を取得しました。")

;; level3

(deftimes :lambda-master lambda 30 "称号：無名関数マスターを取得しました。")

(deftimes :format-black-belt format 30 "称号：FORMAT黒帯を取得しました。")

(deftimes :first-time-&key &key 1 "初めてのキーワード引数を取得しました。")

(deftimes :first-time-values values 1 "初めての多値返却を取得しました。")

(deftimes :first-time-in-package in-package 1 "初めてのお出かけを取得しました。")

(deftimes :first-time-in-package in-package 50 "称号：流浪の民を取得しました。")

;; level4

(deftimes :first-time-&optional &optional 1 "初めてのオプショナル引数を取得しました。")

(deftimes :first-time-&rest &rest 1 "初めての可変長引数を取得しました。")

(deftimes :first-time-defgeneric defgeneric 1 "初めての総称関数定義を取得しました。")

;; level5

(deftimes :first-time-defmacro defmacro 1 "初めてのマクロ定義を取得しました。")

(deftimes :first-time-defconstant defconstant 1 "初めての定数定義を取得しました。")

(deftimes :alternate-car car 15 "FIRSTはご存知ですか？")

;; level6

(deftimes :alternate-cdr cdr 15 "RESTはご存知ですか？")

(deftimes :first-time-defclass defclass 1 "初めてのクラス定義を取得しました。")

(deftimes :first-time-defvar defvar 1 "初めてのvar定義を取得しました。")

;; level7

(deftimes :setq-principleists setq 30 "称号：SETQ原理主義者を取得しました。")

;; level9

(deftimes :first-time-defparameter defparameter 1 "初めてのparameter定義を取得しました。")

;; level11

(deftimes :first-time-defpackage defpackage 1 "初めてのパッケージ定義を取得しました。")

(deftimes :introduce-gensym-counter gensym 30 "*GENSYM-COUNTER*はご存知ですか？")

;; level12

(deftimes :first-time-define-condition define-condition 1
          "初めてのコンディション定義を取得しました。")

(deftimes :allow-other-keys-tips &allow-other-keys 30
          "呼び出し側で指定することもできるとご存知ですか？")

;; level14

(deftimes :alternate-find find 10 "NILを見つけたいときは代わりにMEMBERが使えます。")

;; level15

(deftimes :first-time-defstruct defstruct 1 "初めての構造体定義を取得しました。")

(deftimes :first-time-eval-when eval-when 1 "称号：マクロ中級者を取得しました。")

;; level17

(deftimes :first-time-deftype deftype 1 "初めての型定義を取得しました。")

(deftimes :alternate-cadr cadr 5 "SECONDはご存知ですか？")

;; level26

(deftimes :first-time-define-compiler-macro define-compiler-macro 1
          "初めてのコンパイラマクロ定義を取得しました。")

;;;; SPECIAL-COMMANDS

(defvar *special-commands* (make-hash-table))

(defun get-special-command (form) (cdr (gethash form *special-commands*)))

(defun comcall (com)
  (funcall
    (or (get-special-command com) (error "Missing special command: ~S" com))))

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

(define-special-command :d
    "Print dictionary informations."
  (prog* ((unknown :?????)
          (known-dictionaries
           (loop :for name :being :each :hash-key :of *dictionaries*
                 :for achievement
                      := (or (gethash name *achievements*)
                             (error "Missing achievement of dict: ~S" name))
                 :if (achievement-completed? achievement)
                   :collect name
                 :else
                   :collect unknown)))
    (funcall (formatter "~<~:@_~:I~@/pprint-tabular/~:@_~:>") nil
             (list known-dictionaries))
    (force-output)
   :top
    (let ((name
           (prompt-for:prompt-for
             `(member :q ,@(remove unknown known-dictionaries))
             "~%To check dictionary, input its name.~%To quit, input :q.~%>> ")))
      (if (eq :q name)
          (return (values))
          (progn
           (print-dictionary-information (find-dictionary name))
           (if (y-or-n-p "Check others?")
               (go :top)
               (return (values))))))))

(defun print-dictionary-information (dictionary)
  (funcall (formatter "~%~{~50<~S~; ~D times used.~%~>~}") nil
           (loop :for symbol :being :each :hash-key :of
                      (dictionary-table dictionary) :using (:hash-value count)
                 :collect symbol
                 :collect count))
  (force-output))

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
                 (return-from trophy-eval (comcall exp))
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
             (cond ;; To ignore NIL which is in the end of a proper list.
                   ((null x) x)
                   ((and (symbolp x) (special-operator-p x))
                    (check-achievement :first-special-operator)
                    (check-achievement x))
                   ((and (symbolp x) (macro-function x))
                    (check-achievement :first-macro)
                    (check-achievement x))
                   ((symbolp x) (check-achievement x))
                   ;; To capture NIL which is not in the end of a proper list.
                   ((and (consp x) (null (car x))) (check-achievement nil))
                   (t x))))) ; do nothing.
    form))

(defvar *trophy-package* (find-package :cl-user))

(defun macroexpand-hook (expander form env)
  (when (and (eq *package* *trophy-package*) (null env)) ; Top level form.
    (trophy-walk form))
  (funcall expander form env))

(defun debugger-hook (condition hook)
  (declare (ignore condition hook))
  (check-achievement :first-error))

(defvar *user-name* nil)

(defun set-env (user-name)
  (if user-name
      (progn
       (setf *user-name* user-name)
       (when (probe-file
               (merge-pathnames (string-downcase user-name) +users-directory+))
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
       (save *user-name*)
       (setf *user-name* nil)))
  *user-name*)
