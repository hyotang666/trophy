(in-package :trophy)

;;;; APIs

(declaim
 (ftype (function (symbol) (values (or null cons) &optional))
        symbol-achievements))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric check-achievement (arg &optional op)
    (:method ((arg symbol) &optional op)
      (assert (null op))
      (incf (symbol-times arg))
      (dolist (achievement (symbol-achievements arg))
        (check-achievement achievement arg))))
  (defvar *readable-types* nil))

(defgeneric charms (name arg))

(defmethod charms (name (message string))
  (declare (ignore name))
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (loop (charms:clear-window charms:*standard-window*)
          (multiple-value-bind (width height)
              (charms:window-dimensions charms:*standard-window*)
            (let* ((lines
                    (uiop:split-string message
                                       :separator #.(string #\Newline)))
                   (length/2
                    (floor
                      (reduce #'max lines :key #'babel:string-size-in-octets)
                      2)))
              (loop :with w-pos := (- (floor width 2) length/2)
                    :for h-pos :upfrom (floor (- height (length lines)) 2)
                    :for line :in lines
                    :do (charms:write-string-at-point charms:*standard-window*
                                                      line w-pos h-pos))
              (charms:write-string-at-point charms:*standard-window*
                                            "Press [Enter] to next." 0
                                            (1- height))))
          (charms:refresh-window charms:*standard-window*)
          (case (charms:get-char charms:*standard-window* :ignore-error t)
            ((#\Newline) (return nil))
            ((#\q) (return nil))))))

(defmethod charms ((stream stream) (message string))
  (terpri stream)
  (format stream message))

(defmethod charms (name (messages list))
  (dolist (message messages) (charms name message)))

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

(defmethod charms (name (arg achievement))
  (charms name (translate:translate (achievement-message arg))))

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
  (prog ((*print-pprint-dispatch* (print-readable-dispatch)))
    (print `(in-package :trophy))
    (print `(setf translate:*language* ,translate:*language*))
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
   (with-slots (symbols completed? message name)
       arg
     (unless completed?
       (setf symbols (remove op symbols))
       (unless symbols
         (setf completed? t)
         (charms name
                 (concatenate 'string
                              (translate:translate "congratulations-aa")
                              #.(format nil "~2%")
                              (translate:translate message)))))))
  (:defmacro defrank (name message list)
   `(let* ((list ,list)
           (achievement
            (setf (gethash ',name *achievements*)
                    (make-complete-ranking :name ',name
                                           :message ,message
                                           :symbols list))))
      (loop :for symbol :in ,list
            :do (setf (symbol-achievements symbol) achievement)))))

(defrank :symbol-usage-level1 "symbol-usage-level1"
         '(quote setf defun type t let function stream if *))

(defrank :symbol-usage-level2 "symbol-usage-level2"
         '(defmethod when declare list string or and loop length =))

(defrank :symbol-usage-level3 "symbol-usage-level3"
         '(lambda format
            &key
            signed-byte
            aref
            +
            double-float
            simple-array
            values
            in-package))

(defrank :symbol-usage-level4 "symbol-usage-level4"
         '(- &optional error cond &rest let* unless not defgeneric null))

(defrank :symbol-usage-level5 "symbol-usage-level5"
         '(ignore do funcall defmacro car eq defconstant eql cons first))

(defrank :symbol-usage-level6 "symbol-usage-level6"
         '(cdr gethash single-float defclass make-instance defvar progn
               multiple-value-bind push mapcar))

(defrank :symbol-usage-level7 "symbol-usage-level7"
         '(export symbol inline < > apply / &body char setq))

(defrank :symbol-usage-level8 "symbol-usage-level8"
         '(return slot-value incf vector class fixnum count the assert rest))

(defrank :symbol-usage-level9 "symbol-usage-level9"
         '(complex 1+ make-array declaim equal ldb 1- sequence defparameter
                   second))

(defrank :symbol-usage-level10 "symbol-usage-level10"
         '(integer unsigned-byte member dolist condition array zerop typep <=
           getf))

(defrank :symbol-usage-level11 "symbol-usage-level11"
         '(position defpackage with-slots max optimize subseq gensym append
                    speed return-from check-type))

(defrank :symbol-usage-level12 "symbol-usage-level12"
         '(elt float flet number byte &allow-other-keys string= min pathname
               define-condition destructuring-bind))

(defrank :symbol-usage-level13 "symbol-usage-level13"
         '(map case go safety make-hash-table labels coerce >= method package))

(defrank :symbol-usage-level14 "symbol-usage-level14"
         '(char= dotimes find call-next-method symbol-name listp ecase tagbody
                 assoc debug))

(defrank :symbol-usage-level15 "symbol-usage-level15"
         '(defstruct character
            concatenate
            consp
            eval-when
            etypecase
            ignorable
            logxor
            ash
            write-string))

(defrank :symbol-usage-level16 "symbol-usage-level16"
         '(svref boolean symbolp set intern abs nreverse expt time plusp))

(defrank :symbol-usage-level17 "symbol-usage-level17"
         '(pop deftype print-object documentation cadr stringp mod identity nth
               cddr))

(defrank :symbol-usage-level18 "symbol-usage-level18"
         '(logand write-char list* reduce equalp last handler-case class-name
                  decf real floor /=))

(defrank :symbol-usage-level19 "symbol-usage-level19"
         '(typecase princ
            print-unreadable-object
            unwind-protect
            multiple-value-list
            char-code
            string-equal
            every
            initialize-instance
            otherwise))

(defrank :symbol-usage-level20 "symbol-usage-level20"
         '(block keyword
            warn
            prog1
            reverse
            parse-integer
            logior
            macrolet
            *standard-output*
            with-output-to-string))

(defrank :symbol-usage-level21 "symbol-usage-level21"
         '(find-package ftype remove dynamic-extent with-open-file log variable
                        sort find-class hash-table code-char))

(defrank :symbol-usage-level22 "symbol-usage-level22"
         '(third atom *package* string-upcase class-of string-downcase
                 slot-boundp pushnew truncate step))

(defrank :symbol-usage-level23 "symbol-usage-level23"
         '(read simple-string round read-char merge-pathnames simple-vector
                random &aux replace directory get))

(defrank :symbol-usage-level24 "symbol-usage-level24"
         '(numberp namestring space nconc eval type-of ignore-errors sqrt
                   remove-if exp minusp))

(defrank :symbol-usage-level25 "symbol-usage-level25"
         '(array-dimension package-name with-accessors locally
                           vector-push-extend princ-to-string mapc terpri
                           &whole integerp subtypep write-byte make-pathname
                           sin))

(defrank :symbol-usage-level26 "symbol-usage-level26"
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

(defrank :symbol-usage-level27 "symbol-usage-level27"
         '(bit cos copy-list caar handler-bind &environment get-universal-time
               some constantp find-symbol close))

(defrank :symbol-usage-level28 "symbol-usage-level28"
         '(copy-seq logbitp probe-file realpart find-if file-position
                    multiple-value-setq endp make-string evenp))

(defrank :symbol-usage-level29 "symbol-usage-level29"
         '(maphash *readtable* ceiling schar read-from-string read-sequence
                   symbol-macrolet imagpart rotatef restart-case))

(defrank :symbol-usage-level30 "symbol-usage-level30"
         '(mapcan phase rational read-line peek-char finish-output
                  *error-output* boundp array-dimensions readtable *features*
                  fboundp))

(defrank :symbol-usage-level31 "symbol-usage-level31"
         '(symbol-value remove-duplicates hash-table-count array-element-type
                        delete values-list constantly write
                        most-positive-fixnum union short-float functionp))

(defrank :symbol-usage-level32 "symbol-usage-level32"
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

(defrank :symbol-usage-level33 "symbol-usage-level33"
         '(long-float pathname-directory intersection multiple-value-call
           mismatch string-trim row-major-aref pathname-type fdefinition open
           make-condition force-output))

(defrank :symbol-usage-level34 "symbol-usage-level34"
         '(rem symbol-function pathname-name complement abort
               multiple-value-prog1 char-equal remf string< signal
               *print-pretty* continue standard-class cdar caddr))

(defrank :symbol-usage-level35 "symbol-usage-level35"
         '(arrayp catch reinitialize-instance sleep print compilation-speed
                  cerror warning get-internal-real-time substitute notinline
                  get-output-stream-string bignum))

(defrank :symbol-usage-level36 "symbol-usage-level36"
         '(*trace-output* write-line char<= fresh-line standard-object
           change-class ratio subst ensure-directories-exist clrhash
           simple-error with-input-from-string satisfies))

(defrank :symbol-usage-level37 "symbol-usage-level37"
         '(fourth prin1-to-string oddp end-of-file invoke-restart *query-io*
                  *debug-io* set-macro-character shared-initialize dpb vectorp
                  count-if compile truename make-load-form
                  make-string-output-stream *standard-input*))

(defrank :symbol-usage-level38 "symbol-usage-level38"
         '(integer-length byte-size declaration macro-function defsetf
                          double-float-epsilon copy-readtable denominator
                          map-into define-modify-macro write-to-string
                          formatter type-error decode-universal-time
                          with-standard-io-syntax char/=
                          internal-time-units-per-second file-length))

(defrank :symbol-usage-level39 "symbol-usage-level39"
         '(numerator rplacd prin1 unread-char char-upcase position-if-not
                     adjust-array pprint-logical-block upper-case-p base-char
                     set-dispatch-macro-character))

(defrank :symbol-usage-level40 "symbol-usage-level40"
         '(alphanumericp bit-vector *print-readably* realp signum
                         simple-vector-p loop-finish file-write-date logtest
                         file-namestring readtable-case make-symbol structure
                         alpha-char-p atan *default-pathname-defaults*
                         char-downcase progv macroexpand-1))

(defrank :symbol-usage-level41 "symbol-usage-level41"
         '(delete-file define-symbol-macro pathnamep encode-universal-time
                       with-simple-restart break describe cdddr muffle-warning
                       cadar string/= generic-function streamp find-restart
                       most-positive-double-float *random-state* find-if-not
                       trace lambda-list-keywords *read-default-float-format*
                       *print-length* user-homedir-pathname macroexpand
                       slot-makunbound))

(defrank :symbol-usage-level42 "symbol-usage-level42"
         '(load acons *print-level* *read-eval* make-broadcast-stream
                stream-element-type fmakunbound shiftf get-setf-expansion tan
                get-macro-character import lower-case-p merge substitute-if
                stable-sort lognot read-delimited-list fifth string-right-trim
                array-rank notany *print-circle* style-warning))

(defrank :symbol-usage-level43 "symbol-usage-level43"
         '(pprint-newline vector-push *print-escape* hash-table-p sbit
                          simple-bit-vector delete-duplicates open-stream-p
                          parse-error inspect delete-if *terminal-io* use-value
                          adjoin get-internal-run-time hash-table-test
                          structure-object make-string-input-stream logandc2
                          make-load-form-saving-slots with-open-stream
                          conjugate directory-namestring *read-suppress*))

(defrank :symbol-usage-level44 "symbol-usage-level44"
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

(defrank :symbol-usage-level45 "symbol-usage-level45"
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

(defrank :symbol-usage-level46 "symbol-usage-level46"
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

(defrank :symbol-usage-level47 "symbol-usage-level47"
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

(defrank :symbol-usage-level48 "symbol-usage-level48"
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
  (:checker (arg &optional op) (declare (ignore op))
   (with-slots (released? name completed? dictionary)
       arg
     (unless released?
       (setf released? t)
       (charms name
               (concatenate 'string (translate:translate "gatcha-aa")
                            #.(format nil "~2%")
                            (format nil
                                    (translate:translate
                                      "dictionary-is-released")
                                    name)
                            #.(format nil "~2%")
                            (translate:translate "explain-:d"))))
     (unless completed?
       (when (dictionary-complete-p dictionary)
         (setf completed? t)
         (charms name
                 (concatenate 'string
                              (translate:translate "congratulations-aa")
                              #.(format nil "~2%")
                              (format nil
                                      (translate:translate
                                        "dictionary-is-completed")
                                      name)))))))
  (:defmacro defdict (name)
   `(let* ((dictionary (find-dictionary ',name))
           (achievement
            (setf (gethash ',name *achievements*)
                    (make-complete-dictionary :name ',name
                                              :dictionary dictionary))))
      (loop :for symbol :being :each :hash-key :of (dictionary-table dictionary)
            :do (setf (symbol-achievements symbol) achievement)))))

(defdict :evaluation-and-compile)

(defdict :types-and-classes)

(defdict :iteration)

(defdict :objects)

(defdict :structure)

(defdict :conditions)

(defdict :packages)

(defdict :numbers)

(defdict :characters)

(defdict :conses)

(defdict :arrays)

(defdict :strings)

(defdict :sequences)

(defdict :hash-tables)

(defdict :pathnames)

(defdict :files)

(defdict :streams)

(defdict :printer)

(defdict :reader)

(defdict :system-constructions)

(defdict :environment)

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
     (charms (achievement-name arg) arg)))
  (:defmacro deffirst (name message)
   `(let ((achievement
           (setf (gethash ',name *achievements*)
                   (make-first-time :name ',name :message ,message))))
      (setf (symbol-achievements ',name) achievement))))

(defmethod charms (name (arg first-time))
  (charms name
          (concatenate 'string (translate:translate "congratulations-aa")
                       #.(format nil "~2%")
                       (translate:translate (achievement-message arg)))))

(deffirst :first-sexp "first-sexp")

(defmethod charms ((name (eql :first-sexp)) (arg first-time))
  (charms name
          (list
            (concatenate 'string (translate:translate "welcome-aa")
                         #.(format nil "~2%")
                         (translate:translate (achievement-message arg)))
            (translate:translate "explain-?")
            (translate:translate "explain-:a"))))

(deffirst :first-error "first-error")

(defmethod charms ((name (eql :first-error)) (arg first-time))
  (charms name
          (list
            (concatenate 'string (translate:translate "congratulations-aa")
                         #.(format nil "~2%")
                         (translate:translate (achievement-message arg)))
            (translate:translate "explain-debugger"))))

(deffirst :first-macro "first-macro")

(deffirst :first-special-operator "first-special-operator")

;;;; TIMES

(defachievement times (count symbol)
  (:printer (stream exp)
   (write
     `(let ((achievement (make-times ,@(kv-args exp))))
        (setf (symbol-achievements ',(times-symbol exp)) achievement))
     :stream stream))
  (:checker (arg &optional op)
   (with-slots (completed? count message name)
       arg
     (unless completed?
       (when (= (symbol-times op) count)
         (setf completed? t)
         (charms name arg)))))
  (:defmacro deftimes (name op count message)
   `(let ((achievement
           (setf (gethash ',name *achievements*)
                   (make-times :name ',name
                               :message ,message
                               :count ,count
                               :symbol ',op))))
      (setf (symbol-achievements ',op) achievement))))

;; level1

(deftimes :first-time-defun defun 1 "first-time-defun")

(deftimes :setf-philia setf 30 "setf-philia")

;; level2

(deftimes :first-time-declare declare 1 "first-time-declare")

(deftimes :declare-philia declare 30 "declare-philia")

(deftimes :loop-philia loop 30 "loop-philia")

(deftimes :first-time-defmethod defmethod 1 "first-time-defmethod")

;; level3

(deftimes :lambda-master lambda 30 "lambda-master")

(deftimes :format-black-belt format 30 "format-black-belt")

(deftimes :first-time-&key &key 1 "first-time-&key")

(deftimes :first-time-values values 1 "first-time-values")

(deftimes :first-time-in-package in-package 1 "first-time-in-package")

(deftimes :nomad in-package 50 "nomad")

;; level4

(deftimes :first-time-&optional &optional 1 "first-time-&optional")

(deftimes :first-time-&rest &rest 1 "first-time-&rest")

(deftimes :first-time-defgeneric defgeneric 1 "first-time-defgeneric")

;; level5

(deftimes :first-time-defmacro defmacro 1 "first-time-defmacro")

(deftimes :first-time-defconstant defconstant 1 "first-time-defconstant")

;; level6

(deftimes :first-time-defclass defclass 1 "first-time-defclass")

(deftimes :first-time-defvar defvar 1 "first-time-defvar")

;; level7

(deftimes :setq-principleists setq 30 "setq-principleists")

;; level9

(deftimes :first-time-defparameter defparameter 1 "first-time-defparameter")

;; level11

(deftimes :first-time-defpackage defpackage 1 "first-time-defpackage")

;; level12

(deftimes :first-time-define-condition define-condition 1
          "first-time-define-condition")

;; level14
;; level15

(deftimes :first-time-defstruct defstruct 1 "first-time-defstruct")

(deftimes :first-time-eval-when eval-when 1 "first-time-eval-when")

;; level17

(deftimes :first-time-deftype deftype 1 "first-time-deftype")

;; level26

(deftimes :first-time-define-compiler-macro define-compiler-macro 1
          "first-time-define-compiler-macro")

;;;; TIPS

(defachievement tips (count symbol)
  (:printer (stream exp)
   (write
     `(let ((achievement (make-tips ,@(kv-args exp))))
        (setf (symbol-achievements ',(tips-symbol exp)) achievement))
     :stream stream))
  (:checker (arg &optional op)
   (with-slots (completed? count name)
       arg
     (unless completed?
       (when (= (symbol-times op) count)
         (setf completed? t)
         (charms :added arg)))))
  (:defmacro deftips (op count name)
   `(let ((achievement
           (setf (gethash ',name *achievements*)
                   (make-tips :name ',name
                              :message ,(string-downcase name)
                              :count ,count
                              :symbol ',op))))
      (setf (symbol-achievements ',op) achievement))))

(defmethod charms ((name (eql :added)) (arg tips))
  (charms name
          (concatenate 'string (translate:translate "tips-is-added")
                       #.(format nil "~2%")
                       (translate:translate "explain-:t"))))

(deftips setf 20 :setf-can-accepts-some-places)

(defmethod charms ((name (eql :setf-can-accepts-some-places)) (arg tips))
  (charms name
          (translate:translate (achievement-message arg))))
