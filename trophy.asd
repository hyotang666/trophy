; vim: ft=lisp et
(in-package :asdf)
(defsystem "trophy"
  :version
  "0.10.1"
  :depends-on
  (
   "closer-mop" ; Wrapper of Meta-Object-Protocols.
   "trestrul" ; TREeSTRUcturedList utilities.
   "prompt-for" ; Type safe user input.
   "alexandria" ; Public domain utilities.
   "translate" ; Internatinalization.
   "system-locale" ; System locale discovery.
   "cffi" ; Currently lem-setlocale/cffi does not have cffi in its depends-on.
   "lem-setlocale/cffi" ; setlocale for ncurses initialization.
   ;; "cl-charms" ; Bindings for ncurses.
   "babel" ; For STRING-SIZE-IN-OCTETS.
   )
  :pathname
  "src/"
  :components
  ((:file "package")
   (:module "languages"
            :components
            ((:file "english")
             (:file "japanese"))
            :depends-on ("package"))
   (:file "dictionary" :depends-on ("package"))
   (:file "achievement" :depends-on ("package" "dictionary"))
   (:file "trophy" :depends-on ("package" "achievement" "dictionary" "languages"))))

;; Guard.
(defmethod perform :after ((o load-op) (c (eql (find-system "trophy"))))
  (when (find "cffi" (system-depends-on (find-system "lem-setlocale/cffi"))
              :test #'equal)
    (warn "Fix trophy asd to remove cffi.")))

;; Before compiling cl-charms locale must be set.
(defmethod perform :after ((o load-op) (c (eql (find-system "lem-setlocale/cffi"))))
  (symbol-call "LEM-SETLOCALE/CFFI" "SETLOCALE"
               (symbol-value (find-symbol* "+LC-ALL+" "LEM-SETLOCALE/CFFI"))
               "en_US.UTF-8"))

;; Ensure load cl-charms after setlocale.
(defmethod component-depends-on ((o load-op) (c (eql (find-system "lem-setlocale/cffi"))))
  (append (call-next-method) '((load-op "cl-charms"))))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "trophy").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "trophy"))))
  (append (call-next-method) '((test-op "trophy.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "trophy")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "trophy"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
