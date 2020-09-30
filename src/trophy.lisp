(in-package :trophy)

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
    ":q"
  (throw 'quit (values)))

(define-special-command ?
    "?"
  (let ((max
         (loop :for key :being :each :hash-key :of *special-commands*
               :maximize (length (prin1-to-string key)))))
    (loop :for (description . nil) :being :each :hash-value :of
               *special-commands* :using (:hash-key command)
          :do (format t "~%[~VS] :~A" max command
                      (translate:translate description)))
    (values)))

(define-special-command :a
    ":a"
  (let ((size (hash-table-count *achievements*))
        (completed
         (loop :for achievement :being :each :hash-value :of *achievements*
               :if (achievement-completed? achievement)
                 :collect (achievement-name achievement))))
    (funcall (formatter "~<~&~:I~D/~D ~A~:@_~/cl:pprint-tabular/~:@_~:>")
             *standard-output*
             (list (length completed) size
                   (translate:translate "achievements-are-done") completed)))
  (values))

(define-special-command :d
    ":d"
  (prog* ((unknown :?????)
          (known-dictionaries
           (loop :for name :being :each :hash-key :of *dictionaries*
                 :for achievement
                      := (or (gethash name *achievements*)
                             (error "Missing achievement of dict: ~S" name))
                 :if (complete-dictionary-released? achievement)
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
             (translate:translate "check-dictionary?"))))
      (if (eq :q name)
          (return (values))
          (progn
           (print-dictionary-information (find-dictionary name))
           (if (y-or-n-p (translate:translate "check-others?"))
               (go :top)
               (return (values))))))))

(defun print-dictionary-information (dictionary)
  (funcall (formatter "~%~{~50<~S~; ~D times used.~%~>~}") nil
           (loop :for symbol :being :each :hash-key :of
                      (dictionary-table dictionary) :using (:hash-value count)
                 :collect symbol
                 :collect count))
  (force-output))

(define-special-command :l
    ":l"
  (setf translate:*language* (query-language))
  (values))

(define-special-command :t
    ":t"
  (prog ((tips
          (loop :for achievement :being :each :hash-value :of *achievements*
                :if (and (tips-p achievement)
                         (achievement-completed? achievement))
                  :collect (achievement-name achievement))))
    (if tips
        (funcall (formatter "~<~:@_~:I~@/pprint-tabular/~:@_~:>") nil
                 (list tips))
        (format t (translate:translate "no-tips")))
    (force-output)
    (unless tips
      (return (values)))
   :top
    (let ((name
           (prompt-for:prompt-for `(member :q ,@tips)
                                  (translate:translate "see-tips?"))))
      (if (eq :q name)
          (return (values))
          (progn
           (charms name (gethash name *achievements*))
           (if (y-or-n-p (translate:translate "check-others?"))
               (go :top)
               (return (values))))))))

;;;; REPL

(defun trophy-read (&optional (*standard-input* *query-io*))
  (let ((*standard-output* *query-io*))
    (format t "~%TROPHY> ")
    (force-output)
    (read)))

(defun trophy-eval (exp &optional non-toplevel-p)
  (check-achievement :first-sexp exp)
  (let* ((*debugger-hook*
          (lambda (condition hook)
            (declare (ignore condition hook))
            (check-achievement :first-error)))
         (results
          (multiple-value-list
           (cond
            ((atom exp)
             (if (and (get-special-command exp) (not non-toplevel-p))
                 (return-from trophy-eval (comcall exp))
                 (eval exp)))
            ((and (symbolp (car exp)) (special-operator-p (car exp)))
             (check-achievement :first-special-operator exp)
             (check-achievement exp (car exp))
             (eval exp))
            ((and (symbolp (car exp)) (macro-function (car exp)))
             (check-achievement :first-macro exp)
             (check-achievement exp (car exp))
             ;; We do not treat expanded code as your achievements.
             (eval (macroexpand exp)))
            (t
             (check-achievement exp (car exp))
             (destructuring-bind
                 (op . args)
                 exp
               (let ((args (mapcar (lambda (x) (trophy-eval x t)) args)))
                 (apply op args))))))))
    (shiftf +++ ++ + exp)
    (shiftf *** ** * (car results))
    (shiftf /// // / results)
    (values-list results)))

(defun trophy-print (&rest values)
  (map nil #'print values)
  (force-output)
  (values))

(defvar *user-name* nil)

(defvar *trophy-package* (find-package :cl-user))

(defun query-language ()
  (let ((langs
         (loop :for lang :in translate::*translations* :by #'cddr
               :collect lang)))
    (prompt-for:prompt-for `(member ,@langs)
                           "~&~{~W~^ ~}~%Which language do you want? >> "
                           langs)))

(defun language ()
  (let ((*package* (find-package :keyword)))
    (values (read-from-string (system-locale:language "en" "ja")))))

(defun repl (user-name)
  (if (probe-file
        (merge-pathnames (string-downcase user-name) +users-directory+))
      (load-user user-name)
      (cerror "Make new user." "No such user: ~S" user-name))
  (let ((*user-name* user-name)
        (*trophy-package* *package*)
        (translate:*language* (or translate:*language* (language))))
    (unwind-protect
        (catch 'quit
          (loop (restart-case (multiple-value-call #'trophy-print
                                (trophy-eval (trophy-read)))
                  (abort () :report "Return to trophy repl."))))
      (save user-name))))

(defun trophy-walk (form)
  (check-achievement :first-sexp form)
  (trestrul:traverse
    (lambda (x)
      (cond #+sbcl
            ((sb-int:comma-p x) (trophy-walk (sb-int:comma-expr x)))
            (t
             (cond ;; To ignore NIL which is in the end of a proper list.
                   ((null x) x)
                   ((and (symbolp x) (special-operator-p x))
                    (check-achievement :first-special-operator form)
                    (check-achievement x))
                   ((and (symbolp x) (macro-function x))
                    (check-achievement :first-macro form)
                    (check-achievement x))
                   ((symbolp x) (check-achievement x))
                   ;; To capture NIL which is not in the end of a proper list.
                   ((and (consp x) (null (car x))) (check-achievement nil))
                   (t x))))) ; do nothing.
    form))

(defun macroexpand-hook (expander form env)
  (when (and (eq *package* *trophy-package*) (null env)) ; Top level form.
    (trophy-walk form))
  (funcall expander form env))

(defun debugger-hook (condition hook)
  (declare (ignore condition hook))
  (check-achievement :first-error))

(defun set-env (user-name)
  (if user-name
      (progn
       (if (probe-file
             (merge-pathnames (string-downcase user-name) +users-directory+))
           (load-user user-name)
           (cerror "Make new user." "No such user: ~S" user-name))
       (setf *user-name* user-name)
       (setf *trophy-package* *package*)
       (or translate:*language* (setf translate:*language* (language)))
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