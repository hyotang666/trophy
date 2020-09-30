(in-package :cl-user)

(defpackage :trophy
  (:use :cl)
  (:export ;;;; main api
           #:repl
           #:set-env
           ;;;; For developer.
           ;;; dictionary
           #:defdictionary
           #:find-dictionary
           #:dictionary-table
           #:dictionary-complete-p
           #:symbol-times
           #:symbol-dictionary
           ;;; achievement
           ;; Type name.
           #:achievement
           ;; Slot names.
           #:name
           #:message
           #:complited?
           ;; Accessors.
           #:achievement-name
           #:achievemetn-message
           #:achievement-complited?
           ;;; DSL
           #:defachievement
           #:charms
           #:symbol-achievements
           #:check-achievement))
