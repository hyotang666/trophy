; vim: ft=lisp et
(in-package :asdf)
(defsystem "trophy.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "trophy")
  :components
  ((:file "trophy"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :trophy args)))