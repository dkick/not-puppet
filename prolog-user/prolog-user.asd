; -*- lisp -*-

(in-package :cl-user)

(asdf:defsystem :prolog-user
  :components
  ((:file "packages")
   (:file "ext" :depends-on ("packages"))
   (:file "duff-config" :depends-on ("packages" "ext"))))
