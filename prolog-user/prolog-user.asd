; -*- lisp -*-

(in-package :cl-user)

(asdf:defsystem :prolog-user
  :components
  ((:file "packages")
   (:file "ext" :depends-on ("packages"))
   (:file "kick-family" :depends-on ("packages" "ext"))
   (:file "duff-config" :depends-on ("packages" "ext"))))
