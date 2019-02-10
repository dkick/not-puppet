; -*- lisp -*-

(defpackage :net.earthlink.dkixk.duff-msv.system
  (:use :common-lisp :asdf))
(in-package :net.earthlink.dkixk.duff-msv.system)

(defsystem :duff-msv
  :serial t
  :depends-on (:uncommon-lisp)
  :components ((:file "packages")
               (:file "messages")
               (:file "state-machine")
               (:file "scratch")
               (:file "user-scratch")))
