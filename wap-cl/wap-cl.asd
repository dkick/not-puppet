; -*- lisp -*-

(defpackage :net.earthlink.dkixk.wap-cl.system (:use :asdf :cl))
(in-package :net.earthlink.dkixk.wap-cl.system)

(defsystem :wap-cl
  :serial t
  :depends-on (:uncommon-lisp)
  :components ((:file "packages")
               (:file "binary-data")
               (:file "generic-message-parts")
               (:file "wap-message-parts")
               (:file "content-type")
               (:file "wap-message")))
