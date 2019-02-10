(in-package :cl-user)

;; I'm going to want to use this at some point for dealing with a duff
;; MSV which deals with multiple simultaneous client requests.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process))

(defpackage :net.earthlink.dkixk
  (:use :common-lisp :uncommon-lisp))

(defpackage :net.earthlink.dkixk.duff-msv
  (:use :common-lisp :uncommon-lisp)
  (:export :read-message :write-message :handle-event
           :subscriber-validation-request :subscriber-validation-response
           :make-ldap-search-request :ldap-search-response))

(defpackage :net.earthlink.dkixk.duff-msv-user
  (:use :common-lisp :uncommon-lisp :net.earthlink.dkixk.duff-msv))
