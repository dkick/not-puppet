(in-package :cl-user)

(defpackage :net.earthlink.dkixk
  (:use :common-lisp :uncommon-lisp))

(defpackage :net.earthlink.dkixk.binary-data
  (:use :common-lisp :uncommon-lisp)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :read-object
           :write-object
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :swap-bytes
           :+null+))

(defpackage :net.earthlink.dkixk.generic-message-parts
  (:use :common-lisp :uncommon-lisp :net.earthlink.dkixk.binary-data)
  (:export :error*
           :encoding-error
           :not-coded-error
           :uint8
           :uint16
           :uint24
           :uint32
           :iso-8859-1-char
           :iso-8859-1-string
           :iso-8859-1-terminated-string
           :ucs-2-char-big-endian
           :ucs-2-char-little-endian
           :ucs-2-string
           :ucs-2-terminated-string))

(defpackage :net.earthlink.dkixk.wap-message
  (:use :common-lisp :uncommon-lisp :net.earthlink.dkixk.binary-data
        :net.earthlink.dkixk.generic-message-parts)
  (:export :uint-var
           :generic-fixed-size-buffer
           :generic-wsp-value
           :wsp-parameter
           :name
           :value))

(defpackage :net.earthlink.dkixk.wap-cl
  (:use :common-lisp :uncommon-lisp :net.earthlink.dkixk.binary-data
        :net.earthlink.dkixk.wap-message))

(defpackage :net.earthlink.dkixk.wap-cl-user
  (:use :common-lisp :uncommon-lisp :net.earthlink.dkixk.wap-cl
        :net.earthlink.dkixk.binary-data
        :net.earthlink.dkixk.generic-message-parts
        :net.earthlink.dkixk.wap-message))
