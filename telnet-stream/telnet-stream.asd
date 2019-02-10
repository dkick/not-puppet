(defpackage :net.earthlink.dkixk.telnet-stream.system
  (:use :cl :ul :asdf))

(in-package :net.earthlink.dkixk.telnet-stream.system)

(defsystem :telnet-stream
  :depends-on
  (:uncommon-lisp :cl-ppcre :trivial-gray-streams :flexi-streams)
  :components
  ((:file "packages")
   (:file "byte-code" :depends-on ("packages"))
   (:file "misc" :depends-on ("packages" "byte-code"))
   (:file "io-send-recv" :depends-on ("packages" "byte-code" "misc"))
   (:file "io-gray" :depends-on ("packages" "misc" "io-send-recv"))
   (:file "prefer" :depends-on ("packages" "misc" "io-send-recv"))))
