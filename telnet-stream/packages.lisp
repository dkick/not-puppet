(in-package :cl-user)

#+allegro
(eval-when (:compile-toplevel :load-toplevel)
  (require :iodefs)
  (require :socket))

(defpackage :net.earthlink.dkixk.telnet-stream
  (:use :cl :ul :trivial-gray-streams
        #+allegro :socket
        #+clisp :acl-compat.socket)
  (:export :telnet :recv-byte :recv-char :send-byte :send-char
           :send-line :wait-for :with-telnet :generic-telnet-stream
           :telnet-quasi-stream :telnet-gray-stream :*default-echo-fn*
           :*default-recv-char-fn* :*default-telnet-stream-type*))

(defpackage :net.earthlink.dkixk.telnet-stream.user
  (:use :cl :ul :net.earthlink.dkixk.telnet-stream
        #+allegro :socket
        #+clisp :acl-compat.socket))
