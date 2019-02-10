(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :prolog))

(defpackage :prolog-user (:use :cl :ul :prolog))
