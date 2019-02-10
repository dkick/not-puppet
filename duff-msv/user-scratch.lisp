(in-package :net.earthlink.dkixk.duff-msv-user)

(defun duff-duff (subscriber-validator)
  (with-open-file (in "test1.lisp")
    (loop for message = (read-message in nil)
          while message do (handle-event subscriber-validator message)))
  subscriber-validator)

