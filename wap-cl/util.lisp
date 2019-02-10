(in-package :pg-user)

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equal)

(defparameter *wap-cl-packages*
  (let ((base (symbol-name :net.earthlink.dkixk)))
    (mapcar #'(lambda (x)
                (concatenate 'string base "." (symbol-name x)))
            '(wap-cl-user wap-cl wap-message generic-message-parts
              binary-data))))

(defun clear-all-wap-cl-packages ()
  (loop for package in (mapcar #'find-package *wap-cl-packages*)
        when package do (delete-package package)))

(defun reload-all-wap-cl-packages ()
  (clear-all-wap-cl-packages)
  (asdf:oos 'asdf:load-op
            (asdf:find-component (asdf:find-system :wap-cl) "packages")
            :force t))

(defun reload-wap-cl-system ()
  (clear-all-wap-cl-packages)
  (asdf:oos 'asdf:load-op :wap-cl :force t))
