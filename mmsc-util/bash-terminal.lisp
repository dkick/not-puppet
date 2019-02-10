(in-package :com.github.dkick.mmsc-util)

(defclass bash-shell (generic-shell) ())

(defun make-bash-shell () (make-instance 'bash-shell))

(defmethod make-terminal ((shell (eql 'bash-shell)) (stream (eql t)) &key)
  (make-terminal shell *standard-output*))

(defmethod make-terminal ((shell (eql 'bash-shell)) (stream stream) &key)
  (make-instance 'generic-pseudo-terminal
                 :shell (make-bash-shell) :stream stream))

(defmethod cmd-cd-volume-tag* ((shell bash-shell) (stream stream)
                               volume tag)
  (let ((subvolume (concatenate 'string (string #\r) tag "exe")))
    (cmd-do* shell stream
             (use concatenate 'string "cd" (string #\Space)
                  (string #\$) volume (string #\.) subvolume))))

(defmethod cmd-put-exec* ((shell bash-shell) (stream stream)
                          exec-dir exec-name)
  (cmd-do* shell stream (format nil "cd ~A"
                                (namestring/cygwin exec-dir)))
  (cmd-do* shell stream
           (format nil "/usr/bin/perl ~A -ignore ~A R~AEXE ~A BI ~A"
                   (namestring/cygwin (tsend-pathname *overrides*))
                   (string-upcase (snet-hostname *overrides*))
                   (string-upcase (snet-tag *overrides*))
                   (string-upcase (snet-volume *overrides*))
                   exec-name)))
