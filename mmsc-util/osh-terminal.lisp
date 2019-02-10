(in-package :com.github.dkick.mmsc-util)

(defclass osh-shell (generic-shell) ())

(defun make-osh-shell () (make-instance 'osh-shell))

(defmethod make-subterminal ((shell (eql 'osh-shell))
                             (supershell tacl-shell)
                             stream &key start-osh)
  (declare (ignore start-osh))
  (cmd-do* supershell stream
           (use concatenate 'string
                "#changeuser super.webmastr" (string #\Space)
                *default-super-webmastr-password*))
  (make-instance 'generic-pseudo-subterminal
                 :supershell supershell :shell (make-osh-shell)
                 :stream stream))

(defmethod init-subshell ((shell osh-shell) (supershell tacl-shell)
                          stream &key)
  ;; The following command will execute the command in a tacl-shell
  ;; but wait-for the prompt of an osh-shell.
  (cmd-do* shell stream "osh"))

(defmethod cmd-exit* ((shell osh-shell) (stream stream))
  (format stream "~&exit~%"))

(defmethod cmd-exit* ((shell osh-shell) (stream generic-telnet-stream))
  (send-line "exit" stream))

(defmethod cmd-exit-subshell* :before ((shell osh-shell)
                                       (supershell tacl-shell)
                                       (stream generic-telnet-stream))
  (wait-for (shell-prompt supershell) stream))

(defmethod cmd-exit-subshell* ((shell osh-shell) (supershell tacl-shell)
                               (stream t))
  (cmd-do* supershell stream
           (use concatenate 'string
                "#changeuser" (string #\Space) (snet-username *overrides*)
                (string #\Space) (snet-password *overrides*))))

(defmethod shell-prompt ((shell osh-shell))
  (concatenate 'string (string #\$) (string #\Space)))
