(in-package :com.github.dkick.mmsc-util)

(defmacro with-terminal ((terminal &rest args) &body forms)
  ` (let ((,terminal (make-terminal ,@args)))
      (unwind-protect
           (progn ,@forms)
        (when ,terminal
          (cmd-exit ,terminal)))))

(defmacro with-subterminal ((terminal &rest args) &body forms)
  ` (let ((,terminal (make-subterminal ,@args)))
      (unwind-protect
           (progn ,@forms)
        (when ,terminal
          (cmd-exit ,terminal)))))

(defmacro with-terminals ((&rest args) &body forms)
  (reduce #'(lambda (arg accumulator)
              ` (with-terminal ,arg ,accumulator))
          args :from-end t :initial-value ` (progn ,@forms)))

(defmacro with-subterminals ((&rest args) &body forms)
  (reduce #'(lambda (arg accumulator)
              ` (with-subterminal ,arg ,accumulator))
          args :from-end t :initial-value ` (progn ,@forms)))

(defclass generic-shell () ())

(defclass generic-pseudo-terminal ()
    ((shell :initarg :shell :accessor terminal-shell)
     ;; Not everything will have a stream.  For example, something
     ;; which used ftp-get would not have a stream.  However, most
     ;; will.
     (stream :initform nil :initarg :stream :accessor terminal-stream)
     (close-stream-p :initform t :initarg :close-stream-p
                     :accessor terminal-close-stream-p)))

(defclass generic-pseudo-subterminal (generic-pseudo-terminal)
    ((close-stream-p :initform nil)
     (supershell :initarg :supershell
                 :accessor terminal-supershell)))

(defmethod shared-initialize :after ((object generic-pseudo-subterminal)
                                     slot-names &rest initargs
                                     &key (allow-other-keys nil))
  (declare (ignore slot-names initargs allow-other-keys))
  (with-accessors ((shell terminal-shell)
                   (supershell terminal-supershell)
                   (stream terminal-stream))
      object
    (init-subshell shell supershell stream)))

(defmethod cmd-do ((terminal generic-pseudo-terminal) cmd-what)
  (with-accessors ((shell terminal-shell)
                   (stream terminal-stream))
      terminal
    (cmd-do* shell stream cmd-what)))

(defmethod cmd-do* ((shell generic-shell) (stream stream) cmd-what)
  (format stream "~&~A~%" cmd-what))

(defmethod cmd-do* ((shell generic-shell) (stream generic-telnet-stream)
                    cmd-what)
  (send-line cmd-what stream)
  (wait-for (shell-prompt shell) stream))

(defmethod cmd-exit ((terminal generic-pseudo-terminal))
  (with-accessors ((shell terminal-shell) (stream terminal-stream)
                   (close-stream-p terminal-close-stream-p))
      terminal
    (cmd-exit* shell stream)
    (when (and stream close-stream-p)
      (cmd-exit-close-stream stream))
    (setq shell nil stream nil close-stream-p nil)))

(defmethod cmd-exit-close-stream (stream)
  (declare (ignore stream))
  (values))

(defmethod cmd-exit-close-stream ((stream stream))
  (when (notany #'(lambda (cl-stream) (eq stream cl-stream))
                (list *debug-io* *error-output* *query-io*
                      *standard-input* *standard-output*
                      *terminal-io* *trace-output*))
    (close stream)))

(defmethod cmd-exit ((terminal generic-pseudo-subterminal))
  (with-accessors ((shell terminal-shell) (stream terminal-stream)
                   (supershell terminal-supershell))
      terminal
    (cmd-exit* shell stream)
    (cmd-exit-subshell* shell supershell stream)))

(defmethod cmd-exit* (shell stream)
  "Default is to do nothing special."
  (declare (ignore shell stream))
  (values))

(defmethod cmd-send-execs ((terminal generic-pseudo-terminal)
                           execs)
  (loop for exec-name in execs do
        (cmd-put-exec terminal (exec-directory-pathname exec-name)
                      exec-name)))

(defmethod cmd-put-exec ((terminal generic-pseudo-terminal)
                         exec-dir exec-name)
  (with-accessors ((shell terminal-shell)
                   (stream terminal-stream))
      terminal
    (cmd-put-exec* shell stream exec-dir exec-name)))

;; This method doesn't really make sense for a generic-pseduo-terminal
;; as it really only applies to a TACL shell.
(defmethod cmd-sql-compiles ((terminal generic-pseudo-terminal)
                             execs)
  (cmd-set-param terminal "sqlvol" (sql-volume *overrides*))
  (cmd-cd-volume-tag terminal
                     (snet-volume *overrides*) (snet-tag *overrides*))
  (loop for exec in execs do
        (cmd-do terminal (format nil "sqlc ~A" exec))
        (when (some #'(lambda (x) (equalp exec x))
                    '("cgi" "fwdcgi" "provcgi"))
          (cmd-do terminal (format nil "fup secure ~A,,progid" exec)))
        (when (equalp exec "dist")
          (cmd-do terminal
                  (use concatenate 'string
                       "#changeuser super.super" (string #\Space)
                       *default-super-super-password*))
          (cmd-do terminal "fup give dist, super.super")
          (cmd-do terminal "fup secure dist, NNNN, progid")
          (cmd-do terminal
                  (use concatenate 'string
                       "#changeuser" (string #\Space)
                       (snet-username *overrides*) (string #\Space)
                       (snet-password *overrides*))))))

(defmethod cmd-cd ((terminal generic-pseudo-terminal) path)
  (with-accessors ((shell terminal-shell) (stream terminal-stream))
      terminal
    (cmd-cd* shell stream path)))

(defmethod cmd-cd-volume-tag ((terminal generic-pseudo-terminal)
                              volume tag)
  (with-accessors ((shell terminal-shell) (stream terminal-stream))
      terminal
    (cmd-cd-volume-tag* shell stream volume tag)))

(defmethod cmd-set-param ((terminal generic-pseudo-terminal)
                          param-name param-value)
  (cmd-do terminal
          (concatenate 'string
                       "#set #param " param-name (string #\Space)
                       param-value)))

(defmethod cmd-backup-mmexec ((terminal generic-pseudo-terminal)
                              execs)
  (with-accessors ((shell terminal-shell)) terminal
    (loop for exec in execs do
          (cmd-do terminal
                  (format nil "mv $~A.mmexec.~A $~A.mmexec.~A"
                          (string-downcase (snet-volume *overrides*))
                          exec
                          (string-downcase (snet-volume *overrides*))
                          (exec-backup-name shell exec))))))

(defmethod cmd-move-execs-to-mmexec ((terminal generic-pseudo-terminal)
                                     execs)
  (loop for exec in execs do (cmd-move-exec-to-mmexec terminal exec)))

(defmethod cmd-move-exec-to-mmexec ((terminal generic-pseudo-terminal)
                                    exec)
  (cmd-do terminal
          (format nil "mv $~A.r~Aexe.~A $~A.mmexec.~A"
                  (string-downcase (snet-volume *overrides*))
                  (string-downcase (snet-tag *overrides*))
                  exec
                  (string-downcase (snet-volume *overrides*))
                  exec)))

;; We only want to bother to restart the web-server if we have an
;; executable for which this is needed, i.e. cgi.
(defmethod cmd-restart-web-server ((terminal generic-pseudo-terminal)
                                   execs &key start-osh)
  (when (member "cgi" execs :test #'equal)
    (with-accessors ((shell terminal-shell) (stream terminal-stream))
        terminal
      (with-subterminal (osh 'osh-shell shell stream :start-osh start-osh)
        (cmd-do osh "cd /usr/tandem/webserver/conf")
        (cmd-do osh "./stop")
        (cmd-do osh "./start")))))

(defmethod cmd-stop-mmexec-start-execs ((terminal generic-pseudo-terminal))
  (cmd-do terminal
          (format nil "status *,prog $~A.mmexec.*o,stop"
                  (string-downcase (snet-volume *overrides*))))
  (cmd-wait-for-delmgr terminal (snet-volume *overrides*)))

(defmethod cmd-wait-for-delmgr ((terminal generic-pseudo-terminal)
                                volume)
  (with-accessors ((shell terminal-shell)
                   (stream terminal-stream))
      terminal
    (cmd-wait-for-delmgr* shell stream volume)))

;; Emacs only indents correctly when I use '(shell t)', as opposed to
;; just 'shell'.
(defmethod cmd-wait-for-delmgr* ((shell t) (stream generic-telnet-stream)
                                 volume)
  (warn "Not implemented: ~S with ~S.  Wait for it \"manually\"."
        (find-method #'cmd-wait-for-delmgr* nil
                     (mapcar #'find-class '(t generic-telnet-stream t)))
        (mapcar #'(lambda (x) (class-name (class-of x)))
                (list shell stream volume))))

(defmethod cmd-move-execs-from-mmexec ((terminal generic-pseudo-terminal)
                                       execs)
  (loop for exec in execs do
        (cmd-do terminal
                (format nil "mv $~A.mmexec.~A $~A.r~Aexe.~A"
                        (string-downcase (snet-volume *overrides*))
                        exec
                        (string-downcase (snet-volume *overrides*))
                        (string-downcase (snet-tag *overrides*))
                        exec))))

(defmethod cmd-restore-mmexec ((terminal generic-pseudo-terminal)
                               execs)
  (with-accessors ((shell terminal-shell)) terminal
    (loop for exec in execs do
          (cmd-do terminal
                  (format nil "mv $~A.mmexec.~A $~A.mmexec.~A"
                          (string-downcase (snet-volume *overrides*))
                          (exec-backup-name shell exec)
                          (string-downcase (snet-volume *overrides*))
                          exec)))))

(defmethod cmd-stop-execs ((terminal generic-pseudo-terminal))
  (cmd-do terminal (format nil "status *,prog $~A.r~Aexe.*,stop"
                           (string-downcase (snet-volume *overrides*))
                           (string-downcase (snet-tag *overrides*)))))
