(in-package :com.github.dkick.mmsc-util)

(defclass tacl-shell (generic-shell)
    ((volume :initarg :volume :accessor tacl-volume)
     (subvolume :initarg :subvolume :accessor tacl-subvolume)))

(defun make-tacl-shell (volume subvolume)
  (make-instance 'tacl-shell :volume volume :subvolume subvolume))

(defmethod make-terminal ((shell (eql 'tacl-shell)) (stream (eql 'telnet))
                          &key)
  (with-accessors ((hostname snet-hostname)
                   (username snet-username)
                   (password snet-password))
      *overrides*
    (assert (equalp username "node.mgr"))
    (let* ((shell (make-tacl-shell "$APPL1" "MGR"))
           (where (telnet hostname))
           (it (make-instance 'generic-pseudo-terminal
                              :shell shell :stream where)))
      (wait-for "Enter Choice> " where)
      (send-line "TACL" where)
      (wait-for (list :sequence "TACL " :digits
                      (coerce '(#\> #\Space) 'string))
                where)
      (send-line (concatenate 'string "logon " username) where)
      (wait-for "Password: " where)
      (send-line password where)
      (wait-for (shell-prompt shell) where)
      (cmd-do it "$appl1.aaamm.mmtbldef")
      it)))

(defmethod make-terminal ((shell (eql 'tacl-shell)) (stream stream) &key)
  (make-instance 'generic-pseudo-terminal
                 :shell (make-tacl-shell "$APPL1" "MGR")
                 :stream stream))

(defmethod shell-prompt ((shell tacl-shell))
  (with-accessors ((volume tacl-volume)
                   (subvolume tacl-subvolume))
      shell
    (list :sequence
          (use concatenate 'string
               (string-upcase volume) (string #\Space)
               (string-upcase subvolume) (string #\Space))
          :digits
          (coerce '(#\> #\Space) 'string))))

(defmethod cmd-cd* ((shell tacl-shell) stream raw-path)
  (multiple-value-bind (volume subvolume)
      (ensure-path shell raw-path)
    (cmd-do* (make-tacl-shell volume subvolume) stream
             (use concatenate 'string
                  "cd" (string #\Space) volume (string #\.) subvolume))
    (setf (tacl-volume shell) volume
          (tacl-subvolume shell) subvolume)))

(defmethod ensure-path ((shell tacl-shell) (raw-path string))
  (error "<grumble> Now I have to write ensure-path for a STRING."))

(defmethod ensure-path ((shell tacl-shell) (raw-path list))
  (destructuring-bind (type volume/t subvolume/t) raw-path
    (assert (eq type :absolute))
    (let ((volume/string (string volume/t))
          (subvolume/string (string subvolume/t)))
      (values volume/string subvolume/string))))

(defmethod cmd-cd-volume-tag* ((shell tacl-shell) stream volume tag)
  (cmd-cd* shell stream
           (list :absolute
                 (concatenate 'string (string #\$) volume)
                 (concatenate 'string (string #\r) tag "exe"))))

(defmethod cmd-wait-for-delmgr* ((shell tacl-shell) (stream stream)
                                 volume)
  (cmd-do* shell stream
           (format nil "status *,prog $~A.mmexec.delmgr*" volume)))

(defmethod exec-backup-name ((shell tacl-shell) exec-name)
  (cond ((< (length exec-name) 8)
         (concatenate 'string exec-name "o"))
        ((= (length exec-name) 8)
         (concatenate 'string (subseq exec-name 0 7)
                      (if (eql (schar exec-name 7) #\o) "z" "o")))
        (t (error "~S is not a valid Tandem file name.  ~
                   It is ~A characters long but should be <= 8 characters ~
                   long."
                  exec-name (length exec-name)))))
