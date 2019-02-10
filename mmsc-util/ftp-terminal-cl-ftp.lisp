(in-package :com.github.dkick.mmsc-util)

(defclass ftp-shell (generic-shell) ())

(defun make-ftp-shell () (make-instance 'ftp-shell))

(defmethod make-terminal ((shell (eql 'ftp-shell)) (stream null) &key)
  (let* ((stream (use make-instance 'ftp.client:ftp-connection
                      :hostname (snet-hostname *overrides*)
                      :username (snet-username *overrides*)
                      :password (snet-password *overrides*)))
         (it (make-instance 'generic-pseudo-terminal
                            :shell (make-ftp-shell)
                            :stream stream)))
    (use ftp.client:send-cwd-command stream
         (format nil "$~A.r~Aexe"
                 (snet-volume *overrides*)
                 (snet-tag *overrides*)))
    it))

(defmethod cmd-exit* ((shell ftp-shell)
                      (stream ftp.client:ftp-connection))
  (ftp.client:close-connection stream))

(defmethod cmd-put-exec* ((shell ftp-shell)
                          (stream ftp.client:ftp-connection)
                          exec-dir exec-name)
  (ftp.client:store-file stream
                         (make-pathname :name exec-name
                                        :defaults exec-dir)
                         (concatenate 'string exec-name ",700")))
