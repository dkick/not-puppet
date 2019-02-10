(in-package :com.github.dkick.mmsc-util)

(defclass ftp-shell (generic-shell) ())

(defun make-ftp-shell () (make-instance 'ftp-shell))

(defmethod make-terminal ((shell (eql 'ftp-shell)) (stream null) &key)
  (let* ((stream (use connect-to-ftp-server (snet-hostname *overrides*)
                      :user (snet-username *overrides*)
                      :password (snet-password *overrides*)))
         (it (make-instance 'generic-pseudo-terminal
                            :shell (make-ftp-shell)
                            :stream stream)))
    (ftp-stream-cwd stream (format nil "$~A.r~Aexe"
                                   (snet-volume *overrides*)
                                   (snet-tag *overrides*)))
    it))

(defmethod cmd-put-exec* ((shell ftp-shell) (stream stream)
                          exec-dir exec-name)
  (ftp-stream-put stream
                  (make-pathname :name exec-name :defaults exec-dir)
                  (concatenate 'string exec-name ",700")))
