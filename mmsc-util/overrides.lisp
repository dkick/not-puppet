(in-package :com.github.dkick.mmsc-util)

(defparameter *execs*
  ;; cgi, dist, fwdcgi, and provcgi all need different permissions
  ;; (super.super).
  (prog1 (sort (list "audmgr" "cgi" "delmgr" "dist" "dsm" "fsm" "fwdcgi"
                     "mmbilmgr" "mmcpulog" "mmsmpcln" "mmstimn" "mmtrcexp"
                     "mmtrmn" "mmweb" "msm" "msv" "ppm" "retmgr" "sch"
                     "smtpd" "smtpr" "statmgr" "submgr")
               #'string<)
    (warn "Still need to implement special handling for dist exec.")))

(defparameter *sql-volumes*
  (append (mapcar #'(lambda (host) (cons host "$conf1"))
                  '("snet1" "snet2" "snet4" "snet7"))
          (mapcar #'(lambda (host) (cons host "$data1"))
                  '("snet3" "snet8" "snet9" "fiona"))))

(defvar *exclude-pathnames*
  (list (make-pathname :directory '(:relative "atca3pv"))))

(defvar *overrides*)

(defgeneric read-overrides (overrides &key))
(defgeneric snet-hostname (object))
(defgeneric snet-username (object))
(defgeneric snet-password (object))

(defvar *default-host-info-username* "node.mgr")
(defvar *default-host-info-password* "aa")
(defvar *default-super-super-password* "bb")
(defvar *default-super-webmastr-password* "bb")
(defvar *default-overrides-volume* "APPL1")

(defclass host-info ()
    ((hostname :initarg :hostname :accessor host-info-hostname)
     (username :initform *default-host-info-username* :initarg :username
               :accessor host-info-username)
     (password :initform *default-host-info-password* :initarg :password
               :accessor host-info-password)))

(defun make-host-info (hostname &optional
                                (username *default-host-info-username*)
                                (password *default-host-info-password*))
  (use make-instance 'host-info
       :hostname hostname :username username :password password))

;; Okay, sure there is probably a better way than changing the value
;; of *default-super-webmastr-password* as a side effect is probably
;; not the best way to do this.  <shrug>
(defmethod shared-initialize :after ((object host-info)
                                     slot-names &rest initargs
                                     &key (allow-other-keys nil))
  (declare (ignore slot-names initargs allow-other-keys))
  (warn "Code assumes hostname which is not a FQDN.")
  (when (slot-boundp object 'hostname)
    (with-accessors ((hostname host-info-hostname)) object
      (when (string-equal hostname "fiona")
        (setq *default-super-webmastr-password* "aa")))))

;; Okay, sure there is probably a better way than changing the value
;; of *default-super-webmastr-password* as a side effect is probably
;; not the best way to do this.  <shrug>
(defmethod (setf host-info-hostname) :after (value (object host-info))
  (declare (ignore value))
  (warn "Code assumes hostname which is not a FQDN.")
  (with-accessors ((hostname host-info-hostname)) object
    (when (string-equal hostname "fiona")
      (setq *default-super-webmastr-password* "aa"))))

(defclass overrides ()
    ((vob-tag :initarg :vob-tag :accessor vob-tag)
     (snet-host-info :initarg :snet-host-info :accessor snet-host-info)
     (snet-volume :initform *default-overrides-volume*
                  :initarg :snet-volume :accessor snet-volume)
     (snet-tag :initarg :snet-tag :accessor snet-tag)
     (tsend :accessor tsend-pathname)
     (exec-directories :accessor exec-directories)
     (sql-volume :accessor sql-volume)))

(defmethod shared-initialize :after ((object overrides)
                                     slot-names &rest initargs
                                     &key (allow-other-keys nil))
  (declare (ignore slot-names initargs allow-other-keys))
  (when (slot-boundp object 'vob-tag)
    (with-accessors ((vob-tag vob-tag) (tsend tsend-pathname)
                     (exec-directories exec-directories))
        object
      (multiple-value-setq (tsend exec-directories)
        (after-set-vob-tag vob-tag))))
  (when (slot-boundp object 'snet-host-info)
    (with-accessors ((snet-host-info snet-host-info)
                     (sql-volume sql-volume))
        object
      (setq sql-volume (find-sql-volume snet-host-info)))))

(defmethod (setf vob-tag) :after (value (object overrides))
  (with-accessors ((tsend tsend-pathname)
                   (exec-directories exec-directories))
      object
    (multiple-value-setq (tsend exec-directories)
      (after-set-vob-tag value))))

(defmethod (setf snet-host-info) :after (value (object overrides))
  (with-accessors ((sql-volume sql-volume)) object
    (setq sql-volume (find-sql-volume value))))

(defmethod snet-hostname ((object overrides))
  (host-info-hostname (snet-host-info object)))

(defmethod snet-username ((object overrides))
  (host-info-username (snet-host-info object)))

(defmethod snet-password ((object overrides))
  (host-info-password (snet-host-info object)))

(defmethod (setf snet-tag) :before (value (object overrides))
  (assert (eql (length value) 2)))

(defun make-overrides
    (vob-tag snet-hostname snet-tag &key
             (snet-username *default-host-info-username*)
             (snet-password *default-host-info-password*)
             (snet-volume *default-overrides-volume*))
  (use make-instance 'overrides
       :vob-tag vob-tag
       :snet-host-info (use make-host-info
                            snet-hostname snet-username snet-password)
       :snet-volume snet-volume :snet-tag snet-tag))

(defmethod read-overrides ((overrides string) &rest args)
  (apply #'read-overrides (pathname overrides) args))

(defmethod read-overrides ((overrides pathname) &rest args)
  (with-open-stream (stream (apply #'open overrides args))
    (read-overrides stream)))

(defmethod read-overrides ((overrides stream) &key)
  (let (vob-tag snet-hostname snet-username snet-password
        snet-volume snet-tag)
    (loop for line = (read-line overrides nil) while line do
          (cl-ppcre:register-groups-bind (code)
              ('(:sequence
                 :start-anchor
                 (:register
                  (:greedy-repetition 0 nil
                   (:inverted-char-class #\#))))
                line)
            (cl-ppcre:register-groups-bind (name value)
                ('(:sequence
                   (:register
                    (:greedy-repetition 1 nil :word-char-class))
                   (:greedy-repetition 0 nil :whitespace-char-class)
                   ":="
                   (:greedy-repetition 0 nil :whitespace-char-class)
                   (:register (:greedy-repetition 1 nil :everything)))
                  code)
              (cond ((equal name "VOBTAG")
                     #+allegro
                     (setq vob-tag
                           (use translate-cygwin-pathname
                                (use pathname/cygwin value
                                     :name-as-directory t)))
                     #+clisp
                     (setq vob-tag (pathname value)))
                    ((equal name "SNET_HOSTNAME")
                     (setq snet-hostname value))
                    ((equal name "SNET_USERNAME")
                     (setq snet-username value))
                    ((equal name "SNET_PASSWORD")
                     (setq snet-password value))
                    ((equal name "SNET_VOL")
                     (setq snet-volume value))
                    ((equal name "SNET_TAG")
                     (setq snet-tag value))))))
    (make-overrides vob-tag snet-hostname snet-tag
                    :snet-username snet-username
                    :snet-password snet-password
                    :snet-volume snet-volume)))

(defvar *warn-if-exec-count-mismatch* t)

(defun after-set-vob-tag (vob-tag)
  (multiple-value-bind (tsend exec-pathnames)
      (find-some-execs vob-tag)
    (unless (and *warn-if-exec-count-mismatch*
                 (= (length exec-pathnames) (length *execs*)))
      (warn "Expected ~A executables but actually found ~A executables."
             (length *execs*) (length exec-pathnames)))
    (values tsend
            (mapcar #'(lambda (exec-pathname)
                        (cons (pathname-name exec-pathname)
                              (use make-pathname
                                   :host (pathname-host exec-pathname)
                                   :device (pathname-device exec-pathname)
                                   :directory (use pathname-directory
                                                   exec-pathname))))
                    exec-pathnames))))

(defun find-some-execs (vob-tag)
  "Walks the pathname vob-tag, looking for pathnames for the perl
script tsend and the MMSC executables, \"*execs*\"."
  (when vob-tag
    (let ((tsend nil) (exec-pathnames '()))
      (use walk-directory vob-tag
           #'(lambda (pathname)
               (let ((name (pathname-name pathname)))
                 (cond ((equal name "tsend")
                        (setq tsend pathname))
                       ((and (or (null (pathname-type pathname))
                                 (eq (pathname-type pathname)
                                     :unspecific))
                             (member name *execs* :test #'equal))
                        (push pathname exec-pathnames)))))
           :prune #'(lambda (pathname)
                      (exclude-pathname-p pathname vob-tag)))
      (values tsend
              (sort exec-pathnames #'string< :key #'pathname-name)))))

(defun exclude-pathname-p (pathname vob-tag)
  ;; dkick1: There needs to be a better way to check for the equality
  ;; of the pathnames.
  (member (namestring pathname) *exclude-pathnames*
          :key #'(lambda (exclude-pathname)
                   (use namestring
                        (merge-pathnames exclude-pathname vob-tag)))
          :test #'string=))

(defun find-sql-volume (host-info)
  (or (find-sql-volume/telnet host-info)
      (find-sql-volume/ftp host-info)
      (find-sql-volume* (host-info-hostname host-info))))

(defun find-sql-volume/telnet (host-info)
  (declare (ignore host-info))
  (warn "find-sql-volume/telnet not implemeted yet.")
  nil)

(defun find-sql-volume/ftp (host-info)
  #+allegro
  (find-sql-volume/ftp/allegro host-info)
  #+clisp
  (find-sql-volume/ftp/clisp host-info))

#+allegro
(defun find-sql-volume/ftp/allegro (host-info)
  (let ((local-pathname (pathname (sys:make-temp-file-name))))
    ;; Damien Kick (2006-12-05): Right me: Don't just completely
    ;; swallow all errors but rather do something with restarts.
    (when (ignore-errors
            (ftp-get (host-info-hostname host-info)
                     "$appl1.aaamm.mmtbldef" local-pathname
                     :user (host-info-username host-info)
                     :password (host-info-password host-info)
                     :mode :text))
      (unwind-protect
           (let ((ftp-value (find-sql-volume/pathname local-pathname))
                 (alt-value (use find-sql-volume*
                                 (host-info-hostname host-info))))
             (unless (equalp ftp-value alt-value)
               (warn "For host ~A: find-sql-volume/ftp returns ~S and ~
                      find-sql-volume* returns ~S."
                     (host-info-hostname host-info)
                     ftp-value alt-value))
             (values ftp-value local-pathname))
        (when (file-exists-p local-pathname)
          (delete-file local-pathname))))))

#+clisp
(defun find-sql-volume/ftp/clisp (host-info)
  (declare (ignore host-info))
  (warn "find-sql-volume/ftp/clisp not implemented yet.")
  nil)

(defun find-sql-volume/pathname (pathname)
  (with-open-file (in pathname :direction :input)
    (loop for line = (read-line in nil) while line do
          (cl-ppcre:register-groups-bind (code)
              ('(:sequence
                 :start-anchor
                 (:register
                  (:greedy-repetition 0 nil (:inverted-char-class #\=)))
                 (:negative-lookahead "=="))
                line)
            ;; For now, we're only looking for one parameter, "svol".
            (cl-ppcre:register-groups-bind (value)
                ('(:sequence
                   (:greedy-repetition 0 nil :whitespace-char-class)
                   "#SET"               ;Make this case insensitive
                   (:greedy-repetition 0 nil :whitespace-char-class)
                   "svol"               ;Should be case insensitive?
                   (:greedy-repetition 0 nil :whitespace-char-class)
                   (:register
                    (:greedy-repetition 1 nil :everything)))
                  code)
              (return-from find-sql-volume/pathname value))))
    nil))

(defun find-sql-volume* (hostname)
  (or (cdr (assoc hostname *sql-volumes* :test #'equalp))
      (error "Could not find SQL volume for ~A" hostname)))

(defun exec-directory-pathname (exec)
  (or (cdr (assoc exec (exec-directories *overrides*) :test #'equal))
      (error "Could not find directory for ~A" exec)))
