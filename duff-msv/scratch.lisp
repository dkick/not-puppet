(in-package :net.earthlink.dkixk.duff-msv)

(defclass subscriber-validator-state ()
  ((the-request :initarg :the-request :reader the-request)))

(defclass find-orig-profile-local (subscriber-validator-state) ())

(defun make-find-orig-profile-local (&rest args)
  (apply #'make-instance 'find-orig-profile-local args))

(defclass subscriber-validator (state-machine)
  ;; Authentication should really come first
  ((state :initform (make-find-orig-profile-local))))

(defun make-subscriber-validator (&rest args)
  (apply #'make-instance 'subscriber-validator args))

(defmethod transition ((state find-orig-profile-local)
                       (event subscriber-validation-request))
  ;; Might this be a good place for condlet?
  (with-accessors ((orig-address orig-address)) event
    (let ((local (find-local-subscriber-profile orig-address)))
      (cond (local (make-phake-state event))
            (t (make-find-orig-profile-remote orig-address event))))))

(defclass phake-state (subscriber-validator-state) ())

(defun make-phake-state (the-request &rest args)
  (apply #'make-instance 'phake-state :the-request the-request args))

(defclass find-orig-profile-remote (subscriber-validator-state)
  ((address :initarg :address :reader address)))

(defun make-find-orig-profile-remote (address the-request &rest args)
  (apply #'make-instance 'find-orig-profile-remote
         :address address :the-request the-request
         args))

(defmethod initialize-instance :after ((object find-orig-profile-remote)
                                       &key)
  (with-accessors ((address address)) object
    (write-message (make-ldap-search-request address)
                   (find-ldap-server-stream address))))

(defmethod transition ((state find-orig-profile-remote)
                       (event ldap-search-response))
  ;; For now, just pretend that we always recieve a successful
  ;; response.
  (make-validate-rcpt-addresses (the-request state)))

(defclass validate-rcpt-addresses (subscriber-validator-state)
  ((rcpt-validators :initarg :rcpt-validators :reader rcpt-validators)))

(defun make-validate-rcpt-addresses (the-request &rest args)
  ;; I should really have the setting of the rcpt-validators slot be
  ;; done at the same time that a value is set for the-request so that
  ;; using make-instance would always do the right thing, too.
  (apply #'make-instance 'validate-rcp-addresses :the-request the-request
         :rcpt-validators (mapcar #'(lambda (address)
                                      (use make-rcpt-validator address
                                           :the-request the-request))
                                  (rcpt-addresses the-request))
         args))

(defclass rcpt-validator (state-machine)
  ;; This is going to need some way to call back into the
  ;; validate-rcpt-addresses state which created it.
  ((state :initform (make-find-rcpt-profile-local))
   (rcpt-address :initarg :rcpt-address :reader rcpt-address)))

(defun make-rcpt-validator (address &rest args)
  (apply #'make-instance 'rcpt-validator :rcpt-address address
         args))

(defclass find-rcpt-profile-local (subscriber-validator-state) ())

(defun make-find-rcpt-profile-local (&rest args)
  (apply #'make-instance 'find-rcpt-profile-local args))

;;; Mostly just silly little stubs for things like DB proxies and
;;; such.

(defun find-local-subscriber-profile (address)
  (declare (ignore address))
  ;; Just for now.  Simulates not finding the local subscriber.
  nil)

(defun find-ldap-server-stream (address)
  (declare (ignore address))
  *standard-output*)
