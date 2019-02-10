(in-package :net.earthlink.dkixk.duff-msv)

(defclass message () ())

(defclass subscriber-validation-data ()
  ((orig-address :initarg :orig-address :accessor orig-address)
   (rcpt-addresses :initarg :rcpt-addresses :accessor rcpt-addresses)))

(defclass subscriber-validation-request (subscriber-validation-data message)
  ())

(defclass subscriber-validation-response (subscriber-validation-data message)
  ())

(defun make-ldap-search-request (address)
  (list 'ldap-search-request :address address))

(defclass ldap-search-response (message) ())

(defvar *debug-print-read-message* t)

(defun read-message (&optional (stream t) (eof-error-p t))
  (let ((encoded-message
         (with-standard-io-syntax
           (let ((*package* (use find-package
                                 :net.earthlink.dkixk.duff-msv-user)))
             (read stream eof-error-p)))))
    (when encoded-message
      (when *debug-print-read-message*
        (fresh-line)
        (princ "<- ")
        (pprint encoded-message))
      (apply #'make-instance encoded-message))))

(defgeneric write-message (message stream))

(defmethod write-message ((message list) stream)
  (fresh-line stream)
  (princ "-> " stream)
  (pprint message stream)
  (fresh-line stream)
  message)
