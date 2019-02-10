(in-package :net.earthlink.dkixk.wap-message)

(define-tagged-binary-class message-entity (&key length)
    ((headers message-headers))
  (:dispatch (find-body-class headers) :self-size :headers-size))

(define-tagged-binary-class content-entity (&key header-length data-length)
    ((headers (content-headers :length header-length)))
  (:dispatch (find-body-class headers)))

(defgeneric find-body-class (header-info))

(defgeneric multipart-body-p (header-info))

(define-binary-class simple-message-entity (message-entity &key headers-size)
  ((body (generic-fixed-size-buffer :size (- length headers-size)))))

(define-binary-class multipart-message-entity
    (message-entity &key headers-size)
  ((body (multipart-body :length (- length headers-size)))))

(define-binary-class simple-content-entity (content-entity)
    ((body (generic-fixed-size-buffer :size data-length))))

(define-binary-class multipart-content-entity (content-entity)
    ((body multipart-body)))

(define-binary-class multipart-body (&key length)
    ((num-entries uint-var)
     (parts (multipart-body-parts :length (- length self-size))))
  (:self-size self-size))

(define-binary-type multipart-body-parts (length)
  (:reader (in)
    (let ((self-size 0))
      (loop for header-length = (multiple-value-bind
                                      (header-length-value
                                       header-length-size)
                                    (read-value 'uint-var in)
                                  (incf self-size header-length-size)
                                  header-length-value)
            for data-length = (multiple-value-bind
                                    (data-length-value
                                     data-length-size)
                                  (read-value 'uint-var in)
                                (incf self-size data-length-size)
                                data-length-value)
            for part = (multiple-value-bind (part-value part-size)
                           (read-value 'content-entity in
                                       :header-length header-length
                                       :data-length data-length)
                         (incf self-size part-size)
                         part-value)
            collect part into body-parts while (< self-size length)
            finally (return (values body-parts self-size)))))
  (:writer (out value)
    (declare (ignore out value))
    (error* 'not-coded-error "~S not coded yet."
            (use find-method #'write-value '()
                 (list (use aclmop:intern-eql-specializer
                            'multipart-body-parts)
                       (find-class t) (find-class t))))))

(defclass entity-headers ()
    ((content-type :initform nil :initarg :content-type
                   :accessor content-type)
     (date :initform nil :initarg :date :accessor date)
     (from :initform nil :initarg :from :accessor from)
     (message-class :initform nil :initarg :message-class
                    :accessor message-class)
     (message-id :initform nil :initarg :message-id
                 :accessor message-id)
     (message-type :initform nil :initarg :message-type
                   :accessor message-type)
     (mms-version :initform nil :initarg :mms-version
                  :accessor mms-version)
     (priority :initform nil :initarg :priority
               :accessor priority)
     (subject :initform nil :initarg :subject
              :accessor subject)
     (to :initform nil :initarg :to :accessor to)
     (transaction-id :initform nil :initarg :transaction-id
                     :accessor transaction-id)
     (unknowns :initform '() :initarg :unknowns
              :accessor unknowns)))

(defclass message-headers (entity-headers) ())

(defclass content-headers (entity-headers) ())

(defmethod find-body-class ((headers message-headers))
  (if (multipart-body-p (value (content-type headers)))
      'multipart-message-entity
      'simple-message-entity))

(defmethod find-body-class ((headers content-headers))
  (if (multipart-body-p (content-type headers))
      'multipart-content-entity
      'simple-content-entity))

(defmethod multipart-body-p ((header-info integer))
  (find header-info '(35 36 38 51)))

;; If we don't have a specialization for the type, treat it like a
;; single simple entity.
(defmethod multipart-body-p ((header-info t))
  nil)

(defmethod read-object + ((part message-headers) stream &key)
  (let ((self-size 0))
    (unwind-protect
         (loop for parameter = (multiple-value-bind
                                     (parameter-value parameter-size)
                                   (read-value 'wsp-parameter stream)
                                 (incf self-size parameter-size)
                                 parameter-value)
               for name = (name parameter)
               do (set-field name parameter part)
               until (= name +content-type-pname/integer+))
      (setf (unknowns part) (nreverse (unknowns part))))
    self-size))

(defmethod read-value ((type (eql 'content-headers)) stream &key length)
  (let ((object (make-instance type)))
    (read-object object stream :length length)))

(defmethod read-object + ((part content-headers) stream &key length)
  (let ((self-size 0))
    (multiple-value-bind (content-type-value content-type-size)
        (read-value 'generic-wsp-value stream)
      (incf self-size content-type-size)
      (setf (content-type part) content-type-value))
    (unless (>= self-size length)
      (unwind-protect
           (loop for parameter = (multiple-value-bind
                                       (parameter-value parameter-size)
                                     (read-value 'wsp-parameter stream)
                                   (incf self-size parameter-size)
                                   parameter-value)
                 for name = (name parameter)
                 do (set-field name parameter part)
                 until (>= self-size length))
        (setf (unknowns part) (nreverse (unknowns part)))))
    self-size))

(defgeneric find-accessor-for-name (name part))

(defun set-field (name parameter part)
  (funcall (find-accessor-for-name name part) parameter part))

(defmethod find-accessor-for-name ((name integer) (part entity-headers))
  (case name
    (#x04 #'(setf content-type))
    (#x05 #'(setf date))
    (#x09 #'(setf from))
    (#x0a #'(setf message-class))
    (#x0b #'(setf message-id))
    (#x0c #'(setf message-type))
    (#x0d #'(setf mms-version))
    (#x0f #'(setf priority))
    (#x16 #'(setf subject))
    (#x17 #'(setf to))
    (#x18 #'(setf transaction-id))
    (t #'(lambda (p m)
           (push p (unknowns m))
           p))))
