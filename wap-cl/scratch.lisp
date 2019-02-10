(in-package :net.earthlink.dkixk.wap-cl-user)

(defparameter *file*
  (merge-pathnames (make-pathname :directory '(:relative "test-files")
                                  :name "023k_retr_jpg" :type "wap")
                   *load-truename*))

(defvar *stream* (open *file* :direction :input))

(defgeneric read-wap-file (file))

(defmethod read-wap-file ((file t))
  (with-open-file (file/stream file :direction :input)
    (read-wap-file file/stream)))

(defparameter *read-wap-file-entity* '..wap-message::message-entity)

(defmethod read-wap-file ((file stream))
  (let ((message-length (file-length file)))
    (read-value *read-wap-file-entity* file :length message-length)))
