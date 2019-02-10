;;;; Things To Do
;;;;
;;;; Currently, when one of the functions signals an error, we have
;;;; still read a certain number of bytes.  This information is not
;;;; contained in the condition but it probably should be.

(in-package :net.earthlink.dkixk.wap-message)

(defconstant +uint-var-bits-per-byte+ 7)

(define-binary-type uint-var ()
  (:reader (in)
    (loop for byte = (read-byte in)
          for value = 0 then (ash value +uint-var-bits-per-byte+)
          counting byte into num-bytes
          do (setf (ldb (byte +uint-var-bits-per-byte+ 0) value)
                   (ldb (byte +uint-var-bits-per-byte+ 0) byte))
          while (logbitp +uint-var-bits-per-byte+ byte)
          finally (return (values value num-bytes))))
  (:writer (out value)
    (let ((octets (make-array 5 :element-type (stream-element-type out)
                              :fill-pointer 0 :adjustable t)))
      ;; Is it better to determine the number of bytes with a lookup
      ;; table?
      (loop for low-bit from 0 by +uint-var-bits-per-byte+
            for byte = (ldb (byte +uint-var-bits-per-byte+ low-bit) value)
            until (= byte 0) do
            (vector-push-extend byte octets))
      (loop for i from (1- (length octets)) downto 1 do
            (write-byte (+ #x80 (aref octets i)) out)
            finally (write-byte (aref octets 0) out)))))

;; Not currently used at the moment but might it be used to write a
;; more efficient version of the above writer for uint-var?
(defun size-as-uint-var (value)
  (cond ((< value #x80) 1)
        ((< value #x4000) 2)
        ((< value #x200000) 3)
        ((< value #x10000000) 4)
        ((< value #x800000000) 5)
        (t (error* 'encoding-error
                   "Value ~S requires more than 5 bytes as a variable ~
                    length integer."))))

(define-binary-type generic-fixed-size-buffer (size first-octet)
  (:reader (in)
    (let* ((true-size (if first-octet (1+ size) size))
           (buffer (make-array true-size
                               :element-type (stream-element-type in))))
      (when first-octet (setf (aref buffer 0) first-octet))
      (let* ((file-pos (file-position in))
             (buffer-pos (read-sequence buffer in
                                        :start (if first-octet 1 0))))
        (unless (= buffer-pos size)
          (error* 'encoding-error
                  "Expected to read ~S bytes.  Actually read ~S bytes at ~
                   position ~S of ~S."
                  true-size buffer-pos file-pos in)))
      (values buffer size)))
  (:writer (out buffer)
    (assert (= (length buffer) size))
    (let* ((file-pos (file-position out))
           (buffer-pos (write-sequence buffer out)))
      (unless (= buffer-pos size)
        (error* 'encoding-error
                "Expected to write ~S bytes.  Actually wrote ~S bytes ~
                   at position ~S of ~S."
                size buffer-pos file-pos out)))))

(define-binary-type generic-wsp-value ()
  (:reader (in)
    (let ((first-octet (read-value 'uint8 in)))
      (cond ((< first-octet 31)
             (let ((object (read-value 'generic-fixed-size-buffer in
                                       :size first-octet)))
               (values object
                       ;; 1 for the size of the first-octet and the
                       ;; size of the buffer is the value of the
                       ;; first-octet
                       (+ 1 first-octet))))
            ((= first-octet 31)
             (multiple-value-bind (size-value size-size)
                 (read-value 'uint-var in)
               (multiple-value-bind (object object-size)
                   (read-value 'generic-fixed-size-buffer in
                               :size size-value)
                 (values object (+ 1 size-size object-size)))))
            ((< first-octet 128)
             (multiple-value-bind (result partial-size)
                 (read-value 'iso-8859-1-terminated-string in
                             :first-octet first-octet :terminator +null+)
               ;; Add in one for the size of the first-octet
               (values result (1+ partial-size))))
            ;; Must be less than 256
            (t (values (ldb (byte 7 0) first-octet)
                       ;; 1 being the size of uint8 in number of
                       ;; (unsigned-byte 8)
                       1)))))
  (:writer (out value)
    (declare (ignore out value))
    (error* 'not-coded-error "~S not coded yet."
            (use find-method #'write-value '()
                 (list (aclmop:intern-eql-specializer 'generic-wsp-value)
                       (find-class t) (find-class t))))))

(define-tagged-binary-class wsp-parameter ()
    ((name generic-wsp-value))
  (:dispatch (find-wsp-field-name-class name)))

(define-binary-class generic-wsp-parameter (wsp-parameter)
  ((value generic-wsp-value)))

(defmethod find-wsp-field-name-class ((name t))
  (warn "Using default method ~S"
        (use find-method #'find-wsp-field-name-class '()
             (list (find-class t))))
  'generic-wsp-parameter)

(defmethod find-wsp-field-name-class ((name integer))
  (cond ((< name #x01)
         (error* 'encoding-error "~S not a valid field name." name))
        ((= name #x04)
         ;; Wants to be something specific for content-type.
         'generic-wsp-parameter)
        ((< name #x21) 'generic-wsp-parameter) ;Last of known names
        (t 'generic-wsp-parameter)))

#-(and)
(defmacro define-dispatch-function (name (&rest options) &body forms)
  (declare (ignore options))
  (with-gensyms (octet)
    `(defun ,name (,octet)
      (cond ,@(loop for byte-code upfrom 0
                    for form in forms
                    collect `((= ,octet ,byte-code)
                              ',(car form)))))))
