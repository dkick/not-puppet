(in-package :net.earthlink.dkixk.generic-message-parts)

(defun error* (type control &rest arguments)
  (error type :format-control control :format-arguments arguments))

(define-condition encoding-error (error) ())
(define-condition not-coded-error (program-error) ())

;;;; Types borrowed from Practical Common Lisp's implementation of an
;;;; ID3v2

;;; A few basic types

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
          for low-bit downfrom (* bits-per-byte (1- bytes)) to 0
          by bits-per-byte do
          (setf (ldb (byte bits-per-byte low-bit) value)
                (read-byte in))
          finally (return (values value bytes))))
  (:writer (out value)
    (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0
          by bits-per-byte
          do (write-byte (ldb (byte bits-per-byte low-bit) value)
                         out))))

(define-binary-type uint8  () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type uint16 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type uint24 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type uint32 () (unsigned-integer :bytes 4 :bits-per-byte 8))

;;; Strings

(define-binary-type generic-string (length character-type first-octet)
  (:reader (in)
    (let ((true-length (if first-octet (1+ length) length)))
      (use values
           (with-output-to-string
               (s (make-array true-length :element-type 'character
                              :fill-pointer 0))
             (when first-octet (write-char (code-char first-octet) s))
             (loop repeat length do
                   (write-char (read-value character-type in) s)))
           length)))
  (:writer (out string)
    (dotimes (i length)
      (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string
    (terminator character-type first-octet)
  (:reader (in)
    (let ((num-bytes 0))
      (use values
           (with-output-to-string (s)
             (when first-octet (write-char (code-char first-octet) s))
             (loop for char = (read-value character-type in)
                   do (incf num-bytes)
                   until (char= char terminator) do (write-char char s)))
           num-bytes)))
  (:writer (out string)
    (loop for char across string
          do (write-value character-type out char)
          finally (write-value character-type out terminator))))

;;; ISO-8859-1 strings

(define-binary-type iso-8859-1-char ()
  (:reader (in)
    (let ((code (read-byte in)))
      (use values
           (or (code-char code)
               (error* 'encoding-error "Character code ~d not supported"
                       code))
           1)))
  (:writer (out char)
    (let ((code (char-code char)))
      (if (<= 0 code #xff)
          (write-byte code out)
          (error* 'encoding-error
                  "Illegal character for iso-8859-1 encoding: ~
                   character: ~c with code: ~d"
                  char code)))))

(define-binary-type iso-8859-1-string (length first-octet)
  (generic-string :length length :character-type 'iso-8859-1-char
                  :first-octet first-octet))

(define-binary-type iso-8859-1-terminated-string (terminator first-octet)
  (generic-terminated-string :terminator terminator
                             :character-type 'iso-8859-1-char
                             :first-octet first-octet))

;;; UCS-2 (Unicode) strings (i.e. UTF-16 without surrogate pairs,
;;; phew.)

;;; Define a binary type for reading a UCS-2 character relative to a
;;; particular byte ordering as indicated by the BOM value.  v2.3
;;; specifies that the BOM should be present. v2.2 is silent though it
;;; is arguably inherent in the definition of UCS-2) Length is in
;;; bytes. On the write side, since we don't have any way of knowing
;;; what BOM was used to read the string we just pick one.  This does
;;; mean roundtrip transparency could be broken.

(define-binary-type ucs-2-char (swap)
  (:reader (in)
    (let ((code (read-value 'uint16 in)))
      (when swap (setf code (swap-bytes code)))
      (use values
           (or (code-char code)
               (error* 'encoding-error "Character code ~d not supported"
                       code))
           ;; This size of a uint16 in terms of (unsigned-byte 8)
           2)))
  (:writer (out char)
    (let ((code (char-code char)))
      (unless (<= 0 code #xffff)
        (error* 'encoding-error
                "Illegal character for ucs-2 encoding: ~
                 ~c with char-code: ~d"
                char code))
      (when swap (setf code (swap-bytes code)))
      (write-value 'uint16 out code))))

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
    (let ((byte-order-mark (read-value 'uint16 in))
          (characters (1- (/ length 2))))
      (use values
           (read-value 'generic-string in
                       :length characters
                       :character-type (ucs-2-char-type byte-order-mark))
           ;; 2 being the number of (unsigned-byte 8) for a uint16
           ;; encoding the byte-order-mark
           (+ length 2))))
  (:writer (out string)
    (write-value 'uint16 out #xfeff)
    (write-value 'generic-string out string
                 :length (length string)
                 :character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
    (let ((byte-order-mark (read-value 'uint16 in)))
      (multiple-value-bind (object partial-size)
          (read-value 'generic-terminated-string in
                      :terminator terminator
                      :character-type (ucs-2-char-type byte-order-mark))
          ;; 2 being the number of (unsigned-byte 8) for a uint16
          ;; encoding the byte-order-mark
        (values object (+ partial-size 2)))))
  (:writer (out string)
    (write-value 'uint16 out #xfeff)
    (write-value 'generic-terminated-string out string
                 :terminator terminator
                 :character-type (ucs-2-char-type #xfeff))))

