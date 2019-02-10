(in-package :net.earthlink.dkixk.telnet-stream)

(defmacro with-telnet ((stream &rest args) &body forms)
  ` (let ((,stream (telnet ,@args)))
      (unwind-protect
           (progn ,@forms)
        (when ,stream
          (close ,stream)))))

(defclass generic-telnet-stream ()
    ((suppress-go-ahead :initform nil :accessor telnet-suppress-go-ahead)
     (wait-for-terminal-type-suboption
      :initform nil
      :accessor telnet-wait-for-terminal-type-suboption)))

(defclass telnet-tcp-stream-mixin () ())

(defclass telnet-using-socket-simple-stream (generic-telnet-stream
                                             telnet-tcp-stream-mixin)
    ((tcp-stream :initarg :tcp-stream :accessor telnet-tcp-stream)))

(defconstant +default-telnet-port+ 23)

(defmethod initialize-instance :after
    ((object telnet-using-socket-simple-stream)
     &key remote-host (remote-port +default-telnet-port+))
  (with-accessors ((tcp-stream telnet-tcp-stream)) object
    (setq tcp-stream (make-socket* :remote-host remote-host
                                   :remote-port remote-port))
    (send-do +suppress-go-ahead/byte-code+ object)
    (send-do +echo/byte-code+ object)))

(defun make-socket* (&key remote-host remote-port)
  #+allegro
  (make-socket :remote-host remote-host :remote-port remote-port)
  #+clisp
  (make-socket :remote-host remote-host :remote-port remote-port
               :format :bivalent))

(defclass telnet-quasi-stream (telnet-using-socket-simple-stream)
    ())

(defclass telnet-gray-stream (telnet-using-socket-simple-stream
                              fundamental-binary-input-stream
                              fundamental-binary-output-stream)
    ())

(defvar *default-telnet-stream-type* 'telnet-quasi-stream)

(defun telnet (remote-host &key
                           (remote-port +default-telnet-port+)
                           (stream-type *default-telnet-stream-type*))
  (make-instance stream-type
                 :remote-host remote-host
                 :remote-port remote-port))

(defmethod close ((stream telnet-using-socket-simple-stream)
                  &key abort)
  (with-accessors ((tcp-stream telnet-tcp-stream)
                   (suppress-go-ahead telnet-suppress-go-ahead)
                   (wait-for-terminal-type-suboption
                    telnet-wait-for-terminal-type-suboption))
      stream
    (when tcp-stream
      (prog1 (close tcp-stream :abort abort)
        (setq tcp-stream nil)
        (setq suppress-go-ahead nil)
        (setq wait-for-terminal-type-suboption nil)))))
