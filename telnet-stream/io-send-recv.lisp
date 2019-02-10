(in-package :net.earthlink.dkixk.telnet-stream)

;;; I suppose that all of these should really be specialized on
;;; telnet-using-socket-simple-stream.

(defun send-char (char telnet-stream)
  (with-accessors ((tcp-stream telnet-tcp-stream)) telnet-stream
    (unless (swank::ascii-char-p char)
      (warn "~A is not an ASCII character." char))
    (write-char char tcp-stream)))

(defun send-byte (byte telnet-stream)
  (with-accessors ((tcp-stream telnet-tcp-stream)) telnet-stream
      (when (= byte +iac/byte-code+)
        (write-byte +iac/byte-code+ tcp-stream))
    (write-byte byte tcp-stream)))

(defun recv-char (stream &optional eof-error-p eof-value)
  (recv-octet #'code-char stream eof-error-p eof-value))

(defun recv-byte (stream &optional eof-error-p eof-value)
  (recv-octet #'identity stream eof-error-p eof-value))

(defun recv-octet (fn telnet-stream &optional eof-error-p eof-value)
  (with-accessors ((tcp-stream telnet-tcp-stream)
                   (suppress-go-ahead telnet-suppress-go-ahead)
                   (wait-for-terminal-type-suboption
                    telnet-wait-for-terminal-type-suboption))
      telnet-stream
    (labels
        ((%read-byte () (read-byte tcp-stream eof-error-p eof-value))
         (%recv-will (option-byte-code)
           (cond ((= option-byte-code +suppress-go-ahead/byte-code+)
                  (setq suppress-go-ahead t))
                 (t (warn "Received unknown option ~S."
                          option-byte-code))))
         (%recv-do (option-byte-code)
           (cond ((= option-byte-code +terminal-type/byte-code+)
                  (send-will +terminal-type/byte-code+ telnet-stream)
                  (setq wait-for-terminal-type-suboption t))
                 (t (send-will-not option-byte-code telnet-stream))))
         (%recv-do-not (option-byte-code)
           ;; At this moment, we will gladly not do just about
           ;; anything.  Don't do anything with the option-byte-code.
           option-byte-code)
         (%recv-sb (option-byte-code)
           (cond ((= option-byte-code +terminal-type/byte-code+)
                  (unless wait-for-terminal-type-suboption
                    (warn "Received SB ~S before having sent a ~
                           WILL TERMINAL-TYPE"
                          option-byte-code))
                  (let ((send/byte-code 1))
                    (when (= (%read-byte) send/byte-code)
                      (assert (and (= (%read-byte) +iac/byte-code+)
                                   (= (%read-byte) +se/byte-code+)))
                      (send-sub-terminal-type-is telnet-stream))))
                 (t (warn "Received unknown SB option ~S."
                          option-byte-code)
                    (loop for octet = (%read-byte)
                          ;; Should probably really look for
                          ;; (+iac/byte-cote+ +se/byte-code+)
                          until (or (not octet)
                                    (= octet +se/byte-code+)))))))
      (loop for byte = (%read-byte) while byte
            if (= byte +iac/byte-code+) do
            (let ((command-byte-code (%read-byte)))
              (cond ((= command-byte-code +iac/byte-code+)
                     ;; Not really a command-code but rather an
                     ;; escapded data value 255.
                     (return (funcall fn command-byte-code)))
                    ((= command-byte-code +will/byte-code+)
                     (%recv-will (%read-byte)))
                    ((= command-byte-code +do/byte-code+)
                     (%recv-do (%read-byte)))
                    ((= command-byte-code +do-not/byte-code+)
                     (%recv-do-not (%read-byte)))
                    ((= command-byte-code +sb/byte-code+)
                     (%recv-sb (%read-byte)))
                    (t (let ((unknown (%read-byte)))
                         (warn "Received command-byte-code ~S and ~
                                unknown byte-code ~S."
                               command-byte-code unknown)))))
            else do
            (unless suppress-go-ahead
              (warn "Have received data before WILL SUPPRESS-GO-AHEAD."))
            (return (funcall fn byte))))))

(defun send-will (option-byte-code telnet-stream)
  (send-command +will/byte-code+ option-byte-code telnet-stream))

(defun send-will-not (option-byte-code telnet-stream)
  (send-command +will-not/byte-code+ option-byte-code telnet-stream))

(defun send-do (option-byte-code telnet-stream)
  (send-command +do/byte-code+ option-byte-code telnet-stream))

(defun send-do-not (option-byte-code telnet-stream)
  (send-command +do-not/byte-code+ option-byte-code telnet-stream))

(defun send-command (command-byte-code option-byte-code telnet-stream)
  (with-accessors ((tcp-stream telnet-tcp-stream)) telnet-stream
    (write-byte +iac/byte-code+ tcp-stream)
    (write-byte command-byte-code tcp-stream)
    (write-byte option-byte-code tcp-stream)
    (force-output tcp-stream)))

(defun send-sub-terminal-type-is (telnet-stream)
  (with-accessors ((tcp-stream telnet-tcp-stream)) telnet-stream
    (flet ((%write-byte (byte) (write-byte byte tcp-stream))
           (%write-string (string) (write-string string tcp-stream)))
      (let ((is/byte-code 0))
        (%write-byte +iac/byte-code+)
        (%write-byte +sb/byte-code+)
        (%write-byte +terminal-type/byte-code+)
        (%write-byte is/byte-code)
        (%write-string "NETWORK-VIRTUAL-TERMINAL")
        (%write-byte +iac/byte-code+)
        (%write-byte +se/byte-code+)
        (force-output tcp-stream)))))
