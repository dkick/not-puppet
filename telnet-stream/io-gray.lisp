(in-package :net.earthlink.dkixk.telnet-stream)

;;; This file uses symbols from both "misc" and "io-send-recv" so
;;; these forms could not simply be put into one or the other of those
;;; files without causing compiler warnings.

(defmethod stream-read-byte ((stream telnet-gray-stream))
  (recv-octet #'identity stream nil :eof))

(defmethod stream-write-byte ((stream telnet-gray-stream)
                              byte)
  (send-byte stream byte))
