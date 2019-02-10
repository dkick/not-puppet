(in-package :net.earthlink.dkixk.telnet-stream)

;; Gray streams can not be bivalent like simple streams.  We'll have
;; this be a binary stream because I'm anticipating using
;; flexi-streams with this at some point.  Cute.  So, we'll have a
;; flexi-stream using a Gray stream using a simple-stream.
(defclass telnet-gray-stream (fundamental-binary-output-stream
                              fundamental-binary-input-stream)
  (tcp-stream))
