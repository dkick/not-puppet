(in-package :net.earthlink.dkixk.telnet-stream)

(defconstant +iac/byte-code+ 255
  "Integer value for telnet IAC (is a command), used to indicate the
beginning of a sequence used for negotiating telnet options.")

(defconstant +se/byte-code+ 240)
(defconstant +sb/byte-code+ 250)
(defconstant +will/byte-code+ 251)
(defconstant +will-not/byte-code+ 252)
(defconstant +do/byte-code+ 253)
(defconstant +do-not/byte-code+ 254)

;; The following are the telnet options I'm finding either the Solaris
;; telnet server or the Tandem telnet server is trying to negotiate
;; with me.
(defconstant +echo/byte-code+ 1)
(defconstant +suppress-go-ahead/byte-code+ 3)
(defconstant +terminal-type/byte-code+ 24)
(defconstant +window-size/byte-code+ 31) ;aka NAWS
(defconstant +xdisploc/byte-code+ 35)
(defconstant +environ/byte-code+ 36)
(defconstant +newenv/byte-code+ 39)

