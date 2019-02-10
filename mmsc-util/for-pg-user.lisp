(in-package :pg-user)

(defun clear-packages ()
  (delete-package :com.github.dkick.mmsc-util.user)
  (delete-package :com.github.dkick.mmsc-util)
  (delete-package :net.earthlink.dkixk.telnet-stream.user)
  (delete-package :net.earthlink.dkixk.telnet-stream))
