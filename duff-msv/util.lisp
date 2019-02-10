(in-package :pg-user)

(defun delete-duff-msv-packages ()
  (delete-package :net.earthlink.dkixk.duff-msv-user)
  (delete-package :net.earthlink.dkixk.duff-msv))
