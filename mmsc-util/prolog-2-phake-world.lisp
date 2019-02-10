(in-package :com.github.dkick.mmsc-util.user)

(defclass phake-world ()
    ((snets :initform (let ((hash (make-hash-table :test #'eq)))
                        (setf (gethash 'fiona hash)
                              (make-phake-snet))
                        (setf (gethash 'snet4 hash)
                              (make-phake-snet (make-phake-snet-paths)))
                        hash)
            :accessor phake-world-snets)))

(defun make-phake-world () (make-instance 'phake-world))

(defclass phake-snet ()
    ((paths :initform (make-hash-table :test #'equal) :initarg :paths
            :accessor phake-snet-paths)))

(defun make-phake-snet (&optional paths)
  (apply #'make-instance 'phake-snet
         (when paths (list :paths paths))))

(defun make-phake-snet-paths ()
  (let ((hash (make-hash-table :test #'equal)))
    (loop for file in *execs*
          for key = (list :absolute "$appl1" "mmexec"
                          file)
          do (setf (gethash key hash) t))
    hash))

(defgeneric phake-snet (world host))

(defmethod phake-snet ((world phake-world) (host symbol))
  (or (gethash host (phake-world-snets world))
      (error "Unknown host ~A." host)))
