(in-package :com.github.dkick.mmsc-util)

;;; This might be a good one to move into uncommon-lisp at some point?
;;; I'm not sure but am I perhaps reinventing logical pathnames (and
;;; poorly, too) with this mechanism?  I probably also want to start
;;; using the command cygpath to implement this idea, as it already
;;; deals with Cygwin mounts, etc.

#+allegro
(defvar *cygpath-exec-namestring* "D:\\cygwin\\bin\\cygpath")

#+clisp
(defvar *cygpath-exec-namestring* "/usr/bin/cygpath")

(defgeneric pathname/cygwin (name &key))
(defgeneric namestring/cygwin (name))
(defgeneric find-mount-point (pathname))
(defgeneric valid-mount-pathname-p (pathname))

;; I can't decide whether or not it would be a good idea to have
;; pathname/cygwin inherit from pathname.  With Allegro CL, for one
;; thing, this would require that it be a defstruct, instead of a
;; defclass.  It seems odd to me, however, to have pathname/cygwin
;; inherit from pathname, and then have #P"" show what should be a
;; Cygwin pathname as if it was a pathname.
(defclass pathname/cygwin ()
    ((directory :initarg :directory :accessor pathname-directory/cygwin)
     (name :initarg :name :accessor pathname-name/cygwin)
     (type :initarg :type :accessor pathname-type/cygwin)))

(defun make-pathname/cygwin (&key directory name type)
  (make-instance 'pathname/cygwin
                 :directory directory :name name :type type))

(defmethod pathname/cygwin ((name string)
                            &key ignore-type name-as-directory)
  (let* ((name-as-directory-p (or name-as-directory
                                  (eql (char name (1- (length name)))
                                       #\/)))
         (split (cl-ppcre:split "/" name))
         (parts (if (equal (car split) "")
                    (cons :absolute (cdr split))
                    (cons :relative split))))
    (if name-as-directory-p
        (make-pathname/cygwin :directory parts)
        (let* ((name (last1 parts))
               (pos (unless ignore-type
                      (position #\. name :from-end t))))
          (if pos
              (make-pathname/cygwin :directory (butlast parts)
                                    :name (subseq name 0 pos)
                                    :type (subseq name (1+ pos)))
              (make-pathname/cygwin :directory (butlast parts)
                                    :name name))))))

(defmethod pathname/cygwin ((name pathname) &key)
  (pathname/cygwin (cygpath-unix (namestring name))))

(defmethod namestring/cygwin ((name pathname))
  (namestring/cygwin (pathname/cygwin name)))

(defmethod namestring/cygwin ((name pathname/cygwin))
  (use concatenate 'string
       (when (eq (car (pathname-directory/cygwin name)) :absolute)
         "/")
       (join (if (eq (car (pathname-directory/cygwin name)) :absolute)
                 (cdr (pathname-directory/cygwin name))
                 (pathname-directory/cygwin name))
             "/")
       (when (pathname-name/cygwin name)
         (use concatenate 'string
              "/"
              (pathname-name/cygwin name)
              (when (pathname-type/cygwin name)
                "."
                (pathname-type/cygwin name))))))

(defun translate-cygwin-pathname (pathname/cygwin)
  (pathname (cygpath-windows (namestring/cygwin pathname/cygwin))))

(defun cygpath-windows (file)
  (cygpath "-w" file))

(defun cygpath-unix (file)
  (cygpath "-u" file))

(defun cygpath (option file)
  (let* ((cmd (use concatenate 'string
                   *cygpath-exec-namestring* (string #\Space)
                   option (string #\Space) file))
         #+allegro
         (lines (excl.osi:command-output cmd))
         #+clisp
         (lines (loop with stream = (use ext:run-shell-command
                                         cmd :output :stream)
                      for line = (read-line stream nil) while line
                      collect line)))
    (when (or (null lines) (cdr lines))
      (warn "Expecting a single line returned but found ~S." lines))
    (car lines)))
