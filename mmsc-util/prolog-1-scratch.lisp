(in-package :com.github.dkick.mmsc-util.user)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member (find-package :net.uri)
                  (package-use-list :com.github.dkick.mmsc-util.user))
    (use-package :net.uri)))

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member (find-package :puri)
                  (package-use-list :com.github.dkick.mmsc-util.user))
    (use-package :puri)))

(defun remote-execs ()
  (remove-if #'(lambda (x)
                 (or (equal x "dist") (equal x "fwdcgi")))
             *execs*))

(defun set-duff-overrides ()
  (setq *overrides*
        (make-overrides (use make-pathname :device "D"
                             :directory '(:absolute
                                          "Clearcase"
                                          "dkick1_m19.3.0_dev-895321"
                                          "mp"))
                        "SNET2" "DK")))

(defclass exec ()
    ((uri :initarg :uri :accessor exec-uri)))

(defclass test-exec (exec) ())

(defclass mms-exec (exec) ())

(defun make-foo-test-exec ()
  (use make-instance 'test-exec
       :uri (parse-uri "ftp://snet2/appl1/rdkexe/msv")))

(defun make-bar-test-exec ()
  ;; Cheeky way to get a short alias for the function...
  (let ((fn #'com.github.dkick.mmsc-util::exec-directory-pathname))
    (use make-instance 'test-exec
         :uri (use pathname-to-uri
                   (let ((msv "msv"))
                     (make-pathname :name msv
                                    :defaults (funcall fn msv)))))))

(defun duff-vob-exec-directory (exec)
  (let ((fn #'com.github.dkick.mmsc-util::exec-directory-pathname))
    (with-accessors ((uri exec-uri)) exec
      (and (or (null (uri-host uri))
               (equalp (let (#+allegro
                             (hostname (excl.osi:gethostname))
                             #+clisp
                             (hostname (use posix:uname-nodename
                                            (posix:uname))))
                         hostname)
                       (uri-host uri)))
           (let* ((list (uri-parsed-path uri))
                  (name (last1 list))
                  (uri-dir (cons (car list) (cddr (butlast list))))
                  (pathname (use make-pathname :name name
                                 :defaults (funcall fn name))))
             (and (equal name (pathname-name pathname))
                  (equal uri-dir (pathname-directory pathname))))))))

(defun duff-test-exec-directory (exec)
  (with-accessors ((uri exec-uri)) exec
    (and (equalp (snet-hostname *overrides*) (uri-host uri))
         (equalp "/appl1/rdkexe/msv" (uri-path uri)))))

(defun duff-mms-exec-directory (exec)
  (with-accessors ((uri exec-uri)) exec
    (and (equalp (snet-hostname *overrides*) (uri-host uri))
         (equalp "/appl1/mmexec/msv" (uri-path uri)))))
