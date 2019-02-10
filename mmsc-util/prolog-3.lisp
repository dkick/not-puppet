(in-package :com.github.dkick.mmsc-util.user)

;;; Phake lisp code to be replaced with terminal commands at a later
;;; date.

(defparameter *phake-world* (make-phake-world))

(defgeneric phake-file-exists (host path))

(defmethod phake-file-exists ((host symbol) (path list))
  (gethash path (phake-snet-paths (phake-snet *phake-world* host))))

(defmethod phake-ftp-to-snet-path ((host symbol) (path list))
  (let ((paths (phake-snet-paths (phake-snet *phake-world* host))))
    (setf (gethash path paths) t)))

;;; Prolog terms used to decide which actions to take.

(define-prolog-struct exec ()
  ((?uri :selector exec-uri) (?info :selector exec-info)))

(define-prolog-struct uri ()
  ((?scheme :selector uri-scheme) (?host :selector uri-host)
   (?port :selector uri-port) (?path :selector uri-path)
   (?query :selector uri-query) (?fragment :selector uri-fragment)
   (?plist :selector uri-plist)))

(<-- (exec-uri-host (exec ?uri ?) ?host)
     (uri-host ?uri ?host))
(<-- (unify-uri-host-path (uri ?scheme ? ?port ? ?query ?fragment ?plist)
                          ?host ?path ?uri/1)
     (= ?uri/1 (uri ?scheme ?host ?port ?path ?query ?fragment ?plist)))

;;; Commands

(<-- (command ?exec-is ?exec-will-be)
     (= ?exec-is ?exec-will-be))

;; FTP
(<- (command ?exec-is ?exec-will-be)
    (= ?exec-is (exec ?uri-is ?))
    (diff-snet-host ?exec-is ?exec-will-be ?host-will-be)
    (ftp-to-snet-path ?exec-will-be ?path-is/1)
    (unify-uri-host-path ?uri-is ?host-will-be ?path-is/1 ?uri-is/1)
    !
    (lisp (progn
            (format t "~&; (ftp ~S ~S)~%" ?uri-is ?uri-is/1)
            (force-output)))
    (= ?exec-is/1 (exec ?uri-is/1 (info (enscribe nil))))
    (command ?exec-is/1 ?exec-will-be))

;;; Helper functions

(<-- (ftp-to-snet-path ?exec-will-be ?path)
     (exec-uri ?exec-will-be ?uri-will-be)
     (uri-host ?uri-will-be ?host-will-be)
     (uri-path ?uri-will-be ?path-will-be)
     (new-file ?host-will-be ?path-will-be)
     (= ?path ?path-will-be)
     (lisp (phake-ftp-to-snet-path ?host-will-be ?path)))

(<- (ftp-to-snet-path ?exec-will-be ?path)
    (exec-uri ?exec-will-be ?uri-will-be)
    (uri-host ?uri-will-be ?host-will-be)
    (uri-path ?uri-will-be ?path-will-be)
    (file-exists ?host-will-be ?path-will-be)
    (alt-snet-path-for-ftp ?host-will-be ?path-will-be ?path)
    (lisp (phake-ftp-to-snet-path ?host-will-be ?path)))

(<-- (alt-snet-path-for-ftp ?host-will-be ?path-exists ?new-path)
     (lisp ?volume (use string-downcase
                        (use concatenate 'string
                             (string #\$) (snet-volume *overrides*))))
     (= ?path-exists (:absolute ?volume "mmexec" ?file-name))
     (lisp ?new-subvolume (use string-downcase
                               (use concatenate 'string
                                    (string #\r) (snet-tag *overrides*)
                                    "exe")))
     (= ?new-path (:absolute ?volume ?new-subvolume ?file-name)))

(<-- (well-known-server-net ?host)
     (member ?host (snet1 snet3 snet4 snet6 snet7 snet8 snet9
                    juliet shrek fiona)))

(<-- (diff-snet-host ?diff-exec ?snet-exec ?snet-host)
     (exec-uri-host ?diff-exec ?some-host)
     (exec-uri-host ?snet-exec ?snet-host)
     (lispp (not (eq ?some-host ?snet-host)))
     (well-known-server-net ?snet-host))

;;; Phake lisp code to be replaced with terminal commands at a later
;;; date.

(<-- (new-file ?exec)
     (exec-uri ?exec ?uri)
     (uri-host ?uri ?host)
     (uri-path ?uri ?path)
     (new-file ?host ?path))

(<- (new-file ?uri)
    (uri-host ?uri ?host)
    (uri-path ?uri ?path)
    (new-file ?host ?path))

(<-- (new-file ?host ?path)
     (lispp (not (phake-file-exists ?host ?path))))

(<-- (file-exists ?exec)
     (exec-uri ?exec ?uri)
     (uri-host ?uri ?host)
     (uri-path ?uri ?path)
     (file-exists ?host ?path))

(<- (file-exists ?uri)
    (uri-host ?uri ?host)
    (uri-path ?uri ?path)
    (file-exists ?host ?path))

(<-- (file-exists ?host ?path)
     (lispp (phake-file-exists ?host ?path)))

;;; Functors to help with testing

(<-- (example-exec-is ?exec-is)
     (lisp ?uri-is
           (use pathname-to-uri
                (use make-pathname :device "D"
                     :directory '(:absolute "Clearcase"
                                  "dkick1_m19.3.0_dev-910878" "mp" "mmsc"
                                  "processes" "msv")
                     :name "msv")))
     (lisp ?scheme-is (uri-scheme ?uri-is))
     (lisp ?host-is (uri-host ?uri-is))
     (lisp ?port-is (uri-port ?uri-is))
     (lisp ?path-is (uri-parsed-path ?uri-is))
     (lisp ?query-is (uri-query ?uri-is))
     (lisp ?fragment-is (uri-fragment ?uri-is))
     (lisp ?plist-is (uri-plist ?uri-is))
     (= ?exec-is (exec (uri ?scheme-is ?host-is ?port-is ?path-is
                            ?query-is ?fragment-is ?plist-is)
                       nil)))

(<-- (example-exec ?host ?path ?info ?exec)
     (well-known-server-net ?host)
     (= ?exec (exec (uri :file ?host nil ?path nil nil nil) ?info)))

