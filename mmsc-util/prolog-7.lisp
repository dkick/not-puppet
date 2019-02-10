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

(define-prolog-struct exec ()
  ((?uri :selector exec-uri) (?info :selector exec-info)))

;; Only those parts of a URI that I use.
(define-prolog-struct uri ()
  ((?host :selector uri-host) (?path :selector uri-path)))

(define-prolog-struct existent-file ()
  ((?uri :selector existent-file-uri) (?info :selector existent-file-info)))

(defun existent-file-uri (existent-file)
  (unless (eq (first existent-file) 'existent-file)
    (error "~S is not a valid existent-file." existent-file))
  (second existent-file))

(<-- (exec-uri-host (exec ?uri ?) ?host)
     (uri-host ?uri ?host))
(<-- (exec-uri-path (exec ?uri ?) ?path)
     (uri-path ?uri ?path))
(<-- (unify-exec-uri-path (exec (uri ?host ?) ?info) ?path ?exec/1)
     (= ?exec/1 (exec (uri ?host ?path) ?info)))
(<-- (unify-uri-host-path ? ?host ?path ?uri/1)
     (= ?uri/1 (uri ?host ?path)))

;;; The command predicate

(<-- (command ?exec-is ?exec-will-be ?cmds/1)
     (exec-uri ?exec-is ?uri-is) (exec-info ?exec-is ?info-is)
     (command ?exec-is ?exec-will-be ?exec-is
              ((existent-file ?uri-is t)) ?world () ?cmds)
     ! (lisp ?cmds/1 (reverse ?cmds)))

(<-- (command ?exec-was ?exec-is ?exec-is ?world ?world ?cmds ?cmds)
     !)

;; The FTP command
(<- (command ?exec-was ?exec-will-be ?exec-is ?world ?world/3
             ?cmds ?cmds/2)
    (diff-snet-host ?exec-is ?exec-will-be ?host-will-be)
    (ftp-to-snet-path ?exec-will-be ?path-is/1 ?world ?world/1)
    (exec-uri ?exec-is ?uri-is)
    (unify-uri-host-path ?uri-is ?host-will-be ?path-is/1 ?uri-is/1)
    (= ?exec-is/1 (exec ?uri-is/1 (info (enscribe nil))))
    (= ?cmds/1 ((ftp ?exec-is ?exec-is/1) . ?cmds))
    (alive-in-the-world ?exec-is/1 ?world/1 ?world/2)
    (command ?exec-was ?exec-will-be ?exec-is/1 ?world/2 ?world/3
             ?cmds/1 ?cmds/2))

;; The SQL compile command
(<- (command ?exec-was ?exec-will-be ?exec-is ?world ?world/1
             ?cmds ?cmds/2)
    (same-snet-hosts ?exec-will-be ?exec-is)
    (same-snet-volumes ?exec-will-be ?exec-is)
    (= ?exec-is (exec ?uri-is (info (enscribe nil))))
    (= ?exec-is/1 (exec ?uri-is (info (enscribe valid-sql-program))))
    (= ?cmds/1 ((sql-compile ?exec-is ?exec-is/1) . ?cmds))
    (command ?exec-was ?exec-will-be ?exec-is/1 ?world ?world/1
             ?cmds/1 ?cmds/2))

;; The move command, moving to a file which does not exist.
(<- (command ?exec-was ?exec-will-be ?exec-is ?world ?world/4
             ?cmds ?cmds/2)
    (same-snet-hosts ?exec-will-be ?exec-is)
    (from-old-to-new-snet-path ?exec-is ?exec-will-be ?path-will-be
                               ?world ?world/1)
    (exec-uri-path ?exec-is ?path-is)
    (= ?path-is (:absolute ?volume ?subvolume ?file-is))
    (lispp (not (and (string-equal ?volume
                                   (use concatenate 'string
                                        "$" (snet-volume *overrides*)))
                     (string-equal ?subvolume "mmexec")
                     (member ?file-is *execs* :test #'equal))))
    (exec-info-for-snet-path ?exec-will-be ?exec-is)
    (unify-exec-uri-path ?exec-is ?path-will-be ?exec-is/1)
    (= ?cmds/1 ((move ?exec-is ?exec-is/1) . ?cmds))
    (dead-in-the-world ?exec-is ?world/1 ?world/2)
    (alive-in-the-world ?exec-will-be ?world/2 ?world/3)
    (command ?exec-was ?exec-will-be ?exec-is/1 ?world/3 ?world/4
             ?cmds/1 ?cmds/2))

;; The move command, moving some file to an MMSC executable file which
;; does exist and so we need to move the MMSC executable file to a
;; backup file first.
(<- (command ?exec-was ?exec-will-be ?exec-is ?world ?world/3
             ?cmds ?cmds/2)
    (same-snet-hosts ?exec-will-be ?exec-is)
    (exec-uri ?exec-will-be ?uri-will-be)
    (file-exists ?uri-will-be ?world ?world/1)
    (uri-path ?uri-will-be ?path-will-be)
    (= ?path-will-be (:absolute ?volume ?subvolume ?file))
    ;; If the following test is omitted, one of the examples will
    ;; result in an infinite loop.  It doesn't seem like the correct
    ;; fix for the problem but it also does make some sense to limit
    ;; making backups to those cases for which we expect there to be
    ;; an existing file, i.e. we're replacing an MMSC executable.
    (lispp (member ?file *execs* :test #'equal))
    (lisp ?file/1
          (use com.github.dkick.mmsc-util::exec-backup-name
               (use com.github.dkick.mmsc-util::make-tacl-shell
                    ?volume ?subvolume)
               ?file))
    (= ?path-backup (:absolute ?volume ?subvolume ?file/1))
    (unify-exec-uri-path ?exec-will-be ?path-backup ?exec-backup)
    (warn "Need to use something other than command here...")
    (command ?exec-will-be ?exec-backup ?exec-will-be ?world/1 ?world/2
             ?cmds ?cmds/1)
    (command ?exec-was ?exec-will-be ?exec-is ?world/2 ?world/3
             ?cmds/1 ?cmds/2))

;; The move command, moving an existing MMSC executable file to
;; another file.  Before we can allow the MMSC executable file to be
;; moved, we need to make sure that there is a backup of the MMSC
;; executable file available to be used as a replacement.  There must
;; always be some MMSC executable file available.
#-(and)
(<- (command ?exec-was ?exec-will-be ?exec-is ?world ?world/n
             ?cmds ?cmds/n)
    (same-snet-hosts ?exec-will-be ?exec-is))

;;; FTP command helper predicates

(<-- (ftp-to-snet-path ?exec-will-be ?path ?old-world ?new-world)
     (exec-uri ?exec-will-be ?uri-will-be)
     (uri-host ?uri-will-be ?host-will-be)
     (uri-path ?uri-will-be ?path)
     (new-file (uri ?host-will-be ?path) ?old-world ?new-world))

(<- (ftp-to-snet-path ?exec-will-be ?path ?old-world ?new-world)
    (exec-uri ?exec-will-be ?uri-will-be)
    (file-exists ?uri-will-be ?old-world ?new-world/1)
    (alt-snet-path-for-ftp ?uri-will-be ?path ?new-world/1 ?new-world))

(<-- (alt-snet-path-for-ftp (uri ?host-will-be ?path) ?alt-path
                            ?old-world ?new-world)
     (lisp ?volume (use string-downcase
                        (use concatenate 'string
                             (string #\$) (snet-volume *overrides*))))
     (= ?path (:absolute ?volume "mmexec" ?file))
     (lisp ?alt-subvolume (use string-downcase
                               (use concatenate 'string
                                    (string #\r) (snet-tag *overrides*)
                                    "exe")))
     (= ?alt-path (:absolute ?volume ?alt-subvolume ?file))
     (new-file (uri ?host-will-be ?alt-path) ?old-world ?new-world))

;;; Move command helper predicates

(<-- (exec-info-for-snet-path ?exec-will-be ?exec-is)
     (exec-uri ?exec-will-be ?uri-will-be)
     (uri-path ?uri-will-be ?path-will-be)
     (= ?path-will-be (:absolute "$appl1" "mmexec" ?file))
     (lispp (member ?file *execs* :test #'string=))
     (exec-info ?exec-is ?info-is)
     (= ?info-is (info (enscribe valid-sql-program))))

(<- (exec-info-for-snet-path ?exec-will-be ?exec-is))

;;; General helper predicates

(<-- (new-file ?uri ?old-world ?new-world)
     (in-the-world ?uri ?old-world ?new-world)
     (member (existent-file ?uri nil) ?new-world))

(<-- (file-exists ?uri ?old-world ?new-world)
     (in-the-world ?uri ?old-world ?new-world)
     (member (existent-file ?uri t) ?new-world))

(<-- (same-snet-hosts ?exec/1 ?exec/2)
     (exec-uri-host ?exec/1 ?host)
     (exec-uri-host ?exec/2 ?host)
     (well-known-server-net ?host))

(<-- (diff-snet-host ?diff-exec ?snet-exec ?snet-host)
     (exec-uri-host ?diff-exec ?some-host)
     (exec-uri-host ?snet-exec ?snet-host)
     (lispp (not (eq ?some-host ?snet-host)))
     (well-known-server-net ?snet-host))

(<-- (same-snet-volumes ?exec/1 ?exec/2)
     (exec-uri-path ?exec/1 ?path/1)
     (exec-uri-path ?exec/2 ?path/2)
     (= ?path/1 (:absolute ?volume ? ?))
     (= ?path/2 (:absolute ?volume ? ?)))

(<-- (from-old-to-new-snet-path ?old-exec ?new-exec ?new-path
                                ?world ?world/2)
     (exec-uri-path ?old-exec ?old-path) (exec-uri-path ?new-exec ?new-path)
     (lispp (not (equal ?old-path ?new-path)))
     (exec-uri ?old-exec ?old-uri) (exec-uri ?new-exec ?new-uri)
     (file-exists ?old-uri ?world ?world/1)
     (new-file ?new-uri ?world/1 ?world/2))

(<-- (well-known-server-net ?host)
     (member ?host (snet1 snet3 snet4 snet6 snet7 snet8 snet9
                    juliet shrek fiona)))

;;; '?world's cache information gathered from phake-world functions as
;;; well as record the side effects on "the world" of unifying
;;; commands.

(<-- (in-the-world ?uri ?world ?world)
     (member (existent-file ?uri ?) ?world))

(<- (in-the-world ?uri ?old-world ?new-world)
    (not (member (existent-file ?uri ?) ?old-world))
    (uri-host ?uri ?host) (uri-path ?uri ?path)
    ;; This is meant to simulate having a telnet session check if the
    ;; file exists.
    (lisp ?exists (phake-file-exists ?host ?path))
    (lisp ?new-world (pushnew (list 'existent-file ?uri ?exists) ?old-world
                              :key #'existent-file-uri :test #'equal)))

(<-- (dead-in-the-world ?exec ?old-world ?new-world)
     (exec-uri ?exec ?uri)
     (remove-uri-from-the-world ?uri ?old-world ?world/1)
     (add-to-the-world (existent-file ?uri nil) ?world/1 ?new-world))

(<-- (alive-in-the-world ?exec ?old-world ?new-world)
     (exec-uri ?exec ?uri)
     (remove-uri-from-the-world ?uri ?old-world ?world/1)
     (add-to-the-world (existent-file ?uri t) ?world/1 ?new-world))

(<-- (remove-uri-from-the-world ?uri ?old-world ?new-world)
     ;; The URI is the second thing in the list
     (lisp ?new-world (remove ?uri ?old-world
                              :key #'existent-file-uri :test #'equal)))

(<-- (add-to-the-world ?existent-file ?old-world ?new-world)
     (lisp ?new-world (pushnew ?existent-file ?old-world
                               :key #'existent-file-uri :test #'equal)))

;;; Functors to help with testing

(<-- (example ?exec-is ?exec-will-be ?cmds)
     (member ?file ("msv" "test"))
     (example-exec-is ?file ?exec-is)
     (example-exec-will-be ?file ?exec-will-be)
     (example-cmds-or-nil ?exec-is ?exec-will-be ?cmds))

(<-- (example-cmds-or-nil ?exec-is ?exec-will-be ?cmds)
     (command ?exec-is ?exec-will-be ?cmds) !)

(<- (example-cmds-or-nil ?exec-is ?exec-will-be ()))

(<-- (example-exec-is ?exec-is)
     (example-exec-is "msv" ?exec-is))

(<-- (example-exec-is ?file ?exec-is)
     (lisp ?uri-is
           (use pathname-to-uri
                (use make-pathname :device #-unix "D" #+unix nil
                     :directory '(:absolute "Clearcase"
                                  "dkick1_m19.3.0_dev-910878" "mp" "mmsc"
                                  "processes" "msv")
                     :name ?file)))
     (lisp ?host-is (uri-host ?uri-is))
     (lisp ?path-is (uri-parsed-path ?uri-is))
     (= ?exec-is (exec (uri ?host-is ?path-is) nil)))

(<- (example-exec-is ?file ?exec-is)
    (member ?volume ("$appl1" "$data1"))
    (example-exec-is-on-snet4 ?file ?volume ?exec-is))

(<-- (example-exec-is-on-snet4 ?file ?volume ?exec-is)
     (example-exec snet4 (:absolute ?volume "rdkexe" ?file)
                   (info (enscribe nil)) ?exec-is))

(<- (example-exec-is-on-snet4 ?file ?volume ?exec-is)
    (example-exec snet4 (:absolute ?volume "rdkexe" ?file)
                  (info (enscribe valid-sql-program)) ?exec-is))

(<-- (example-exec-will-be ?file ?exec-will-be)
     (lispp (not (member ?file *execs* :test #'string=)))
     (example-exec snet4 (:absolute "$appl1" "mmexec" ?file)
                   (info (enscribe nil)) ?exec-will-be))

(<- (example-exec-will-be ?file ?exec-will-be)
    (example-exec snet4 (:absolute "$appl1" "mmexec" ?file)
                  (info (enscribe valid-sql-program)) ?exec-will-be))

(<-- (example-exec ?host ?path ?info ?exec)
     (well-known-server-net ?host)
     (= ?exec (exec (uri ?host ?path) ?info)))

(<-- (appl1-mmexec-msv (exec (uri snet4 (:absolute "$appl1" "mmexec" "msv"))
                             (info (enscribe valid-sql-program)))))

(<-- (appl1-rdkexe-msv (exec (uri snet4
                                  (:absolute "$appl1" "rdkexe" "msv"))
                             (info (enscribe valid-sql-program)))))
