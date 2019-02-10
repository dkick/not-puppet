(in-package :com.github.dkick.mmsc-util.user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :prolog))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member (find-package :prolog)
                  (package-use-list :com.github.dkick.mmsc-util.user))
    (use-package :prolog)))

(<-- (vob-exec-directory ?exec ?cmd)
     (vob-exec-directory ?exec nil ?cmd))
(<-- (vob-exec-directory ?exec ?cmd ?cmd)
     (lispp (duff-vob-exec-directory ?exec)))
(<- (vob-exec-directory ?exec ?cmd ?cmd1)
    ;; Fix: Define a type of condition and provide restarts: a retry
    ;; (after the user has built it) and perhaps a use-value.
    (lisp (error "Must build ~A" ?exec)))

(<-- (test-exec-directory ?exec ?cmd)
     (test-exec-directory ?exec nil ?cmd))
(<-- (test-exec-directory ?exec ?cmd ?cmd)
     (lispp (duff-test-exec-directory ?exec)))
(<- (test-exec-directory ?exec ?cmd ?cmd1)
    (vob-exec-directory ?exec ?cmd ?cmd2)
    (is ?cmd1
        (cons #'(lambda (term)
                  (let ((name (last1 (uri-parsed-path (exec-uri ?exec)))))
                    (cmd-put-exec term (exec-directory-pathname name)
                                  name)))
              ?cmd2)))

(<-- (mms-exec-directory ?exec ?cmd)
     (mms-exec-directory ?exec nil ?cmd))
(<-- (mms-exec-directory ?exec ?cmd ?cmd)
     (lispp (duff-mms-exec-directory ?exec)))
;; Fix: Need to introduce rules for backups... i.e. can not move from
;; test-exec-directory until we've also made a backup.
(<- (mms-exec-directory ?exec ?cmd ?cmd1)
    (test-exec-directory ?exec ?cmd ?cmd2)
    (is ?cmd1
        (cons #'(lambda (tacl)
                  (use cmd-move-exec-to-mmexec tacl
                       (last1 (uri-parsed-path (exec-uri ?exec)))))
              ?cmd2)))

(<-- (sql-compile-exec ?exec ?cmd)
     (lisp (error "TBD")))

(<-- (sql-volume ?cmd)
     ;; Does not depend on the ?exec
     (lisp (error "TBD")))

(defun duff-cmds (exec)
  (prolog (lisp ?exec exec)
          (mms-exec-directory ?exec ?cmds)
          (lisp (return-from prolog (reverse ?cmds)))))
