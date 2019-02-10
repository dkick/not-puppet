(in-package :com.github.dkick.mmsc-util.user)

;;; Phake lisp code to be replaced with terminal commands at a later
;;; date.

(defparameter *phake-world* (make-phake-world))

(defgeneric phake-file-exists (host path))

(defmethod phake-file-exists ((host symbol) (path list))
  (gethash path (phake-snet-paths (phake-snet *phake-world* host))))

(define-prolog-struct exec ()
  ((?uri :selector exec-uri) (?info :selector exec-info)))

(define-prolog-struct uri ()
  ((?scheme :selector uri-scheme) (?host :selector uri-host)
   (?port :selector uri-port) (?path :selector uri-path)
   (?query :selector uri-query) (?fragment :selector uri-fragment)
   (?plist :selector uri-plist)))

(<-- (exec-uri-host (exec ?uri ?) ?host)
     (uri-host ?uri ?host))
(<-- (exec-uri-path (exec ?uri ?) ?path)
     (uri-path ?uri ?path))
(<-- (let-exec-uri-path (exec (uri ?scheme ?host ?port ?
                                   ?query ?fragment ?plist)
                              ?info)
                        ?path ?exec/1)
     (= ?exec/1
        (exec (uri ?scheme ?host ?port ?path ?query ?fragment ?plist)
              ?info)))
(<-- (let-uri-host-path (uri ?scheme ? ?port ?
                             ?query ?fragment ?plist)
                        ?host ?path ?uri/1)
     (= ?uri/1 (uri ?scheme ?host ?port ?path ?query ?fragment ?plist)))

;;; To-do: setup will figure out all of the commands achieve the
;;; desired setup.

(<-- (setup ?vob ?execs ?execs ()))
(<- (setup ?vob
           (?exec-now . ?rest-execs-now)
           (?exec-then . ?rest-exec-then)
           ?to-do)
    (command ?exec-now ?exec-then ?to-do-exec)
    (lisp ?to-do (cons ?to-do-exec ?to-do)))

;;; Commands

(<-- (command ?exec-is ?exec-will-be ?cmds)
     (command ?exec-is ?exec-will-be ?exec-is () ?cmds/1)
     (lisp ?cmds (reverse ?cmds/1)))

(<-- (command ?exec-was ?exec-is ?exec-is ?cmds ?cmds))

;; ftp
(<- (command ?exec-was ?exec-will-be ?exec-is ?cmds-was ?cmds-is)
    (= ?exec-is (exec ?uri-is ?))
    (diff-snet-host ?exec-is ?exec-will-be ?host-will-be)
    (ftp-to-path ?exec-will-be ?path-is/1)
    (let-uri-host-path ?uri-is ?host-will-be ?path-is/1 ?uri-is/1)
    (= ?exec-is/1 (exec ?uri-is/1 (info (enscribe nil))))
    (command ?exec-was ?exec-will-be ?exec-is/1
             ((ftp ?exec-is ?exec-is/1) . ?cmds-was) ?cmds-is))

;; sql-compile
(<- (command ?exec-was ?exec-will-be ?exec-is ?cmds-was ?cmds-is)
    (same-snet-hosts ?exec-will-be ?exec-is)
    (same-snet-volumes ?exec-will-be ?exec-is)
    (= ?exec-is (exec ?uri-is (info (enscribe nil))))
    (= ?exec-is/1 (exec ?uri-is (info (enscribe valid-sql-program))))
    (command ?exec-was ?exec-will-be ?exec-is/1
             ((sql-compile ?exec-is ?exec-is/1) . ?cmds-was) ?cmds-is))

;; move
(<- (command ?exec-was ?exec-will-be ?exec-is ?cmds-was ?cmds-is)
    (same-snet-hosts ?exec-will-be ?exec-is)
    (diff-new-path ?exec-will-be ?exec-is ?path-will-be)
    (exec-info ?exec-is ?info-is)
    (move-to-path ?exec-will-be ?info-is)
    (let-exec-uri-path ?exec-is ?path-will-be ?exec-is/1)
    (command ?exec-was ?exec-will-be ?exec-is/1
             ((move ?exec-is ?exec-is/1) . ?cmds-was) ?cmds-is))

;;; Command helper functors

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

(<-- (diff-new-path ?exec/1 ?exec/2 ?new-path)
     (exec-uri-path ?exec/1 ?new-path)
     (exec-uri-path ?exec/2 ?old-path)
     (lispp (not (equal ?new-path ?old-path))))

(<-- (well-known-server-net ?host)
     (member ?host (snet1 snet3 snet4 snet6 snet7 snet8 snet9
                    juliet shrek fiona)))

(<-- (ftp-to-path (exec (uri ? ?host ? ?path ? ? ?) ?)
                  ?path/1)
     (well-known-server-net ?host)
     (new-file ?host ?path)
     (= ?path (:absolute "$appl1" "mmexec" ?exec-name))
     (= ?path/1 (:absolute "$appl1" "rdkexe" ?exec-name)))

(<- (ftp-to-path (exec (uri ? ?host ? ?path ? ? ?) ?)
                 ?path)
    (well-known-server-net ?host)
    (new-file ?host ?path)
    (= ?path (:absolute ?volume ?sub-volume ?exec-name))
    (lispp (or (string/= ?volume "$appl1")
               (string/= ?sub-volume "mmexec"))))

(<-- (move-to-path (exec (uri ? ?host ? ?path ? ? ?) ?)
                   (info (enscribe valid-sql-program)))
     (well-known-server-net ?host)
     (new-file ?host ?path)
     (= ?path (:absolute "$appl1" "mmexec" ?exec-name)))

(<- (move-to-path (exec (uri ? ?host ? ?path ? ? ?) ?)
                  (info ?))
    (well-known-server-net ?host)
    (new-file ?host ?path)
    (= ?path (:absolute ?volume ?sub-volume ?exec-name))
    (lispp (or (string/= ?volume "$appl1")
               (string/= ?sub-volume "mmexec"))))

(<-- (new-file (exec ?uri ?))
     (uri-host ?uri ?host)
     (uri-path ?uri ?path)
     (new-file ?host ?path))

(<-- (new-file ?host ?path)
     (lispp (not (phake-file-exists ?host ?path))))

(<-- (file-exists (exec ?uri ?))
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

